# Load and plot the ESM model output to look at combinations of SST v Chl
# Adapted from Jason Everett' (UQ)'s code in ZooMSS_Climate_Change github repo
# 13th May 2025

library(tidyverse)
library(tidync)
library(raster)

base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/R Projects/ZooMSS_2300/Input/"

# Define the models and experiments we expect to find
ModelArray <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
ExpArray <- c("historical","picontrol", "ssp126", "ssp534-over", "ssp585")

for (m in 1:length(ModelArray)) {
  for (e in 1:length(ExpArray)) {
    # Change pattern to match the actual file format
    # Notice we're using a more flexible pattern with fixed=FALSE
    ftos <- list.files(paste0(base_dir, "tos"),
                       pattern = paste0(ModelArray[m], ".*", ExpArray[e]),
                       full.names = TRUE)

    fchl <- list.files(paste0(base_dir, "chl"),
                       pattern = paste0(ModelArray[m], ".*", ExpArray[e]),
                       full.names = TRUE)

    # Add error checking
    if(length(ftos) == 0) {
      warning(paste("No tos files found for", ModelArray[m], ExpArray[e]))
      next
    }
    if(length(fchl) == 0) {
      warning(paste("No chl files found for", ModelArray[m], ExpArray[e]))
      next
    }

    # Rest of your code...
    ttos <- as.data.frame(stack(ftos), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "SST")

    tchl <- as.data.frame(stack(fchl), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "Chl") %>%
      dplyr::select(Chl)

    temp <- bind_cols(ttos, tchl) %>%
      add_column(Model = ModelArray[m],
                 Experiment = ExpArray[e]) %>%
      mutate(SST = round(SST, digits = 1),
             Chl_log10 = log10(Chl),
             Chl_log10 = round(Chl_log10, digits = 2))
    # %>%
    #   distinct(SST, Chl_log10, .keep_all = TRUE)

    if (m == 1 & e == 1){
      df <- temp
    } else {
      df <- bind_rows(df, temp)
      # %>%
      #   distinct(SST, Chl_log10, .keep_all = TRUE) # Only keep saving the
    }
    rm(temp, ttos, tchl)
  }
}

df <- df %>%
  rename("Lon" = x, "Lat" = y)

## Do the plotting for all the data

p <- ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw()
# ggsave("Figures/SSTChl_All.png", p, width = 12, height = 8, dpi = 300)


# p <- ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
#   geom_hex() +
#   scale_fill_continuous(type = "viridis", trans = "log10") +
#   theme_bw() +
#   facet_wrap(facets = "Model", scales = "fixed")
# ggsave("Figures/SSTChl_ModelFacet.png", p, width = 12, height = 8, dpi = 300)


## Now check the distinct rows
ds <- df %>%
  distinct(SST, Chl_log10, .keep_all = TRUE)

p <- ggplot(data = ds, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw()
# ggsave("Figures/SSTChl_Distinct.png", p, width = 12, height = 8, dpi = 300)


# Now save environmental data space
enviro_data <- ds %>%
  mutate(Chl = 10^Chl_log10) %>%
  dplyr::select(c(SST, Chl, Chl_log10)) %>%
  arrange(desc(Chl), desc(SST)) %>%
  filter(is.na(Chl)==FALSE) %>%
  filter(is.na(SST)==FALSE) %>%
  rename(chlo = Chl, sst = SST)


df <- df %>%
  dplyr::select(c(Lon, Lat, Date, SST, Chl, Model, Experiment, Chl_log10))

# saveRDS(enviro_data, "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/R Projects/ZooMSS_2300/Output/enviro_matrix_2300_distinct.RDS")
# saveRDS(df, "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/R Projects/ZooMSS_2300/Output/enviro_matrix_2300_all_DateIncorrect.RDS")
















