# ==============================================================================
# ZooMSS 2300 - Prepare FishMIP 2300 Protocol Priority 1 Outputs
# ==============================================================================
# Purpose: Transform ZooMSS biomass predictions into FishMIP 2300 protocol format
# Priority 1 scenarios: historical, ssp126, ssp585
# Based on: ZooMSS_FishMIP_P1_2_Experiments.R
# ==============================================================================

library(tidyverse)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

cat("=== FISHMIP 2300 PRIORITY 1 OUTPUT PREPARATION ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Directories
base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
input_dir <- paste0(base_dir, "Output/Biomass_projections/")
output_dir <- paste0(base_dir, "Output/FishMIP_2300_Priority1/")

# Create output directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# ==============================================================================
# FISHMIP 2300 PROTOCOL SPECIFICATIONS
# ==============================================================================

# Priority 1 scenarios (from FishMIP 2300 protocol)
priority1_scenarios <- list(
  historical = list(
    name = "historical",
    years = 1850:2014,
    description = "Historical simulation"
  ),
  ssp126 = list(
    name = "ssp126",
    years = 2015:2300,
    description = "Low emissions scenario"
  ),
  ssp585 = list(
    name = "ssp585",
    years = 2015:2300,
    description = "High emissions scenario"
  )
)

# Models
models <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")

# FishMIP required outputs (from protocol):
# - tcb: TOTAL consumer biomass density (g m-2), all consumers (trophic level >1)
# - tcblog10: Biomass in log10 weight bins (1g, 10g, 100g, 1kg, 10kg, 100kg)
# - tpb: TOTAL pelagic biomass density (g m-2), all pelagic consumers
# - bp30cm: Biomass of small pelagics <30cm (g m-2)
# - bp30to90cm: Biomass of medium pelagics >=30cm and <90cm (g m-2)
# - bp90cm: Biomass of large pelagics >=90cm (g m-2)

# Weight thresholds from length-weight relationship
# Weight (g) = 0.01 * length^3 (length in cm)
weight30 <- 10^round(log10(30^(1/3)*100), 1)  # = 100 g
weight90 <- 10^round(log10(90^(1/3)*100), 1)  # = 1000 g

cat("FishMIP weight thresholds:\n")
cat("  Small pelagics (<30cm):", weight30, "g\n")
cat("  Medium pelagics (30-90cm):", weight30, "to", weight90, "g\n")
cat("  Large pelagics (>=90cm):", weight90, "g\n\n")

# Mixed Layer Depth (for conversion from m-3 to m-2)
MLD <- 60  # meters

# Log10 weight bins for tcblog10
# Equal log10 g C weight bins: 1g, 10g, 100g, 1kg, 10kg, 100kg
w_lim <- 10^c(-1, 0, 1, 2, 3, 4, 5)

cat("Log10 weight bins (g):\n")
for(i in 1:(length(w_lim)-1)) {
  cat("  Bin", i-1, ":", w_lim[i], "to", w_lim[i+1], "g\n")
}
cat("\n")

# ==============================================================================
# ZOOMSS SIZE STRUCTURE MAPPING
# ==============================================================================

# ZooMSS size classes (from model parameters)
# These are approximate based on typical ZooMSS setup
# You may need to adjust based on actual model$param$w from ZooMSS matrix

# For now, we'll use the three size classes we have:
# - pico_biom: smallest consumers (flagellates, small ciliates) ~ <0.1g
# - nano_biom: medium consumers (copepods, larvaceans) ~ 0.1-10g  
# - micro_biom: largest consumers (euphausiids, jellyfish, fish) ~ 10-100g

# Mapping to FishMIP size classes:
# tcblog10_0 (0.1-1g): mostly pico_biom
# tcblog10_1 (1-10g): mix of pico/nano
# tcblog10_2 (10-100g): mix of nano/micro
# tcblog10_3 (100-1000g): mostly micro
# tcblog10_4 (1-10kg): micro (large fish component)
# tcblog10_5 (10-100kg): micro (very large fish)

# Mapping to pelagic size classes:
# bp30cm (<100g): pico + most of nano
# bp30to90cm (100-1000g): part of nano + part of micro
# bp90cm (>1000g): large component of micro

cat("ZooMSS to FishMIP size class mapping:\n")
cat("  pico_biom -> small consumers (<1g), contributes to tcblog10_0, bp30cm\n")
cat("  nano_biom -> medium consumers (1-100g), contributes to tcblog10_1-2, bp30cm-bp30to90cm\n")
cat("  micro_biom -> large consumers (>10g), contributes to tcblog10_2-5, bp30to90cm-bp90cm\n\n")

# ==============================================================================
# PROCESSING FUNCTION
# ==============================================================================

process_fishmip_output <- function(model, scenario, scenario_years) {
  
  filename <- paste0("Biomass_ClimateChange_Compiled_withZooMSS_", model, "_", scenario, "_Control.rds")
  filepath <- file.path(input_dir, filename)
  
  if (!file.exists(filepath)) {
    cat("  WARNING: File not found:", filename, "\n")
    return(NULL)
  }
  
  cat("  Processing:", filename, "\n")
  
  # Load data
  data <- readRDS(filepath)
  
  # Filter to scenario years
  data <- data %>%
    filter(Year %in% scenario_years)
  
  cat("    Loaded", nrow(data), "rows for years", min(data$Year), "to", max(data$Year), "\n")
  
  # ZooMSS biomass is already in mg C/m³
  # Convert to g/m² by multiplying by MLD (60m) and converting mg to g
  # ALL ZooMSS outputs are consumers (trophic level >1)
  
  fishmip_data <- data %>%
    mutate(
      # Convert all species from mg C/m³ to g/m²
      # Multiply by MLD (60m) and divide by 1000 (mg to g)
      
      # Zooplankton groups
      Flagellates_gm2 = Flagellates * MLD / 1000,
      Ciliates_gm2 = Ciliates * MLD / 1000,
      Larvaceans_gm2 = Larvaceans * MLD / 1000,
      OmniCopepods_gm2 = OmniCopepods * MLD / 1000,
      CarnCopepods_gm2 = CarnCopepods * MLD / 1000,
      Euphausiids_gm2 = Euphausiids * MLD / 1000,
      Chaetognaths_gm2 = Chaetognaths * MLD / 1000,
      Salps_gm2 = Salps * MLD / 1000,
      Jellyfish_gm2 = Jellyfish * MLD / 1000,
      
      # Fish size classes
      Fish_Small_gm2 = Fish_Small * MLD / 1000,
      Fish_Med_gm2 = Fish_Med * MLD / 1000,
      Fish_Large_gm2 = Fish_Large * MLD / 1000,
      
      # Total consumer biomass (tcb) = all zooplankton + all fish
      tcb = Flagellates_gm2 + Ciliates_gm2 + Larvaceans_gm2 + 
            OmniCopepods_gm2 + CarnCopepods_gm2 + Euphausiids_gm2 + 
            Chaetognaths_gm2 + Salps_gm2 + Jellyfish_gm2 +
            Fish_Small_gm2 + Fish_Med_gm2 + Fish_Large_gm2,
      
      # Total pelagic biomass (tpb) = same as tcb (all ZooMSS are pelagic)
      tpb = tcb,
      
      # Log10 weight bins based on ZooMSS size structure
      # Weight ranges from ZooMSS typically:
      # Flagellates, Ciliates: 0.1-1g (bin 0)
      # Larvaceans, small copepods: 1-10g (bin 1)  
      # Large copepods, euphausiids: 10-100g (bin 2)
      # Chaetognaths, salps, jellyfish, small fish: 100-1000g (bin 3)
      # Medium fish: 1-10kg (bin 4)
      # Large fish: 10-100kg (bin 5)
      
      tcblog10_0 = Flagellates_gm2 + Ciliates_gm2,  # 0.1-1g
      tcblog10_1 = Larvaceans_gm2 + OmniCopepods_gm2 * 0.5,  # 1-10g
      tcblog10_2 = OmniCopepods_gm2 * 0.5 + CarnCopepods_gm2 + Euphausiids_gm2 + 
                   Fish_Small_gm2 * 0.3,  # 10-100g
      tcblog10_3 = Chaetognaths_gm2 + Salps_gm2 + Jellyfish_gm2 + 
                   Fish_Small_gm2 * 0.7 + Fish_Med_gm2 * 0.3,  # 100-1000g
      tcblog10_4 = Fish_Med_gm2 * 0.7 + Fish_Large_gm2 * 0.3,  # 1-10kg
      tcblog10_5 = Fish_Large_gm2 * 0.7,  # 10-100kg
      
      # Pelagic size classes by length (converted from weight)
      # Small pelagics <30cm (<100g): most zooplankton + small fish
      bp30cm = Flagellates_gm2 + Ciliates_gm2 + Larvaceans_gm2 + 
               OmniCopepods_gm2 + CarnCopepods_gm2 + Euphausiids_gm2 +
               Fish_Small_gm2 * 0.3,
      
      # Medium pelagics 30-90cm (100-1000g): larger zooplankton + med fish
      bp30to90cm = Chaetognaths_gm2 + Salps_gm2 + Jellyfish_gm2 + 
                   Fish_Small_gm2 * 0.7 + Fish_Med_gm2 * 0.5,
      
      # Large pelagics >=90cm (>1000g): large fish
      bp90cm = Fish_Med_gm2 * 0.5 + Fish_Large_gm2
    ) %>%
    # Select only required columns for FishMIP output
    select(
      Lon, Lat, Year, SST, Chl_log10,
      Model, Experiment,
      tcb, tpb,
      tcblog10_0, tcblog10_1, tcblog10_2, tcblog10_3, tcblog10_4, tcblog10_5,
      bp30cm, bp30to90cm, bp90cm,
      # Keep individual species for reference
      Flagellates_gm2, Ciliates_gm2, Larvaceans_gm2, 
      OmniCopepods_gm2, CarnCopepods_gm2, Euphausiids_gm2,
      Chaetognaths_gm2, Salps_gm2, Jellyfish_gm2,
      Fish_Small_gm2, Fish_Med_gm2, Fish_Large_gm2
    ) %>%
    # Rename for FishMIP convention
    rename(
      lon = Lon,
      lat = Lat,
      year = Year,
      sst = SST,
      chl_log10 = Chl_log10,
      model = Model,
      scenario = Experiment
    )
  
  cat("    Created FishMIP format data with", nrow(fishmip_data), "rows\n")
  cat("    Variables:", paste(names(fishmip_data), collapse=", "), "\n")
  
  # Summary statistics
  cat("    Biomass summary (g/m²):\n")
  cat("      TCB: mean =", round(mean(fishmip_data$tcb, na.rm=TRUE), 4), 
      ", range =", round(min(fishmip_data$tcb, na.rm=TRUE), 4), "to", 
      round(max(fishmip_data$tcb, na.rm=TRUE), 4), "\n")
  cat("      bp30cm: mean =", round(mean(fishmip_data$bp30cm, na.rm=TRUE), 4), "\n")
  cat("      bp30to90cm: mean =", round(mean(fishmip_data$bp30to90cm, na.rm=TRUE), 4), "\n")
  cat("      bp90cm: mean =", round(mean(fishmip_data$bp90cm, na.rm=TRUE), 4), "\n")
  
  # Save output
  output_filename <- paste0("zoomss_", model, "_", scenario, "_fishmip2300.rds")
  output_path <- file.path(output_dir, output_filename)
  saveRDS(fishmip_data, output_path)
  cat("    Saved:", output_filename, "\n")
  
  # Also save as CSV for easy inspection (sample only to save space)
  if(nrow(fishmip_data) > 100000) {
    # Sample for CSV if very large
    csv_data <- fishmip_data %>% 
      filter(year %% 10 == 0) %>%  # Every 10th year
      sample_n(min(10000, n()))
    csv_note <- "(sampled)"
  } else {
    csv_data <- fishmip_data
    csv_note <- "(complete)"
  }
  
  csv_filename <- paste0("zoomss_", model, "_", scenario, "_fishmip2300_sample.csv")
  csv_path <- file.path(output_dir, csv_filename)
  write_csv(csv_data, csv_path)
  cat("    Saved CSV sample:", csv_filename, csv_note, "\n\n")
  
  return(fishmip_data)
}

# ==============================================================================
# PROCESS ALL PRIORITY 1 SCENARIOS
# ==============================================================================

cat("\n=== PROCESSING PRIORITY 1 SCENARIOS ===\n\n")

results_list <- list()
results_summary <- data.frame()

for (model in models) {
  cat("Model:", toupper(model), "\n")
  cat(strrep("=", 60), "\n")
  
  for (scenario_name in names(priority1_scenarios)) {
    scenario_info <- priority1_scenarios[[scenario_name]]
    
    cat("\nScenario:", scenario_name, "-", scenario_info$description, "\n")
    cat("Years:", min(scenario_info$years), "to", max(scenario_info$years), "\n")
    
    result <- process_fishmip_output(
      model = model,
      scenario = scenario_name,
      scenario_years = scenario_info$years
    )
    
    if (!is.null(result)) {
      # Store result
      key <- paste(model, scenario_name, sep="_")
      results_list[[key]] <- result
      
      # Add to summary
      summary_row <- data.frame(
        model = model,
        scenario = scenario_name,
        year_min = min(result$year),
        year_max = max(result$year),
        n_years = length(unique(result$year)),
        n_cells = nrow(result) / length(unique(result$year)),
        tcb_mean = mean(result$tcb, na.rm=TRUE),
        tcb_sd = sd(result$tcb, na.rm=TRUE),
        bp30cm_mean = mean(result$bp30cm, na.rm=TRUE),
        bp30to90cm_mean = mean(result$bp30to90cm, na.rm=TRUE),
        bp90cm_mean = mean(result$bp90cm, na.rm=TRUE),
        stringsAsFactors = FALSE
      )
      results_summary <- bind_rows(results_summary, summary_row)
    }
  }
  cat("\n")
}

# ==============================================================================
# SAVE SUMMARY
# ==============================================================================

cat("\n=== SUMMARY OF ALL PROCESSED DATA ===\n")
print(results_summary)

summary_path <- file.path(output_dir, "fishmip2300_priority1_summary.csv")
write_csv(results_summary, summary_path)
cat("\nSummary saved to:", summary_path, "\n")

# Create a combined file for each scenario (all models together)
cat("\n=== CREATING COMBINED MODEL FILES ===\n")

for (scenario_name in names(priority1_scenarios)) {
  cat("\nCombining", scenario_name, "across all models...\n")
  
  scenario_data <- results_list %>%
    keep(~!is.null(.) && .$scenario[1] == scenario_name) %>%
    bind_rows()
  
  if (nrow(scenario_data) > 0) {
    combined_filename <- paste0("zoomss_allmodels_", scenario_name, "_fishmip2300.rds")
    combined_path <- file.path(output_dir, combined_filename)
    saveRDS(scenario_data, combined_path)
    cat("  Saved combined file:", combined_filename, "\n")
    cat("  Total rows:", nrow(scenario_data), "\n")
    cat("  Models:", paste(unique(scenario_data$model), collapse=", "), "\n")
  }
}

# ==============================================================================
# COMPLETION MESSAGE
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("FISHMIP 2300 PRIORITY 1 OUTPUT PREPARATION COMPLETE\n")
cat(strrep("=", 70), "\n")
cat("\nProcessed files:\n")
cat("  Total model-scenario combinations:", nrow(results_summary), "\n")
cat("  Models:", paste(unique(results_summary$model), collapse=", "), "\n")
cat("  Scenarios:", paste(unique(results_summary$scenario), collapse=", "), "\n")
cat("\nOutput directory:", output_dir, "\n")
cat("\nFiles created:\n")
cat("  Individual model-scenario RDS files: ", nrow(results_summary), "\n")
cat("  Combined scenario RDS files:", length(unique(results_summary$scenario)), "\n")
cat("  CSV sample files:", nrow(results_summary), "\n")
cat("  Summary CSV: 1\n")
cat("\nNext steps:\n")
cat("  1. Review the summary CSV to verify biomass ranges\n")
cat("  2. Check sample CSV files for data quality\n")
cat("  3. Validate size class distributions make biological sense\n")
cat("  4. Consider refining the size class allocation proportions\n")
cat("  5. Prepare Priority 2 scenarios if needed (picontrol, ssp534-over)\n")
cat("\n")

cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
