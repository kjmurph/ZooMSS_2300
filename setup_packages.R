# ==============================================================================
# ZooMSS_2300 Package Installation and Validation
# ==============================================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Check and install required packages
required_packages <- c("tidyverse", "raster", "ncdf4", "viridis", "scales", 
                      "maps", "config", "logger")

cat("Checking and installing required packages...\n")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
    
    # Verify installation
    if(require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "installed successfully\n")
    } else {
      cat("✗", pkg, "installation failed\n")
    }
  } else {
    cat("✓", pkg, "already available\n")
  }
}

cat("\nPackage installation complete!\n")

# Test basic functionality
cat("\nTesting basic functionality...\n")

# Test config
if(require(config, quietly = TRUE)) {
  cat("✓ Config package working\n")
} else {
  cat("✗ Config package not working\n")
}

# Test logger
if(require(logger, quietly = TRUE)) {
  log_info("Logger package working")
  cat("✓ Logger package working\n")
} else {
  cat("✗ Logger package not working\n")
}

cat("\nSetup validation complete!\n")
