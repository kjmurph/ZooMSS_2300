# Enhanced Logging Utilities for ZooMSS_2300 Project

#' Setup comprehensive logging system
#' @param log_dir Directory for log files
#' @param log_level Logging level (DEBUG, INFO, WARN, ERROR)
#' @param script_name Name of calling script for log identification
setup_logging <- function(log_dir = "logs", log_level = "INFO", script_name = "ZooMSS_Analysis") {
  
  # Create logs directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  # Setup log file with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0(script_name, "_", timestamp, ".log"))
  
  # Configure logger
  library(logger)
  log_threshold(log_level)
  log_appender(appender_tee(log_file))
  
  # Log session start
  log_info("=== {script_name} Session Started ===")
  log_info("Timestamp: {Sys.time()}")
  log_info("R Version: {R.version.string}")
  log_info("Working Directory: {getwd()}")
  log_info("Log File: {log_file}")
  
  return(log_file)
}

#' Log memory usage information
#' @param stage Processing stage description
log_memory_usage <- function(stage = "Current") {
  gc_info <- gc()
  memory_mb <- sum(gc_info[, 2])
  
  log_info("Memory Usage - {stage}: {round(memory_mb, 1)} MB")
  
  # Warn if memory usage is high
  if (memory_mb > 1000) {
    log_warn("High memory usage detected: {round(memory_mb, 1)} MB")
  }
  
  return(memory_mb)
}

#' Log processing checkpoint with timing
#' @param checkpoint_name Name of the checkpoint
#' @param start_time Start time for duration calculation
#' @param additional_info Additional information to log
log_checkpoint <- function(checkpoint_name, start_time = NULL, additional_info = NULL) {
  
  if (!is.null(start_time)) {
    duration <- difftime(Sys.time(), start_time, units = "mins")
    log_info("CHECKPOINT: {checkpoint_name} - Duration: {round(duration, 2)} minutes")
  } else {
    log_info("CHECKPOINT: {checkpoint_name}")
  }
  
  if (!is.null(additional_info)) {
    log_info("  Details: {additional_info}")
  }
  
  # Log memory usage at checkpoint
  log_memory_usage(checkpoint_name)
  
  return(Sys.time())
}

#' Log data processing summary
#' @param data_summary List containing data summary information
log_data_summary <- function(data_summary) {
  log_info("=== DATA PROCESSING SUMMARY ===")
  
  for (name in names(data_summary)) {
    value <- data_summary[[name]]
    log_info("{name}: {value}")
  }
  
  log_info("================================")
}

#' Log file processing information
#' @param filename Name of file being processed
#' @param file_size File size in MB
#' @param processing_stage Current processing stage
log_file_processing <- function(filename, file_size = NULL, processing_stage = "Processing") {
  
  if (!is.null(file_size)) {
    log_info("{processing_stage}: {basename(filename)} ({round(file_size/1e6, 1)} MB)")
  } else {
    # Calculate file size if not provided
    if (file.exists(filename)) {
      file_size <- file.info(filename)$size / 1e6
      log_info("{processing_stage}: {basename(filename)} ({round(file_size, 1)} MB)")
    } else {
      log_info("{processing_stage}: {basename(filename)}")
    }
  }
}

#' Log validation results
#' @param validation_name Name of validation test
#' @param result Validation result (TRUE/FALSE)
#' @param details Additional validation details
log_validation <- function(validation_name, result, details = NULL) {
  
  status <- if (result) "PASSED" else "FAILED"
  level <- if (result) "info" else "error"
  
  log_level(level, "VALIDATION: {validation_name} - {status}")
  
  if (!is.null(details)) {
    log_level(level, "  Details: {details}")
  }
  
  return(result)
}

#' Log error with context
#' @param error_msg Error message
#' @param context Additional context information
#' @param stop_execution Whether to stop execution
log_error_context <- function(error_msg, context = NULL, stop_execution = FALSE) {
  
  log_error("ERROR: {error_msg}")
  
  if (!is.null(context)) {
    log_error("Context: {context}")
  }
  
  # Log current environment info
  log_error("Current working directory: {getwd()}")
  log_error("Available memory: {round(sum(gc()[,2]), 1)} MB")
  
  if (stop_execution) {
    log_error("Stopping execution due to critical error")
    stop(error_msg)
  }
}

#' Log progress for long-running operations
#' @param current Current iteration
#' @param total Total iterations
#' @param operation_name Name of operation
#' @param start_time Start time for ETA calculation
log_progress <- function(current, total, operation_name = "Processing", start_time = NULL) {
  
  percentage <- round((current / total) * 100, 1)
  
  if (!is.null(start_time) && current > 1) {
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    eta <- elapsed * (total - current) / (current - 1)
    log_info("PROGRESS: {operation_name} - {current}/{total} ({percentage}%) - ETA: {round(eta, 1)} min")
  } else {
    log_info("PROGRESS: {operation_name} - {current}/{total} ({percentage}%)")
  }
}

#' Create processing summary report
#' @param log_file Path to log file
#' @param output_dir Directory for summary report
create_processing_summary <- function(log_file, output_dir = "logs") {
  
  if (!file.exists(log_file)) {
    log_warn("Log file not found: {log_file}")
    return(NULL)
  }
  
  # Read log file
  log_lines <- readLines(log_file)
  
  # Extract key information
  checkpoints <- grep("CHECKPOINT:", log_lines, value = TRUE)
  validations <- grep("VALIDATION:", log_lines, value = TRUE)
  errors <- grep("ERROR:", log_lines, value = TRUE)
  warnings <- grep("WARN:", log_lines, value = TRUE)
  
  # Create summary
  summary_file <- file.path(output_dir, paste0("processing_summary_", 
                                               format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  
  writeLines(c(
    "=== ZooMSS_2300 Processing Summary ===",
    paste("Generated:", Sys.time()),
    paste("Log File:", log_file),
    "",
    paste("Total Log Lines:", length(log_lines)),
    paste("Checkpoints:", length(checkpoints)),
    paste("Validations:", length(validations)),
    paste("Warnings:", length(warnings)),
    paste("Errors:", length(errors)),
    "",
    "=== Key Checkpoints ===",
    checkpoints,
    "",
    "=== Validation Results ===",
    validations,
    "",
    if (length(warnings) > 0) c("=== Warnings ===", warnings, ""),
    if (length(errors) > 0) c("=== Errors ===", errors, ""),
    "=== End Summary ==="
  ), summary_file)
  
  log_info("Processing summary saved to: {summary_file}")
  return(summary_file)
}
