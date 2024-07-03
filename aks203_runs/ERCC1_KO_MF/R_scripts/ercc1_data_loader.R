###### check data loading from SLIDE function as well ######
# path to R libraries (installed)
.libPaths("/ix/djishnu/Akanksha/R_libs")

# load necessary libraries
library(yaml)
library(log4r)
library(SLIDE)
library(ggplot2)

# set logger
logger <- logger(threshold = "INFO", appenders = list(file_appender(file = "script.log")))

# Function to log and stop on error
log_and_stop <- function(message) {
    error(logger, message)
    stop(message)
}

# Start logging
info(logger, "Script started.")

# Set output directory
output_dir <- "/ix/djishnu/Akanksha/SLIDE_git/runs/ERCC1_KO_MF/processed_data"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
info(logger, paste("Output directory set to:", output_dir))

# Set variables
tryCatch(
    {
        raw_x <- as.matrix(read.csv("/ix/djishnu/Akanksha/SLIDE_git/examples/Ssc_x.csv", row.names = 1))
        raw_y <- as.matrix(read.csv("/ix/djishnu/Akanksha/SLIDE_git/examples/SkinScore.csv", row.names = 1))
        info(logger, "Raw data loaded successfully.")
    },
    error = function(e) {
        log_and_stop(paste("Failed to load data:", e$message))
    }
)

# Replace NANs with 0 for calculations
tryCatch(
    {
        missing_values <- which(is.na(raw_x) | is.nan(raw_x) | is.infinite(raw_x))
        raw_x[missing_values] <- 0
        numeric_x <- raw_x
        info(logger, "Missing values replaced with 0.")
    },
    error = function(e) {
        log_and_stop(paste("Failed to replace missing values:", e$message))
    }
)

# Filter feature columns for sparsity/zero value proportion being > 0.9
tryCatch(
    {
        # get proportion of zero values in each column
        ratio_zero_values <- apply(numeric_x, MARGIN = 2, function(x) length(which(x == 0)) / length(x))
        hist(ratio_zero_values, xlab = "Ratio of zeroes in each feature/col")
        ggsave(filename = file.path(output_dir, "feature_zero_ratio_hist.png"))
        # index to keep columns based on a threshold
        keep_columns <- ratio_zero_values <= 0.9
        # filter based on index
        non_sparse_x <- numeric_x[, keep_columns]
        info(logger, "Filtered sparse features.")
    },
    error = function(e) {
        log_and_stop(paste("Failed to filter sparse features:", e$message))
    }
)

# Drop sd = 0 feature columns
tryCatch(
    {
        # Logical indexing for columns to keep (standard deviation != 0)
        non_zero_sd_features <- apply(non_sparse_x, 2, sd) != 0
        print(non_zero_sd_features) # Debug print

        # Filter columns using logical indexing
        non_constant_x <- non_sparse_x[, non_zero_sd_features]
        info(logger, "Dropped zero standard deviation features.")
    },
    error = function(e) {
        log_and_stop(paste("Failed to drop zero standard deviation features:", e$message))
    }
)

# Filter very low variance feature columns
tryCatch(
    {
        scale_x_by_mean <- apply(non_constant_x, MARGIN = 2, function(x) scale(x, center = TRUE, scale = FALSE))
        col_variance <- apply(scale_x_by_mean, MARGIN = 2, var)
        hist(col_variance)
        ggsave(filename = file.path(output_dir, "col_variance_hist.png"))
        low_var_cols <- which(col_variance < quantile(col_variance, 0.25))
        processed_x <- non_constant_x[, -low_var_cols]
        info(logger, "Filtered low variance features.")
    },
    error = function(e) {
        log_and_stop(paste("Failed to filter low variance features:", e$message))
    }
)

# Save the processed data
tryCatch(
    {
        write.csv(processed_x, file.path(output_dir, "processed_x.csv"))
        info(logger, paste("Processed data saved to:", file.path(output_dir, "processed_x.csv")))
    },
    error = function(e) {
        log_and_stop(paste("Failed to save processed data:", e$message))
    }
)

# End logging
info(logger, "Script completed successfully.")
