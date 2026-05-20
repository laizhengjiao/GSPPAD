library(terra)

# 1. Path Configuration
future_dir   <- "/home/vms240210/GlobalPoaceae/future_mean/"
current_file <- "/home/vms240210/GlobalPoaceae/current_prediction_10models/mean_prediction.tif"
output_dir   <- "/home/vms240210/GlobalPoaceae/future_minus_current/"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# 2. Load Current Baseline Raster
current_raster <- rast(current_file)

# 3. Identify Future Scenario Files
future_files <- list.files(future_dir, pattern = "\\.tif$", full.names = TRUE)
message("Found ", length(future_files), " scenario files for processing.")

# 4. Iterative Subtraction Workflow
for (f in future_files) {
  
  file_name <- basename(f)
  output_path <- file.path(output_dir, file_name)
  
  message("Processing change map for: ", file_name)
  
  # Load current future scenario
  future_raster <- rast(f)
  
  # Calculate Delta (Change): Future - Current
  # Positive values indicate an increase; negative values indicate a decrease.
  diff_raster <- future_raster - current_raster
  
  # Export change map
  writeRaster(diff_raster, output_path, overwrite = TRUE)
}

message("Analysis complete. All change maps saved in: ", output_dir)