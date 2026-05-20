library(terra)
library(ranger)

# 1. Path Configuration
raster_folder <- "/home/vms240210/GlobalPoaceae/25mdata/"
train_data <- readRDS("/home/vms240210/GlobalPoaceae/model_train.rds")
output_folder <- "/home/vms240210/GlobalPoaceae/current_prediction_10models/"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
}

# 2. Raster Preparation and Alignment
tif_files  <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
ras        <- rast(tif_files)
names(ras) <- tools::file_path_sans_ext(basename(tif_files))

# Ensure predictors match training data exactly
predictors <- names(train_data)[names(train_data) != "pa"]
ras        <- ras[[predictors]]

# Memory optimization for large global rasters
terraOptions(memfrac = 0.8)

# 3. Iterative Ensemble Prediction (10 Models)
pred_files <- character(10)

for (i in 1:10) {
  current_seed <- 123 + i
  set.seed(current_seed)
  
  message("Starting iteration: ", i, " | Seed: ", current_seed)
  
  # Train model for current iteration
  model_i <- ranger(
    pa ~ ., 
    data = train_data,
    num.trees = 1000, 
    mtry = 4,
    min.node.size = 11, 
    sample.fraction = 0.823
  )
  
  outfile <- file.path(output_folder, paste0("pred_", i, "_seed", current_seed, ".tif"))
  
  # Raster prediction with strict NA handling
  predict(
    ras,
    model_i,
    fun = function(model, d, ...) {
      # Identify rows with any missing values
      na_rows <- rowSums(is.na(d)) > 0
      result  <- rep(NA_real_, nrow(d))
      
      # Predict only for complete cases
      if (any(!na_rows)) {
        result[!na_rows] <- predict(
          model,
          data = d[!na_rows, , drop = FALSE]
        )$predictions
      }
      return(result)
    },
    filename  = outfile,
    overwrite = TRUE,
    cores     = 1
  )
  
  pred_files[i] <- outfile
  
  # Explicit memory management
  rm(model_i)
  tmpFiles(remove = TRUE)
  gc()
}

# 4. Calculate Final Ensemble Mean
message("Calculating final ensemble mean...")

pred_stack <- rast(pred_files)

mean_pred <- app(
  pred_stack,
  fun      = function(x) mean(x, na.rm = FALSE),
  filename = file.path(output_folder, "mean_prediction.tif"),
  overwrite = TRUE
)

message("Prediction workflow complete.")
message("Results saved in: ", output_folder)