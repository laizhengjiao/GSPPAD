library(terra)
library(ranger)

# 1. Path Configuration
train_data_path <- "/home/vms240210/GlobalPoaceae/model_train.rds"
future_root     <- "/home/vms240210/GlobalPoaceae/future_prediction/"
output_root     <- "/home/vms240210/GlobalPoaceae/future_prediction_output/"

train_data <- readRDS(train_data_path)

if (!dir.exists(output_root)) {
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
}

# 2. Global Terra Options
terraOptions(memfrac = 0.8)

# 3. Identify Scenario Subdirectories
scenarios <- list.dirs(future_root, full.names = FALSE, recursive = FALSE)
scenarios <- scenarios[scenarios != ""]

message("Found ", length(scenarios), " scenarios to process: ", paste(scenarios, collapse = ", "))

# 4. Batch Prediction Loop
for (scene in scenarios) {
  
  scene_input  <- file.path(future_root, scene)
  scene_output <- file.path(output_root, scene)
  
  if (!dir.exists(scene_output)) {
    dir.create(scene_output, recursive = TRUE, showWarnings = FALSE)
  }
  
  message("\nProcessing Scenario: ", scene)
  
  # Load and align rasters
  tif_files  <- list.files(scene_input, pattern = "\\.tif$", full.names = TRUE)
  ras        <- rast(tif_files)
  names(ras) <- tools::file_path_sans_ext(basename(tif_files))
  
  predictors <- names(train_data)[names(train_data) != "pa"]
  ras        <- ras[[predictors]]
  
  # Ensemble loop (10 independent runs)
  pred_files <- character(10)
  
  for (i in 1:10) {
    current_seed <- 123 + i
    set.seed(current_seed)
    
    # Train model
    model_i <- ranger(
      pa ~ ., 
      data = train_data,
      num.trees = 1000, 
      mtry = 4,
      min.node.size = 11, 
      sample.fraction = 0.823
    )
    
    outfile <- file.path(scene_output, paste0("pred_", i, "_seed", current_seed, ".tif"))
    
    # Prediction with strict NA exclusion
    predict(
      ras,
      model_i,
      fun = function(model, d, ...) {
        na_rows <- rowSums(is.na(d)) > 0
        result  <- rep(NA_real_, nrow(d))
        if (any(!na_rows)) {
          result[!na_rows] <- predict(model, data = d[!na_rows, , drop = FALSE])$predictions
        }
        return(result)
      },
      filename  = outfile,
      overwrite = TRUE,
      cores     = 1
    )
    
    pred_files[i] <- outfile
    
    # Memory management
    rm(model_i)
    tmpFiles(remove = TRUE)
    gc()
  }
  
  # Aggregate ensemble results
  message("Calculating mean prediction for: ", scene)
  pred_stack <- rast(pred_files)
  
  mean_pred <- app(
    pred_stack,
    fun      = function(x) mean(x, na.rm = FALSE),
    filename = file.path(scene_output, "mean_prediction.tif"),
    overwrite = TRUE
  )
  
  # Clear memory before next scenario
  rm(ras, pred_stack, mean_pred)
  tmpFiles(remove = TRUE)
  gc()
}

message("\nBatch prediction workflow for all scenarios is complete.")