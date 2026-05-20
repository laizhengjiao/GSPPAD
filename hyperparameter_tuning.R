library(optRF)
library(tuneRanger)
library(mlr)
library(ranger)
library(ggplot2)
library(dplyr)

# 1. Optimal Number of Trees
set.seed(123)
opt_trees <- opt_prediction(
  y = y_train,
  X = x_train_vif,
  num.trees_values = seq(100, 3000, by = 100),
  number_repetitions = 20,
  visualisation = "prediction",
  recommendation = "prediction"
)
summary(opt_trees)

# 2. Hyperparameter Tuning with tuneRanger
model_train <- as.data.frame(cbind(pa = y_train, x_train_vif))
task <- makeRegrTask(data = model_train, target = "pa")

set.seed(123)
res <- tuneRanger(
  task, 
  iters = 500, 
  iters.warmup = 200, 
  num.trees = 1000, 
  num.threads = 20
)

cat("Optimal mtry:", res$model$mtry, "\n")
cat("Optimal min.node.size:", res$model$min.node.size, "\n")

# 3. Variable Importance Plot
imp <- data.frame(
  var = names(rf_model$variable.importance),
  imp = rf_model$variable.importance
) %>% arrange(desc(imp))

p_imp <- ggplot(imp, aes(x = reorder(var, imp), y = imp)) +
  geom_col(fill = "#20B2AA", width = 0.7) +
  coord_flip() +
  labs(x = "Variables", y = "Permutation Importance") +
  theme_bw() +
  theme(
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_line(color = "gray90")
  )

ggsave("/home/vms240210/GlobalPoaceae/rf_variable_importance.tif", p_imp, dpi = 900, width = 8, height = 10)

# 4. Predicted vs Observed Plot
test_plot <- data.frame(
  obs = model_test$pa,
  pred = test_pred
)

p_val <- ggplot(test_plot, aes(x = obs, y = pred)) +
  geom_point(alpha = 0.5, color = "#E63946") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Observed Pollen Abundance (%)", y = "Predicted Pollen Abundance (%)") +
  theme_bw() +
  theme(
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

ggsave("/home/vms240210/GlobalPoaceae/rf_obs_vs_pred.tif", p_val, dpi = 600, width = 10, height = 7)