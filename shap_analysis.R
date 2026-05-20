library(ranger)
library(fastshap)
library(tidyverse)
library(shapviz)
library(patchwork)

# 1. Final Model Training
set.seed(123)
final_rf <- ranger(
  pa ~ .,
  data = model_train,
  num.trees = 1000,
  mtry = 4,
  min.node.size = 11,
  sample.fraction = 0.823,
  importance = "permutation",
  seed = 123
)

# 2. SHAP Computation
X_test <- model_test %>% select(-pa)

pred_fun <- function(model, newdata) {
  predict(model, data = newdata)$predictions
}

set.seed(123)
shap_values <- explain(
  final_rf,
  X = X_test,
  pred_wrapper = pred_fun,
  nsim = 500,
  adjust = TRUE
)

shap_obj <- shapviz(shap_values, X = X_test)

# 3. Figure A: SHAP Beeswarm Plot
p_a <- sv_importance(shap_obj, kind = "beeswarm", max_display = 19) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", color = "black", size = 14),
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = c(0.87, 0.5),
    plot.tag = element_text(size = 18, face = "bold")
  ) +
  labs(tag = "A") +
  guides(color = guide_colorbar(
    title = "Feature Value",
    barwidth = 0.8,
    barheight = 10,
    title.position = "left",
    title.theme = element_text(family = "Arial", angle = 90, hjust = 0.5, face = "bold")
  ))

# 4. Figure B: Mean SHAP Importance (Categorized)
imp_df <- sv_importance(shap_obj, max_display = 19, sort = TRUE)$data
colnames(imp_df) <- c("Variable", "Mean_Importance")

imp_df <- imp_df %>%
  mutate(
    Type = case_when(
      Variable %in% c("B1","B2","B3","B8") ~ "Temperature",
      Variable %in% c("B12","B14","B15","B19") ~ "Precipitation",
      Variable == "SR" ~ "Diversity",
      Variable %in% c("soc","bdod","pH","nitrogen","clay","ocs","cec","silt","cfvo") ~ "Soil",
      Variable == "ELE" ~ "Elevation",
      TRUE ~ "Other"
    )
  )

type_colors <- c(
  "Temperature"   = "#AEEEEE",
  "Precipitation" = "#4EEE94",
  "Diversity"     = "#B3EE3A",
  "Soil"          = "#FFDAB9",
  "Elevation"     = "#EEB4B4"
)

p_b <- ggplot(imp_df, aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance, fill = Type)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = type_colors) +
  labs(x = NULL, y = "Mean(|SHAP value|)", fill = "Variable Type", tag = "B") +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    axis.text = element_text(color = "black", size = 14),
    axis.title.x = element_text(face = "bold", size = 14),
    legend.position = c(0.73, 0.5),
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_blank(),
    plot.tag = element_text(size = 18, face = "bold")
  )

# 5. Combine and Export
combined_plot <- p_a + p_b + plot_layout(nrow = 1, widths = c(2, 1.5))

ggsave(
  filename = "/home/vms240210/GlobalPoaceae/SHAP_combined.tif",
  plot = combined_plot,
  device = "tiff",
  dpi = 900,
  width = 25,
  height = 18,
  units = "cm",
  compression = "lzw"
)

print(combined_plot)