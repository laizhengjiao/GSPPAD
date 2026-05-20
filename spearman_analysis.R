library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

file_path <- "/home/vms240210/GlobalPoaceae/alldata_filled.xlsx"
df <- read_excel(file_path)

# Dynamically select Bio variables (columns 6 to end)
bio_vars <- colnames(df)[6:ncol(df)]

df <- df %>%
  mutate(
    climate_zone = case_when(
      abs(Latitude) >= 50 ~ "Cold",
      abs(Latitude) >= 23.5 & abs(Latitude) < 50 ~ "Temperate",
      abs(Latitude) < 23.5 ~ "Tropical"
    )
  )

calc_cor <- function(data) {
  res <- data.frame(
    Variable = bio_vars,
    Correlation = sapply(bio_vars, function(v) {
      cor(data$PA, data[[v]], method = "spearman", use = "complete.obs")
    })
  )
  
  # Sorting: Positive high to low, then Negative low to high
  res <- res %>%
    mutate(Sign = ifelse(Correlation >= 0, "Positive", "Negative")) %>%
    arrange(desc(Correlation))
  
  res$Variable <- factor(res$Variable, levels = rev(res$Variable))
  return(res)
}

plot_fun <- function(data) {
  ggplot(data, aes(x = Correlation, y = Variable, fill = Sign)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      values = c("Positive" = "#1f9ebc", "Negative" = "#e64b35"),
      guide = "none"
    ) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    labs(x = "Spearman Correlation Coefficient", y = NULL) +
    theme_bw() +
    theme(
      axis.text.y = element_text(color = "black"),
      axis.text.x = element_text(color = "black"),
      panel.background = element_rect(fill = "white", color = "black"),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# Process Zones
cor_cold <- calc_cor(filter(df, climate_zone == "Cold"))
cor_temp <- calc_cor(filter(df, climate_zone == "Temperate"))
cor_trop <- calc_cor(filter(df, climate_zone == "Tropical"))

# Create Individual Plots
p1 <- plot_fun(cor_cold)
p2 <- plot_fun(cor_temp)
p3 <- plot_fun(cor_trop)

# Combine with Patchwork and add tags (A, B, C)
final_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = "bold", size = 16))

# Save Result
ggsave(
  filename = "/home/vms240210/GlobalPoaceae/PA_spearman_climate_zones.png",
  plot = final_plot,
  width = 16,
  height = 8,
  dpi = 300
)

print(final_plot)