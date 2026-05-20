library(openxlsx)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Data Loading and Preprocessing
file_path <- "~/GlobalPoaceae/alldata_filled.xlsx"
pa_data <- read.xlsx(file_path)

set.seed(123)
pa_data <- pa_data[sample(nrow(pa_data)), ]
colnames(pa_data)[1:4] <- c("lon", "lat", "pa", "ref")
pa_data$row_id <- 1:nrow(pa_data)

# 2. Spatial Grid Stratified Sampling
pa_data$lon_bin <- floor(pa_data$lon / 10) * 10
pa_data$lat_bin <- floor(pa_data$lat / 10) * 10
pa_data$grid_id <- paste(pa_data$lon_bin, pa_data$lat_bin, sep = "_")

set.seed(123)
test_indices <- pa_data %>%
  group_by(grid_id) %>%
  group_modify(~ {
    n <- nrow(.x)
    if (n <= 10) {
      .x %>% slice_sample(n = 1)
    } else {
      k <- min(ceiling(n * 0.1), ceiling(n * 0.3))
      .x %>% slice_sample(n = k)
    }
  }) %>%
  pull(row_id)

cols_to_remove <- c("lon_bin", "lat_bin", "grid_id", "row_id")
test_set  <- pa_data[pa_data$row_id %in% test_indices, !(names(pa_data) %in% cols_to_remove)]
train_set <- pa_data[!pa_data$row_id %in% test_indices, !(names(pa_data) %in% cols_to_remove)]

stopifnot(nrow(train_set) + nrow(test_set) == nrow(pa_data))

# 3. Spatial Distribution Map
world <- ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray70") +
  geom_point(data = train_set, aes(x = lon, y = lat, color = "Training set"), alpha = 0.4, size = 1.5) +
  geom_point(data = test_set, aes(x = lon, y = lat, color = "Testing set"), alpha = 0.8, size = 1.5) +
  scale_color_manual(values = c("Training set" = "blue", "Testing set" = "red"), name = NULL) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE, datum = NA) +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 10)) +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.5),
    legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.3),
    legend.text = element_text(size = 12),
    plot.margin = unit(c(2, 2, 2, 2), "mm")
  )

ggsave("/home/vms240210/GlobalPoaceae/train_test_map.tif", p1, dpi = 900, width = 16, height = 8)

# 4. PA Value Distribution Histogram
train_set$group <- "Training set"
test_set$group  <- "Testing set"
combined <- rbind(train_set, test_set)
combined$group <- factor(combined$group, levels = c("Training set", "Testing set"))

p2 <- ggplot(combined, aes(x = pa, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  scale_fill_manual(values = c("Training set" = "blue", "Testing set" = "red"), name = NULL) +
  labs(x = "Pollen Abundance (%)", y = "Count") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(size = 15),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = c(0.85, 0.85)
  )

ggsave("/home/vms240210/GlobalPoaceae/train_test_count.tif", p2, dpi = 900, width = 10, height = 6, bg = "white")

# 5. Summary Statistics
cat("Train Ratio:", round(nrow(train_set)/nrow(pa_data), 4), "\n")
cat("Test Ratio:", round(nrow(test_set)/nrow(pa_data), 4), "\n")