library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggspatial) 
library(scales)

# --- 1. 路径配置 ---
base_data_path <- "D:/university/GIS/LST/data/"
base_out_path  <- "D:/university/GIS/LST/output/"
years <- c(2000, 2005, 2010, 2015, 2020, 2025)

plot_list <- list()

# --- 2. 预扫描量程 ---
all_values <- c()
for (y in years) {
  tif_file <- paste0(base_data_path, "CQ_ROI_LST_", y, ".tif")
  if (file.exists(tif_file)) {
    r_temp <- raster(tif_file, band = 1)
    v <- suppressWarnings(quantile(r_temp, c(0.01, 0.99), na.rm=TRUE))
    all_values <- c(all_values, v)
  }
}
all_min <- min(all_values, na.rm = TRUE)
all_max <- max(all_values, na.rm = TRUE)

# --- 3. 循环绘图 ---
for (i in 1:length(years)) {
  y <- years[i]
  tif_path <- paste0(base_data_path, "CQ_ROI_LST_", y, ".tif")
  shp_path <- paste0(base_out_path, "Boundary_", y, ".shp")
  
  if (!file.exists(tif_path) || !file.exists(shp_path)) next
  
  lst_r <- raster(tif_path, band = 1)
  lst_df <- as.data.frame(lst_r, xy = TRUE)
  colnames(lst_df) <- c("x", "y", "LST")
  lst_df <- na.omit(lst_df)
  bnd <- st_read(shp_path, quiet = TRUE) %>% st_transform(crs = st_crs(lst_r))
  
  p <- ggplot() +
    geom_raster(data = lst_df, aes(x = x, y = y, fill = LST)) +
    # 核心修改：增加 color 映射用于生成蓝色边界图例
    geom_sf(data = bnd, aes(color = "Urban Area"), fill = NA, size = 1.2, show.legend = TRUE) + 
    scale_fill_viridis(
      option = "magma", limits = c(all_min, all_max), 
      breaks = seq(round(all_min), round(all_max), by = 5),
      name = "LST (°C)",
      guide = guide_colorbar(direction = "horizontal", barwidth = 15, barheight = 0.6, title.position = "top")
    ) +
    # 设置蓝色边界的图例颜色
    scale_color_manual(name = "Boundary", values = c("Urban Area" = "#0066FF")) +
    scale_x_continuous(breaks = seq(106.2, 107.4, by = 0.5), labels = ~ paste0(.x, "°E")) +
    scale_y_continuous(breaks = seq(28.8, 30.2, by = 0.4), labels = ~ paste0(.x, "°N")) +
    labs(title = paste0(y, "年")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(), axis.title = element_blank(),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
    )
  
  if (i == 3) {
    p <- p + annotation_north_arrow(location = "tr", height = unit(1.2, "cm"), width = unit(1.2, "cm"))
  }
  
  plot_list[[as.character(y)]] <- p
}

# --- 4. 提取并组合底部组件 ---

# 4.1 提取包含蓝色线条和LST的复合图例
shared_legend <- get_legend(
  plot_list[[1]] + theme(
    legend.box = "horizontal",
    legend.spacing.x = unit(1, "cm"),
    legend.text = element_text(size = 10, face = "bold")
  )
)

# 4.2 隐藏主图图例
plot_list <- lapply(plot_list, function(x) x + theme(legend.position = "none"))

# 4.3 创建超大比例尺 (Scale Bar)
# 增大宽度比例，并使用样式更明显的数字标注
scale_bar_plot <- ggplot() +
  coord_sf(xlim = c(106.2, 107.4), ylim = c(28.8, 30.2), crs = 4326) +
  annotation_scale(
    location = "bl", width_hint = 0.5, # 占据一半宽度
    text_size = 5, unit = "km", 
    style = "ticks", line_width = 2
  ) +
  theme_void()

# --- 5. 组图逻辑 ---
grid_body <- plot_grid(plotlist = plot_list, ncol = 3, align = 'hv')

# 底部页脚：比例尺在左，复合图例在右
footer <- plot_grid(
  scale_bar_plot, shared_legend, 
  ncol = 2, rel_widths = c(0.4, 0.6)
)

# 最终合成
final_map <- plot_grid(
  grid_body, footer, 
  ncol = 1, rel_heights = c(1, 0.15)
) + theme(plot.background = element_rect(fill = "white", color = NA))

# --- 6. 保存 ---
output_file <- paste0(base_out_path, "CQ_LST_Urban_Development.png")
ggsave(output_file, final_map, width = 18, height = 12, dpi = 300, bg = "white")

message(paste0("✅ 比例尺已放大，且已添加蓝色城区图例：", output_file))