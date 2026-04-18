library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggspatial) 
library(scales)
library(dplyr)

# --- 1. 路径配置 ---
path_in  <- "D:/university/GIS/LST/data/CQ_Analysis_2025_Final.tif"
path_shp <- "D:/university/GIS/LST/output/Boundary_2025.shp"
path_out <- "D:/university/GIS/LST/output/"

# 专题图波段配置 ( band顺序需对应GEE导出 )
topics <- list(
  list(band = 2, name = "NDVI",  title = "植被指数 (NDVI)",   pal = "viridis",  lab = "NDVI Value"),
  list(band = 3, name = "NDBI",  title = "建设用地 (NDBI)",   pal = "rocket",   lab = "NDBI Value"),
  list(band = 4, name = "MNDWI", title = "水体指数 (MNDWI)",  pal = "mako",     lab = "MNDWI Value"),
  list(band = 5, name = "NDBaI", title = "裸土指数 (NDBaI)",  pal = "inferno",  lab = "NDBaI Value"),
  list(band = 6, name = "NDISI", title = "不透水面 (NDISI)",  pal = "magma",    lab = "NDISI Value")
)

bnd <- st_read(path_shp, quiet = TRUE)

# --- 2. 循环生成专题图 ---
for (item in topics) {
  cat("\n正在处理:", item$title, "\n")
  
  # A. 读取并优化数据量 (降采样防止无响应)
  r_raw <- raster(path_in, band = item$band)
  if(ncell(r_raw) > 1e6) {
    r <- aggregate(r_raw, fact = 2, fun = mean) 
  } else {
    r <- r_raw
  }
  
  df <- as.data.frame(r, xy = TRUE) %>% filter(!is.na(.[,3]))
  colnames(df) <- c("x", "y", "val")
  bnd_proj <- st_transform(bnd, crs = st_crs(r))
  
  # B. 绘图 (包含蓝色边界映射)
  p <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = val)) +
    # 蓝色边界图例映射
    geom_sf(data = bnd_proj, aes(color = "Urban Area"), fill = NA, size = 1.2) +
    scale_fill_viridis(
      option = item$pal, 
      name = item$lab,
      guide = guide_colorbar(
        direction = "horizontal", 
        barwidth = 15, 
        barheight = 0.6, 
        title.position = "top"
      )
    ) +
    scale_color_manual(name = "Boundary", values = c("Urban Area" = "#0066FF")) +
    scale_x_continuous(breaks = seq(106.2, 107.4, by = 0.4), labels = ~ paste0(.x, "°E")) +
    scale_y_continuous(breaks = seq(28.8, 30.2, by = 0.4), labels = ~ paste0(.x, "°N")) +
    labs(title = paste0("2025年重庆核心区 ", item$title)) +
    annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(), axis.title = element_blank(),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "bottom"
    )
  
  # C. 提取复合图例 (解决字色重叠)
  shared_legend <- get_legend(
    p + theme(
      legend.box = "horizontal",
      legend.spacing.x = unit(1.2, "cm"), # 增加指数图例与边界图例的间距
      legend.text = element_text(size = 10, face = "bold", margin = margin(l = 5)), # 字色分离
      legend.title = element_text(size = 11, face = "bold")
    )
  )
  
  # D. 创建左侧比例尺
  scale_bar_plot <- ggplot() +
    coord_sf(xlim = range(df$x), ylim = range(df$y), crs = 4326) +
    annotation_scale(
      location = "bl", width_hint = 0.4, 
      style = "ticks", line_width = 1.5, text_cex = 1.0
    ) +
    theme_void()
  
  # E. 组装页脚 (比例尺左，图例右)
  footer <- plot_grid(
    scale_bar_plot, shared_legend, 
    ncol = 2, rel_widths = c(0.35, 0.65), align = "h"
  )
  
  # F. 最终合成
  final_map <- plot_grid(
    p + theme(legend.position = "none"), 
    footer, 
    ncol = 1, rel_heights = c(1, 0.18)
  ) + theme(plot.background = element_rect(fill = "white", color = NA))
  
  # G. 保存独立专题图
  save_name <- paste0(path_out, "CQ_2025_Index_", item$name, ".png")
  ggsave(save_name, final_map, width = 12, height = 11, dpi = 300, bg = "white")
  cat("✅ 已保存: ", save_name, "\n")
}