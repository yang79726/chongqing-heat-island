library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggspatial) 
library(scales)
library(dplyr)

# --- 1. 基础配置与量程计算 ---
base_data_path <- "D:/university/GIS/LST/data/"
base_out_path  <- "D:/university/GIS/LST/output/"
years <- c(2000, 2005, 2010, 2015, 2020, 2025)
plot_list <- list()

# 预扫描量程 (此处保持不变)
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

# --- 2. 循环绘图 ---
for (i in 1:length(years)) {
  y <- years[i]
  tif_path <- paste0(base_data_path, "CQ_ROI_LST_", y, ".tif")
  shp_path <- paste0(base_out_path, "Boundary_", y, ".shp")
  if (!file.exists(tif_path) || !file.exists(shp_path)) next
  
  lst_r <- raster(tif_path, band = 1)
  bnd   <- st_read(shp_path, quiet = TRUE) %>% st_transform(crs = st_crs(lst_r))
  rural_mask <- mask(lst_r, bnd, inverse = TRUE)
  t_rural <- mean(getValues(rural_mask), na.rm = TRUE)
  
  lst_df <- as.data.frame(lst_r, xy = TRUE)
  colnames(lst_df) <- c("x", "y", "LST")
  lst_df <- na.omit(lst_df)
  lst_df$delta_T <- lst_df$LST - t_rural
  
  # 分级标签
  lst_df$intensity <- cut(lst_df$delta_T, breaks = c(-Inf, 3, 5, Inf),
                          labels = c("弱热岛(<3℃)", "次强热岛(3-5℃)", "强热岛(≥5℃)"))
  
  p <- ggplot() +
    geom_raster(data = lst_df, aes(x = x, y = y, fill = intensity)) +
    scale_fill_manual(
      values = c("弱热岛(<3℃)" = "#fee0d2", "次强热岛(3-5℃)" = "#fb6a4a", "强热岛(≥5℃)" = "#a50f15"),
      name = "热岛强度等级"
    ) +
    geom_sf(data = bnd, aes(color = "城区缓冲边界"), fill = NA, size = 1.2) + 
    scale_color_manual(name = "", values = c("城区缓冲边界" = "#0066FF")) +
    scale_x_continuous(breaks = seq(106.2, 107.4, by = 0.5), labels = ~ paste0(.x, "°E")) +
    scale_y_continuous(breaks = seq(28.8, 30.2, by = 0.4), labels = ~ paste0(.x, "°N")) +
    labs(title = paste0(y, "年")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(), axis.title = element_blank(),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  if (i == 3) {
    p <- p + annotation_north_arrow(location = "tr", height = unit(1.2, "cm"), width = unit(1.2, "cm"))
  }
  plot_list[[as.character(y)]] <- p
}

# --- 3. 提取与美化组件 ---

# A. 分级图例：强制横向，大幅拉开字色间距
# A. 分级图例：拉大标题与弱热岛色块的间距 + 横向排列
intensity_legend <- get_legend(
  plot_list[[1]] + 
    guides(fill = guide_legend(
      nrow = 1,                # 保持横向排列
      label.position = "right",# 文字在色块右侧
      keywidth = unit(1.0, "cm"),# 色块宽度不变
      title.position = "top",   # 标题在图例上方（可选，也可保留left）
      title.theme = element_text(
        size = 12, 
        margin = margin(b = 10, r = 30) # 关键：title的r=30 → 标题右侧（离弱热岛色块）留30pt间距
      ),
      label.theme = element_text(
        size = 11, 
        margin = margin(l = 20, r = 60) # 文字与色块、文字之间的间距不变
      ) 
    )) +
    guides(color = "none")  # 隐藏边界图例
)

# B. 城区边界图例（极简样式，适配右侧布局）
# B. 城区缓冲区图例（标签改为“城区缓冲区”）
blue_legend <- get_legend(
  plot_list[[1]] + 
    guides(fill = "none") + 
    guides(color = guide_legend(
      override.aes = list(linetype = 1, shape = NA), # 线条样式
      keywidth = unit(2, "cm"), # 加长线条
      label.position = "right",
      label.theme = element_text(size = 10, margin = margin(l = 5))
    )) +
    theme(
      legend.key = element_blank(),
      legend.margin = margin(0, 0, 0, 0)
    )
)

# C. 比例尺（适配左侧布局）
# C. 比例尺（上移，适配对齐）
long_scale <- ggplot() +
  coord_sf(xlim = c(106.2, 107.4), ylim = c(28.8, 30.2), crs = 4326) +
  annotation_scale(
    location = "bl", 
    width_hint = 0.6, 
    text_cex = 1.0, 
    style = "ticks",
    line_width = 1.5
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 15, 0, 0, 0) # 顶部添加15pt边距，让比例尺上移
  )


# --- 4. 页脚重构：全部图例同一层（比例尺左、热岛分级中、城区缓冲区右）---
footer <- plot_grid(
  long_scale,         # 比例尺（最左）
  intensity_legend,   # 热岛强度分级（中间）
  blue_legend,        # 城区缓冲区（最右）
  ncol = 3,           # 横向排列（同一层）
  rel_widths = c(0.2, 0.6, 0.2), # 宽度占比：比例尺20%、热岛分级60%、缓冲区20%
  align = "h",        # 水平对齐
  axis = "tb"         # 垂直居中
)
# --- 5. 拼大图 ---
grid_body <- plot_grid(
  plotlist = lapply(plot_list, function(x) x + theme(legend.position = "none")), 
  ncol = 3, align = 'hv'
)

final_map <- plot_grid(
  grid_body, footer, 
  ncol = 1, 
  rel_heights = c(1, 0.2) # 给底部留 20% 空间
) + theme(plot.background = element_rect(fill = "white", color = NA))

# 保存
ggsave(paste0(base_out_path, "CQ_UHI_Optimized_Layout.png"), 
       final_map, width = 20, height = 14, dpi = 300)

message("✅ 修复完成：分级图例字色已拉开，边界与比例尺已合并至次行。")