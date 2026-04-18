library(ggplot2)
library(sf)
library(terra)
library(cowplot) 
library(ggspatial)

# --- 1. 数据读取 ---
chongqing_boundary <- st_read("D:/university/GIS/LST/data/chongqin.shp", quiet = TRUE)
study_area_boundary <- st_read("D:/university/GIS/LST/data/roi.shp", quiet = TRUE)
study_area_raster <- rast("D:/university/GIS/LST/data/CQ_ROI_LST_2025.tif")
raster_df <- as.data.frame(study_area_raster, xy = TRUE)
colnames(raster_df) <- c("x", "y", "temperature")

# 获取研究区中心点（用于标注区名）
study_area_centers <- st_centroid(study_area_boundary)

# --- 2. 绘制左图 (重庆市区位) - 经纬度刻度+无网格+无汉字标注 ---
p_left <- ggplot() +
  geom_sf(data = chongqing_boundary, fill = "white", color = "gray60", linewidth = 0.5) +
  geom_sf(data = study_area_boundary, fill = "#FFCC80", color = "black", linewidth = 0.8) +
  
  # 手动图例
  annotate("text", x = st_bbox(chongqing_boundary)$xmin + 0.2, 
           y = st_bbox(chongqing_boundary)$ymax - 0.05, 
           label = "图例", hjust = 0, size = 4, fontface = "bold") +
  annotate("rect", xmin = st_bbox(chongqing_boundary)$xmin + 0.2, 
           xmax = st_bbox(chongqing_boundary)$xmin + 0.6, 
           ymin = st_bbox(chongqing_boundary)$ymax - 0.3, 
           ymax = st_bbox(chongqing_boundary)$ymax - 0.2, 
           fill = "white", color = "gray60") +
  annotate("text", x = st_bbox(chongqing_boundary)$xmin + 0.7, 
           y = st_bbox(chongqing_boundary)$ymax - 0.25, 
           label = "重庆市边界", hjust = 0, size = 3.5) +
  annotate("rect", xmin = st_bbox(chongqing_boundary)$xmin + 0.2, 
           xmax = st_bbox(chongqing_boundary)$xmin + 0.6, 
           ymin = st_bbox(chongqing_boundary)$ymax - 0.5, 
           ymax = st_bbox(chongqing_boundary)$ymax - 0.4, 
           fill = "#FFCC80", color = "black") +
  annotate("text", x = st_bbox(chongqing_boundary)$xmin + 0.7, 
           y = st_bbox(chongqing_boundary)$ymax - 0.45, 
           label = "研究区（6个区）", hjust = 0, size = 3.5) +
  
  # 左图经纬度刻度（无汉字标注）
  scale_x_continuous(breaks = seq(105, 110, 1), name = "") +  # 清空x轴名称
  scale_y_continuous(breaks = seq(28, 32, 1), name = "") +  # 清空y轴名称
  
  # 比例尺
  annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("black", "white"), text_cex = 0.8) +
  
  # 左图去除网格 + 仅保留刻度/边框
  theme_bw() +  
  theme(
    plot.margin = margin(0,0,0,0),
    panel.grid = element_blank(),  # 去除左图网格
    axis.text = element_text(size = 8),  # 刻度文字大小
    axis.title = element_blank()   # 确保轴名称为空
  )

# --- 3. 绘制右图 (研究区地表温度) - 无网格+无经纬度汉字+标注区名 ---
p_right <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = temperature)) +
  scale_fill_gradientn(colors = c("#2166AC", "#4393C3", "#92C5DE", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"), 
                       name = "地表温度/℃") +
  geom_sf(data = study_area_boundary, fill = NA, color = "black", linewidth = 0.8) +
  # 添加6个区的名称（dt_name字段）
  geom_sf_text(data = study_area_centers, aes(label = dt_name), size = 3, fontface = "bold") +
  
  # 比例尺+北向箭头
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()) +
  coord_sf(expand = FALSE) + 
  theme_bw() +
  # 清空经纬度汉字标注 + 去除网格
  labs(x = "", y = "") +  # 清空x/y轴名称
  theme(
    plot.margin = margin(10,10,10,20),
    panel.grid = element_blank(),  # 去除右图网格
    axis.title = element_blank(),  # 确保轴名称为空
    axis.text = element_text(size = 8)  # 保留刻度文字
  )

# --- 4. 组合+标题+连线 ---
combined <- plot_grid(p_left, p_right, ncol = 2, rel_widths = c(1, 2))
final_plot <- ggdraw(combined) +
  theme(plot.background = element_rect(fill = "white", color = NA)) +
  draw_label("重庆市研究区（6个区）区位与地表温度图", 
             x = 0.5, y = 0.98, hjust = 0.5, vjust = 1, 
             fontface = "bold", size = 18) +
  draw_line(x = c(0.12, 0.68), y = c(0.5, 0.95), 
            color = "red", linetype = "dashed", size = 1) +
  draw_line(x = c(0.1, 0.46), y = c(0.42, 0.03), 
            color = "red", linetype = "dashed", size = 1)

# --- 5. 保存 ---
ggsave("chongqing_study_area.png", final_plot, 
       width = 12, height = 8, dpi = 300, bg = "white") 

cat("✅ 图片已保存：左图无网格、无经纬度汉字标注，右图无网格+无经纬度汉字标注！\n")