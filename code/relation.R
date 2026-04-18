# 加载必要库
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# --- 1. 路径与数据加载 ---
path_in  <- "D:/university/GIS/LST/data/CQ_Analysis_2025_Final.tif"
path_out <- "D:/university/GIS/LST/output/"

# 检查文件是否存在
if(!file.exists(path_in)) stop("找不到输入的TIF文件，请检查路径！")

# 加载多波段堆栈
s_raw <- stack(path_in)
# 确保波段名称与导出顺序一致
# 1:LST, 2:NDVI, 3:NDBI, 4:MNDWI, 5:NDBaI, 6:NDISI
names(s_raw) <- c("LST", "NDVI", "NDBI", "MNDWI", "NDBaI", "NDISI")

# --- 2. 稳健的随机抽样 ---
set.seed(123)
cat("正在进行随机抽样（5000点）以计算相关性...\n")

sample_raw <- sampleRandom(s_raw, size = 5000, df = TRUE)
sample_df  <- as.data.frame(sample_raw)

# 动态剔除可能存在的非分析列（如 ID, x, y）
# 只保留我们命名的那 6 个核心变量
analysis_vars <- c("LST", "NDVI", "NDBI", "MNDWI", "NDBaI", "NDISI")
sample_data   <- sample_df[, names(sample_df) %in% analysis_vars] %>% 
  na.omit()

cat("抽样完成，有效样本量:", nrow(sample_data), "\n")

# --- 3. 相关性系数计算与热图绘制 ---
cor_matrix <- cor(sample_data, method = "pearson")

# 保存相关性热图
png(paste0(path_out, "LST_Correlation_Matrix.png"), width = 1000, height = 1000, res = 150)
# 如果系统支持中文，可以使用 family='SimSun'，否则建议用英文标题避免乱码
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", # 聚类排序，让关系近的靠在一起
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         diag = FALSE,
         title = "Correlation: LST vs Land Surface Factors", 
         mar = c(0,0,2,0))
dev.off()
cat("✅ 相关性热图已保存至 output 文件夹。\n")

# --- 4. 散点回归分析 (多分面绘图) ---
# 转换为长格式以便批量绘图
sample_long <- sample_data %>%
  tidyr::pivot_longer(cols = -LST, names_to = "Factor", values_to = "IndexValue")

p_scatter <- ggplot(sample_long, aes(x = IndexValue, y = LST, color = Factor)) +
  geom_point(alpha = 0.2, size = 0.6) + 
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1) + 
  facet_wrap(~Factor, scales = "free_x") + 
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Relationship between LST and Environmental Indices (2025)",
       subtitle = "Linear Regression Analysis with 5,000 Sample Points",
       x = "Index Value", 
       y = "Land Surface Temperature (°C)") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 保存回归分析图
ggsave(paste0(path_out, "LST_Regression_Facets.png"), p_scatter, width = 12, height = 8, dpi = 300)
cat("✅ 散点回归分面图已保存至 output 文件夹。\n")

# --- 5. 控制台输出统计结果 ---
cat("\n--- LST 相关性统计报告 ---\n")
print(round(cor_matrix["LST", ], 3))
# 确保数据已经加载 (使用之前 relation.R 中的 sample_data)

# --- 1. 标准化回归 (Standardized Regression) ---
# 为什么要标准化？因为 NDVI 是 0-1 之间，而 LST 是 20-40 度，
# 不标准化无法比较“谁的贡献更大”。
sample_scaled <- as.data.frame(scale(sample_data))

# 建立多元回归模型
model_multi <- lm(LST ~ NDVI + NDBI + MNDWI + NDBaI + NDISI, data = sample_scaled)

# 查看结果
model_summary <- summary(model_multi)
print(model_summary)

# --- 2. 提取贡献度 (系数绝对值) ---
# 获取各因子的回归系数 (Beta)
coeffs <- as.data.frame(model_summary$coefficients)
coeffs$Factor <- rownames(coeffs)
# 排除截距项 (Intercept)
coeffs_plot <- coeffs %>% 
  filter(Factor != "(Intercept)") %>%
  mutate(Contribution = abs(Estimate),
         Direction = ifelse(Estimate > 0, "增温效应", "降温效应"))

# --- 3. 绘制贡献度对比图 ---
p_importance <- ggplot(coeffs_plot, aes(x = reorder(Factor, Contribution), y = Contribution, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() + # 横向柱状图
  scale_fill_manual(values = c("增温效应" = "#e41a1c", "降温效应" = "#377eb8")) +
  labs(title = "地表要素对 LST 的贡献度排序 (Beta系数)",
       subtitle = paste0("模型解释力度 (R-squared): ", round(model_summary$r.squared, 3)),
       x = "地理要素指数", y = "贡献强度 (标准化系数绝对值)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 保存图片
ggsave(paste0(path_out, "LST_Factor_Contribution.png"), p_importance, width = 8, height = 6)