# 多期LST城区提取（温度>43℃+矢量合并+小斑消除）- 投影修复终极版
# 适配无投影信息的经纬度LST数据（0.000269°分辨率≈30m）
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(exactextractr)

# ===================== 配置参数（适配经纬度数据）=====================
years <- c(2000, 2005, 2010, 2015, 2020, 2025)
base_path <- "D:/university/GIS/LST/data/"     
output_path <- "D:/university/GIS/LST/output/" 
temp_threshold <- 43                           
min_patch_pixels <- 111                        
admin_shp_path <- "D:/university/GIS/LST/data/chongqing_admin.shp"
# 手动指定投影（WGS84，适配经纬度数据）
wgs84_crs <- st_crs(4326)  # EPSG:4326 = WGS84经纬度

# 创建输出目录
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
  cat(paste0("📁 已创建输出目录：", output_path, "\n"))
}

# ===================== 核心函数（修复投影/参数冗余问题）=====================
#' 读取LST数据（补全投影信息，适配经纬度）
read_lst <- function(lst_path) {
  # 读取原始LST栅格
  lst_raster <- raster(lst_path)
  
  # 检查数据有效性
  if (is.null(lst_raster)) {
    stop(paste0("无法读取LST文件：", lst_path))
  }
  
  # 补全投影信息（核心修复：给无投影的栅格赋值WGS84）
  if (is.na(crs(lst_raster))) {
    crs(lst_raster) <- wgs84_crs$wkt
    cat("⚠️  LST数据无投影信息，已手动赋值WGS84（EPSG:4326）\n")
  }
  
  # 仅转换像元值为数值型（不改变栅格对象）
  lst_vals <- values(lst_raster)
  lst_vals <- as.numeric(lst_vals)
  values(lst_raster) <- lst_vals
  
  # 处理NA值
  lst_raster[is.na(lst_raster)] <- 0
  
  return(lst_raster)
}

#' 提取43℃以上高温区（移除冗余crs/extent参数）
extract_high_temp <- function(lst_raster) {
  # 1. 筛选温度>43℃的像元
  high_temp_raster <- lst_raster > temp_threshold
  
  # 2. 转换为二进制栅格（1=高温区，0=非高温区）
  values(high_temp_raster) <- as.integer(values(high_temp_raster))
  
  # 3. 剔除小斑块（3×3窗口滤波，核心：不移除冗余参数）
  high_temp_filtered <- focal(high_temp_raster, 
                              w = matrix(1, 3, 3), 
                              fun = sum, 
                              na.rm = TRUE)
  
  # 保留连续区域（窗口内≥5个像元）
  high_temp_filtered[high_temp_filtered < 5] <- NA
  
  # 4. 转换为二进制（移除冗余的raster()参数，自动继承地理信息）
  high_temp_final <- high_temp_filtered
  high_temp_final[!is.na(high_temp_final)] <- 1
  high_temp_final[is.na(high_temp_final)] <- 0
  
  return(high_temp_final)
}

#' 栅格转矢量（适配经纬度投影）
raster_to_vector <- function(high_temp_raster, output_shp_path) {
  # 仅保留值=1的高温区
  high_temp_raster[high_temp_raster != 1] <- NA
  
  # 栅格转矢量（核心：移除冗余参数，自动继承投影）
  vector_sp <- rasterToPolygons(high_temp_raster, 
                                fun = function(x) !is.na(x), 
                                dissolve = TRUE)
  
  # 转换为sf对象（强制指定WGS84投影）
  vector_sf <- st_as_sf(vector_sp) %>% 
    st_set_crs(wgs84_crs)
  
  # 保存矢量
  if (nrow(vector_sf) > 0) {
    st_write(vector_sf, output_shp_path, delete_layer = TRUE, quiet = TRUE)
    cat(paste0("✅ 临时矢量文件生成：", output_shp_path, "\n"))
  } else {
    cat("⚠️  无高温区数据，跳过矢量保存\n")
  }
  
  return(vector_sf)
}

#' 合并内部多边形（消除挖空）
merge_internal_polygons <- function(vector_sf) {
  # 处理空矢量
  if (nrow(vector_sf) == 0) {
    warning("无高温区矢量可合并")
    return(vector_sf)
  }
  
  # 合并所有面要素
  merged_geom <- st_union(vector_sf$geometry)
  
  # 处理多部分面
  if (st_geometry_type(merged_geom) == "MULTIPOLYGON") {
    merged_sf <- st_sf(geometry = st_cast(merged_geom, "POLYGON"), 
                       crs = wgs84_crs)
  } else if (st_geometry_type(merged_geom) == "POLYGON") {
    merged_sf <- st_sf(geometry = merged_geom, 
                       crs = wgs84_crs)
  } else {
    merged_sf <- vector_sf
  }
  
  return(merged_sf)
}

#' 裁剪至行政边界（统一投影为WGS84）
clip_to_admin <- function(vector_sf, admin_shp_path) {
  # 读取行政边界并转换为WGS84
  admin_sf <- st_read(admin_shp_path, quiet = TRUE) %>% 
    st_transform(wgs84_crs)
  
  # 裁剪（处理空矢量）
  if (nrow(vector_sf) > 0) {
    clipped_sf <- st_intersection(vector_sf, admin_sf)
  } else {
    clipped_sf <- vector_sf
  }
  
  return(clipped_sf)
}

#' 计算城区统计信息（适配经纬度面积转换）
calc_urban_stats <- function(urban_sf, lst_raster) {
  # 处理空矢量
  if (nrow(urban_sf) == 0) {
    return(list(area_km2 = 0, avg_temp = 0))
  }
  
  # 核心修复：经纬度转UTM计算面积（避免面积失真）
  # 自动匹配重庆UTM投影（EPSG:32648，UTM Zone 48N）
  utm_crs <- st_crs(32648)
  urban_sf_utm <- st_transform(urban_sf, utm_crs)
  
  # 计算面积（km²）
  urban_area_km2 <- st_area(urban_sf_utm) %>% 
    sum() %>% 
    as.numeric() %>% 
    divide_by(1e6)
  
  # 计算平均温度
  urban_lst_stats <- exact_extract(lst_raster, urban_sf, fun = "mean")
  urban_avg_temp <- ifelse(is.na(urban_lst_stats), 0, urban_lst_stats)
  
  return(list(area_km2 = round(urban_area_km2, 2), 
              avg_temp = round(urban_avg_temp, 2)))
}

# ===================== 批量处理主流程 =====================
for (year in years) {
  tryCatch({
    cat(paste0("\n========== 开始处理 ", year, " 年数据 ==========\n"))
    
    # 1. 构建文件路径
    lst_tif_path <- file.path(base_path, paste0("CQ_ROI_LST_", year, ".tif"))
    output_shp_path <- file.path(output_path, paste0("Urban_Area_", year, ".shp"))
    
    # 2. 读取LST数据（补全投影）
    lst_raster <- read_lst(lst_tif_path)
    cat(paste0("📊 ", year, "年LST数据信息：\n"))
    cat(paste0("   - 分辨率：", res(lst_raster)[1], "° × ", res(lst_raster)[2], "°（≈30m）\n"))
    cat(paste0("   - 投影：", st_crs(crs(lst_raster))$epsg, "\n"))
    
    # 3. 提取>43℃高温区
    high_temp_raster <- extract_high_temp(lst_raster)
    high_temp_pixels <- sum(values(high_temp_raster) == 1, na.rm = TRUE)
    # 经纬度像元面积≈30m×30m=900m²
    high_temp_area_km2 <- high_temp_pixels * 900 / 1e6
    cat(paste0("🔥 ", year, "年>43℃高温区像元数：", high_temp_pixels, 
               "（约", round(high_temp_area_km2, 2), "km²）\n"))
    
    # 4. 栅格转矢量
    high_temp_sf <- raster_to_vector(high_temp_raster, output_shp_path)
    
    # 5. 合并内部多边形
    merged_sf <- merge_internal_polygons(high_temp_sf)
    cat(paste0("🔗 ", year, "年合并后城区面数量：", nrow(merged_sf), "\n"))
    
    # 6. 裁剪至行政边界
    clipped_sf <- clip_to_admin(merged_sf, admin_shp_path)
    
    # 7. 修复拓扑并保存
    if (nrow(clipped_sf) > 0) {
      clipped_sf <- st_make_valid(clipped_sf)
      st_write(clipped_sf, output_shp_path, delete_layer = TRUE, quiet = TRUE)
      cat(paste0("💾 ", year, "年城区矢量已保存：", output_shp_path, "\n"))
    } else {
      cat(paste0("⚠️  ", year, "年无有效城区数据，未保存矢量\n"))
    }
    
    # 8. 统计信息
    urban_stats <- calc_urban_stats(clipped_sf, lst_raster)
    cat(paste0("🏙️ ", year, "年城区面积：", urban_stats$area_km2, " km²\n"))
    cat(paste0("🌡️ ", year, "年城区平均温度：", urban_stats$avg_temp, " ℃\n"))
    cat(paste0("✅ ", year, "年数据处理完成！\n"))
    
  }, error = function(e) {
    cat(paste0("❌ ", year, "年数据处理失败：", e$message, "\n"))
    # 输出详细错误栈
    cat("   错误详情：", capture.output(print(e)), "\n")
  })
}

# 最终提示
cat(paste0("\n🎉 所有年份城区提取完成！最终结果已保存至：", output_path, "\n"))
cat("📌 输出文件说明：\n")
cat("   - Urban_Area_YYYY.shp：各年份城区边界矢量（WGS84投影，可直接导入ArcGIS/QGIS）\n")
cat("   - 控制台输出：城区面积/平均温度（已修正经纬度面积失真问题）\n")