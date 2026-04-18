# -*- coding: utf-8 -*-
import arcpy
import os

# ---------------------- 适配你的路径和要素类 ----------------------
arcpy.env.workspace = r"D:\university\GIS\LST\data\新建文件地理数据库.gdb"  # 你的GDB路径
arcpy.env.overwriteOutput = True  # 允许覆盖输出
input_fc = "RasterT_Con_tif1_EliminatePo2"  # 你的要素类名称
area_threshold = 100  # 面积阈值（单位：平方米，需根据你的数据调整）

# ---------------------- 执行删除操作 ----------------------
try:
    # 1. 检查并添加Area字段（存储面积）
    field_names = [f.name for f in arcpy.ListFields(input_fc)]
    if "Area" not in field_names:
        arcpy.AddField_management(input_fc, "Area", "DOUBLE")
        print("✅ 已添加Area面积字段")

    # 2. 计算每个多边形的面积（使用图层自身坐标系）
    arcpy.CalculateGeometryAttributes_management(
        in_features=input_fc,
        geometry_property=[["Area", "AREA"]],
        coordinate_system=arcpy.Describe(input_fc).spatialReference
    )
    print("✅ 已完成面积计算")

    # 3. 筛选并选中小于阈值的细碎多边形
    where_clause = f"Area < {area_threshold}"
    arcpy.SelectLayerByAttribute_management(input_fc, "NEW_SELECTION", where_clause)

    # 4. 删除选中的细碎多边形
    selected_count = int(arcpy.GetCount_management(input_fc)[0])
    if selected_count > 0:
        arcpy.DeleteFeatures_management(input_fc)
        print(f"✅ 成功删除 {selected_count} 个面积小于 {area_threshold} 平方米的细碎多边形")
    else:
        print(f"✅ 未找到面积小于 {area_threshold} 平方米的细碎多边形")

    # 清除选中状态
    arcpy.SelectLayerByAttribute_management(input_fc, "CLEAR_SELECTION")

except Exception as e:
    print(f"❌ 执行出错: {str(e)}")