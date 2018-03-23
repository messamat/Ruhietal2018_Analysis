__author__ = 'Mathis Messager'

import arcpy
from arcpy import env
import sys

arcpy.env.overwriteOutput = True

if arcpy.CheckExtension("Spatial") == "Available":
    arcpy.AddMessage("Checking out Spatial")
    arcpy.CheckOutExtension("Spatial")
else:
    arcpy.AddError("Unable to get spatial analyst extension")
    arcpy.AddMessage(arcpy.GetMessages(0))
    sys.exit(0)

wd = "F:/gages_project/results"
env.workspace = wd

wsr = "F:\gages_project\data\general\National_Wild_and_Scenic_River_Segments_Feature_Layer_20180314\\National_Wild_and_Scenic_River_Segments_Feature_Layer.shp"

#Project
gages = '\\gages\\gages_analysis.gdb\\allgages_merge'
print(arcpy.Describe(gages).spatialReference.name)
print(arcpy.Describe(wsr).spatialReference.name)

#Near analysis
#Check results through near analysis
arcpy.CopyFeatures_management(gages,'\\gages\\gages_wildandscenic')
in_features = '\\gages\\gages_wildandscenic.shp'
near_features = wsr
location = "LOCATION"
angle = "NO_ANGLE"
method = "GEODESIC"
arcpy.Near_analysis(in_features, near_features, location = location, angle = angle, method = method)

#Check those within 500 m of WSR
"""
Add field in gages_wildandscenic fieed ConfirmWSR where Y means that a visual check was performed to confirm that gage
is associated with the river and N means that the gage was not associated with a WSR.  Those on the stem of a WSR
within 500 m and not separated from the last portion of WSR by a major tributary were kept (i.e. nearby tributaries, even
large are not included).
"""

#Make a subselection
arcpy.MakeFeatureLayer_management(in_features, 'gageswsrlyr')
arcpy.SelectLayerByAttribute_management('gageswsrlyr', selection_type='NEW_SELECTION', where_clause="ConfirmWSR = 'Y'")
arcpy.CopyFeatures_management('gageswsrlyr', '\\gages\\gages_wildandscenic_select.shp')


#Copy table
arcpy.MakeTableView_management('gageswsrlyr', 'gageswsrtable')
arcpy.CopyRows_management(in_rows='gageswsrtable',out_table='\\gages\\gages_wildandscenic_select.dbf')