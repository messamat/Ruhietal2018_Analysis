__author__ = 'Mathis Messager'
#Contact info: messamat@uw.edu
#Creation date: June 2016
#Last updated: February 23rd 2018

#Objective: This script spatially joins USGS stream gages to flowlines in the National Hydrography Dataset v2 Plus
#           The NHDv2 plus dataset includes a set of gages that were snapped to the digital network by expert knowledge throughout the conterminous USA
#           The NHDv2 plus dataset is also divided into two datasets of flow lines: network and non-network lines
#           non-network lines are essentially isolated flow lines or flow lines with no set direction (often diversions and artificial waterways)
#           Therefore, the workflow is as follow:
#           A. Join gages with discharge data downloaded from NWIS to gages feature points from the NHDv2Plus by ID (USGS NWIS Site Number) (21268/23569)
#           B. For those gages that did not join to NHDv2plus, join them spatially to the network flowlines (snap to the closest flowline within 500 m) (1423/1875 (without AK and HW)
#           C. For those gages that did not snap to a network flowline, join them spatially to non-network flowlines (snap to the closest non-network flowline within 500m) (81/452)
#           D. For those gages that neither snapped to the network nor the non-network flowlines, inspect each individually and snap them to closest network or non-network flowlines when relevant (/371)

import arcpy

arcpy.env.workspace = "F:/gages_project/results/gages/gages_analysis.gdb"
arcpy.env.overwriteOutput = True
arcpy.env.qualifiedFieldNames = False

#Import gages from NHDv2+ (Gages in NHDv2+ have been snapped to network with expert knowledge in the conterminous USA)
NHDpath = "F:\gages_project\data\general\NHDplusv2\NHDPlusV21_NationalData_National_Seamless_Geodatabase_02\NHDPlusNationalData\NHDPlusV21_National_Seamless.gdb\\"
proj_gage = NHDpath+"NHDEvents\\Gage"
#Import NHDv2 flowlines (I excluded coastlines)
NHD_net = NHDpath+"NHDSnapshot\\NHDFlowline_Network_nocoast"
#NHDv2 non-network flowlines (generally those flow lines that are not part of a topologically functional network -- often diversions and artificial waterways)
NHD_nonet = NHDpath+"NHDSnapshot\\NHDFlowline_NonNetwork"

#Import flow lines to database
NHD_net_imp = "NHDFlowline_Network_nocoast"
arcpy.CopyFeatures_management(NHD_net, NHD_net_imp)
NHD_nonet_imp = "NHDFlowline_NonNetwork"
arcpy.CopyFeatures_management(NHD_nonet, NHD_nonet_imp)

##############################################################################
#  A. JOIN GAGES THAT ALREADY EXIST IN THE NHDV2 DATASET TO THE NETWORK BY ID
##############################################################################
#Join gages with network by ID to take out gages that are on non-network flowlines or coastlines
#Gage data availability dataset (previously formatted in Excel worksheet correctly to not have to deal with wrong IDs and date formats)
#Alternative way would have been to export directly as .dbf in R using 'foreign' package
gage_rec = r"F:/gages_project/results/gages/discharge_castdtinfo20180111.dbf"

#Join USGS gage data to NHD2 gages
NHD2gage_join =  "NHD2gage_discharge_castformat_join"
arcpy.MakeFeatureLayer_management(proj_gage, "gage_lyr")
arcpy.AddJoin_management("gage_lyr", in_field = "SOURCE_FEA", join_table = "discharge_castformat", join_field = "site_no", join_type = "KEEP_COMMON")
arcpy.CopyFeatures_management("gage_lyr", NHD2gage_join)
arcpy.Delete_management("gage_lyr")

#Create a table of those gages for which there is USGS discharge data but no corresponding event in NHDplus v2 exists
NHD2gage_nojoin =  "discharge_castformat_NHD2gage_nojoin"
arcpy.MakeTableView_management("discharge_castformat", out_view = "discharge_castformat_lyr")
arcpy.AddJoin_management("discharge_castformat_lyr", in_field = "site_no", join_table = proj_gage, join_field = "SOURCE_FEA", join_type = "KEEP_ALL")
arcpy.SelectLayerByAttribute_management("discharge_castformat_lyr", "NEW_SELECTION", 'SOURCE_FEA IS NULL')
arcpy.CopyRows_management("discharge_castformat_lyr", NHD2gage_nojoin)

#####################################################################
# B. SPATIALLY JOIN THOSE GAGES THAT ARE NOT IN NHDV2 TO THE NETWORK
####################################################################
## Create a feature class of points for those gages that did not join with the NHDv2 dataset but that we know have discharge records
in_XY_Table = "discharge_castformat_NHD2gage_nojoin"
#Coordinate fields are in string format so need to create new identical fields for in numeric format
[f.name for f in arcpy.ListFields(in_XY_Table, "*")]
#Create fields
if "dec_long_va_num" not in [f.name for f in arcpy.ListFields(in_XY_Table, "*va*")]:
    arcpy.AddField_management(in_XY_Table, 'dec_long_va_num', "DOUBLE")
if "dec_lat_va_num" not in [f.name for f in arcpy.ListFields(in_XY_Table)]:
    arcpy.AddField_management(in_XY_Table, 'dec_lat_va_num', "DOUBLE")

#Write fields (check for fields with typos such as longitude fields that are positive or latitude fields that are positive)
with arcpy.da.UpdateCursor(in_XY_Table, ['dec_long_va','dec_lat_va','dec_long_va_num','dec_lat_va_num']) as cursor:
    for row in cursor:
        if float(row[0]) > 0:
            row[2] = -float(row[0])
        else:
            row[2] = row[0]

        if float(row[1]) < 0:
            row[3] = -float(row[1])
        else:
            row[3] = row[1]
        cursor.updateRow(row)

#XLong and YLat fields
Long_Xfield = 'dec_long_va_num'
Lat_Yfield = 'dec_lat_va_num'
disgages = "gages_notinNHDv2"
#Set the spatial reference - default to WGS84 "4326"
Input_SpatialRef = arcpy.Describe(proj_gage).spatialReference
#make XY event layer
Temp_EventLayer = arcpy.MakeXYEventLayer_management(in_XY_Table, Long_Xfield, Lat_Yfield, "Output_eventlyr")
#Copy to gdb
arcpy.CopyFeatures_management("Output_eventlyr", disgages)

############################################################################################
## Join to stream network
arcpy.env.parallelProcessingFactor = "100%"
#Project the gages and network to accurately snap
sr = arcpy.Describe(NHD_net_imp).SpatialReference
arcpy.DefineProjection_management(disgages, sr)
arcpy.Describe(disgages).SpatialReference.name

pr = arcpy.SpatialReference('NAD 1983 Contiguous USA Albers')
disgages_proj = "gages_notinNHDv2_albers"
net_proj = "NHDFlowline_Network_nocoast2_albers"
nonet_proj = "NHDFlowline_NonNetwork_albers"
arcpy.Project_management(disgages, disgages_proj, pr)
arcpy.Project_management(NHD_net_imp, net_proj, pr)
arcpy.Project_management(NHD_nonet_imp, nonet_proj, pr)

#Take out gages in Alaska and Hawaii (because the NHDv2 does not include these areas
arcpy.MakeFeatureLayer_management(disgages_proj, "gages_notinNHDv2_lyr")
arcpy.SelectLayerByAttribute_management("gages_notinNHDv2_lyr", "NEW_SELECTION", 'dec_lat_va_num < 50 AND dec_lat_va_num > 25 ' )

#Snap gages
notinNHD_gages_snap = "gages_notinNHDv2_snap"
arcpy.CopyFeatures_management("gages_notinNHDv2_lyr", notinNHD_gages_snap)
snap_env = [net_proj, "EDGE", "500 Meters"]
arcpy.Snap_edit(notinNHD_gages_snap, [snap_env])

#Check results through near analysis
in_features = notinNHD_gages_snap
near_features = net_proj
location = "LOCATION"
angle = "NO_ANGLE"
method = "GEODESIC"
arcpy.Near_analysis(in_features, near_features, location = location, angle = angle, method = method)

#Select those that snapped and output to new feature class
arcpy.MakeFeatureLayer_management(notinNHD_gages_snap, "gages_notinNHDv2_snap_lyr")
[f.name for f in arcpy.ListFields(notinNHD_gages_snap)]
arcpy.SelectLayerByAttribute_management("gages_notinNHDv2_snap_lyr", "NEW_SELECTION", 'NEAR_DIST < 0.1 ' )
notinNHD_gages_networksnapped = "gages_notinNHDv2_networksnapped"
arcpy.CopyFeatures_management("gages_notinNHDv2_snap_lyr",notinNHD_gages_networksnapped)

#Select those that did not snap and try snapping to NHDv2 plus non-network lines
arcpy.SelectLayerByAttribute_management("gages_notinNHDv2_snap_lyr", "NEW_SELECTION", 'NEAR_DIST > 0.1 ' )
notinNHD_gages_snap_nonetwork = "gages_notinNHDv2_snap_nonetwork"
arcpy.CopyFeatures_management("gages_notinNHDv2_snap_lyr", notinNHD_gages_snap_nonetwork)
snap_env = [nonet_proj, "EDGE", "500 Meters"]
arcpy.Snap_edit(notinNHD_gages_snap_nonetwork, [snap_env])

#Check results through near analysis
in_features = notinNHD_gages_snap_nonetwork
near_features = nonet_proj
location = "LOCATION"
angle = "NO_ANGLE"
method = "GEODESIC"
arcpy.Near_analysis(in_features, near_features, location = location, angle = angle, method = method)

#Create a feature class from those that snapped
arcpy.MakeFeatureLayer_management(notinNHD_gages_snap_nonetwork, "gages_notinNHDv2_snap_nonetwork_lyr")
arcpy.SelectLayerByAttribute_management("gages_notinNHDv2_snap_nonetwork_lyr", "NEW_SELECTION", 'NEAR_DIST < 0.1 ' )
notinNHD_gages_nonetworksnapped = "gages_notinNHDv2_nonetworksnapped"
arcpy.CopyFeatures_management("gages_notinNHDv2_snap_nonetwork_lyr",notinNHD_gages_nonetworksnapped)
#Create a feature class for those that did not snap to non-network
arcpy.SelectLayerByAttribute_management("gages_notinNHDv2_snap_nonetwork_lyr", "NEW_SELECTION", 'NEAR_DIST > 0.1 ' )
notinNHD_gages_nonetwork_notsnapped = "gages_notinNHDv2_nonetwork_notsnapped"
arcpy.CopyFeatures_management("gages_notinNHDv2_snap_nonetwork_lyr",notinNHD_gages_nonetwork_notsnapped)

################################################################################################
# C. MANUALLY JOIN THOSE GAGES THAT ARE NOT IN NHDV2 AND DID NOT AUTOMATICALLY SNAP TO FLOWLINES
################################################################################################
#Check those points that snapped neither to the network nor to the non-network
"F:/Miscellaneous/Hydro_classes/Gages_analysis.gdb/gages_notinNHDv2_nonetwork_notsnapped"

#Add fields to these layers to show which ones were joined, which ones were snapped, and which ones were not snapped
arcpy.AddField_management(notinNHD_gages_nonetwork_notsnapped, 'positioning', "TEXT")
with arcpy.da.UpdateCursor(notinNHD_gages_nonetwork_notsnapped, 'positioning') as cursor:
    for row in cursor:
        print(row)
        row[0] = "notsnapped"
        cursor.updateRow(row)
arcpy.AddField_management(notinNHD_gages_nonetworksnapped, 'positioning', "TEXT")
with arcpy.da.UpdateCursor(notinNHD_gages_nonetworksnapped, 'positioning') as cursor:
    for row in cursor:
        print(row)
        row[0] = "snapped"
        cursor.updateRow(row)
arcpy.AddField_management(notinNHD_gages_networksnapped, 'positioning', "TEXT")
with arcpy.da.UpdateCursor(notinNHD_gages_networksnapped, 'positioning') as cursor:
    for row in cursor:
        print(row)
        row[0] = "snapped"
        cursor.updateRow(row)
arcpy.AddField_management(NHD2gage_join, 'positioning', "TEXT")
with arcpy.da.UpdateCursor(NHD2gage_join, 'positioning') as cursor:
    for row in cursor:
        print(row)
        row[0] = "NHD2join"
        cursor.updateRow(row)

#Merge all layers (did it in Arcmap, crashed repeatedly in Python)
#First make sure that NHDgage_discharge_castformat_join has a site_no field
#-> "Gages_analysis.gdb/allgages_merge"
#Only keep gage ID and source fields

####################
#Need to inspect each gage that did not snap individually and move manually those that should be on a nearby flowline using
# USGS records, satellite imagery, etc.   -> Gages_analysis.gdb/allgages_merge_manual
#WARNING: THIS STEP WAS NOT RE-DONE ON JANUARY 2018, GAGES WERE LINKED TO BASINS AND MAPPED AS IS.

#Join with HUC data (Watershed Boundary Dataset WBD)
allgages = "allgages_merge"
HUC = NHDpath+"WBDSnapshot\HUC12"
allgages_HUC = "allgages_merge_HUCjoin"
arcpy.SpatialJoin_analysis(allgages, HUC, allgages_HUC, match_option="WITHIN")
#13 gages did not join because they fell outside of any HUC, correct them manually
#02246850,04165557,04165710,04216000,04159130,04264331,08375300,09532500,09522200,01103038,04127885,464646092052900,01103040
#Delete Hawaii gages and export attributes to table -> results/gages/allgages_merge_HUCjoin.csv