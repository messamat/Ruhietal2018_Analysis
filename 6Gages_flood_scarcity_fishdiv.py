__author__ = 'Mathis Messager'
#Contact info: messamat@uw.edu
#Creation date: April 2017
#Last updated: March 23rd 2018

#Objective: This script analyzes hydrometereological risk and fish biodiversity at the HUC6 watershed scale for the conterminous US
#           A. Compute area-weighted scarcity index for each HUC derived from Devineni et al.'s (2015) NDC index
#               - Join Devenine et al's data to 2010 census shapefile and intersect counties with HUC6 watersheds
#               - Compute historical average precipitation over each HUC6 watershed
#               - Compute HUC6-level NDC
#           B. Compute endemism weighted richness and Threatened and Extinct Species Endemism weighted richness in each HUC6
#               - Average over HUC8 catchments
#           C. Estimate number/percentage of people in each HUC6 that live in a flood zone (note that much of the python
#               processing crashed and was often run in ArcMap instead).
#               - Subset census blocks in raster format with urban land cover data
#               - Intersect census blocks urban areas with FEMA 100-yr flood zone
#               - For census blocks with no urban areas, directly intersect census block with FEMA 100-yr flood zone
#               - Intersect census blocks with HUC6 watersheds
#               See 8Flood_analysis.R for end of flood analysis

import arcpy
import csv
import re
arcpy.CheckOutExtension("Spatial")
arcpy.env.qualifiedFieldNames = False
arcpy.env.overwriteOutput = True

projdir = "F:/gages_project/"
arcpy.env.workspace = projdir+"results/"
HUC6_dat= "gages/Gages_analysis.gdb/HUC6"
HUC8_dat = "fish/HUC8.shp"


########################################################################################################################
#A. Compute area-weighted scarcity index for each HUC derived from Devineni et al.'s (2015) NDC index
########################################################################################################################

#Join Devineni et al.'s (2015) data to census shapefile
#Note: 2015 census shapefile cannot be used to join data as some county names and FIPS have been changed since census 2010
#And Devineni et al. 2015 used 2010 census counties
county = projdir+ "data/flood/Population/gz_2010_us_050_00_5m/gz_2010_us_050_00_5m.shp"
NDImax = projdir+"data/scarcity/Devineni_et_al_2015/NDC_NDImax.csv"
db_scarcity = "water_Scarcity/Gage_analysis_scarcity.gdb"
NDImax_db = db_scarcity + "\NDC_NDImax"

#Generate FIPS for counties to have a common key with Devineni's data
arcpy.AddField_management(county, "FIPS", "TEXT")
with arcpy.da.UpdateCursor(county, ['STATE', "COUNTY", 'FIPS']) as cursor:
    for row in cursor:
        row[2] = row[0] + row[1]
        cursor.updateRow(row)

#Export csv to geodatabase
arcpy.TableToGeodatabase_conversion(NDImax, db_scarcity)

#Convert FIPS to string in NDImax table
arcpy.AddField_management(NDImax_db, "FIPS_str", "TEXT")
with arcpy.da.UpdateCursor(NDImax_db, ['FIPS', "FIPS_str", ""]) as cursor:
    for row in cursor:
        if row[0] < 10000:
            row[1] = "0" + str(row[0])
        else:
            row[1] = str(row[0])
        cursor.updateRow(row)

#NDImax and NDC are strangely formatted, include "NaN" and 095.02E-05 and things of the like, so need to be formatted.
arcpy.AddField_management(NDImax_db, "NDImax_numb", "DOUBLE")
arcpy.AddField_management(NDImax_db, "NDC_numb", "DOUBLE")
with arcpy.da.UpdateCursor(NDImax_db, ['NDImax', "NDImax_numb", "NDC", "NDC_numb"]) as cursor:
    for row in cursor:
        if row[0] == "NaN":
            row[1] = None
        elif re.search('E', row[0]):
            row[1] = 0
        else:
            row[1]=row[0]

        if row[2] == "NaN":
            row[3] = None
        elif re.search('E', row[2]):
            row[3] = 0
        else:
            row[3]=row[2]
        cursor.updateRow(row)


#Join county shapefile to NDImax table
county_scarcity_join = db_scarcity + "\county_NDImax_join"
arcpy.MakeFeatureLayer_management(county, "county_lyr")
arcpy.AddJoin_management(in_layer_or_view="county_lyr", in_field="FIPS", join_table=NDImax_db, join_field="FIPS_str", join_type="KEEP_COMMON")
arcpy.CopyFeatures_management("county_lyr", county_scarcity_join)

#Compute county area properly (using geodesic calculation)
arcpy.AddGeometryAttributes_management(Input_Features=county_scarcity_join, Geometry_Properties="AREA_GEODESIC", Area_Unit= "SQUARE_MILES_US")
arcpy.AlterField_management(in_table=county_scarcity_join, field = "AREA_GEO", new_field_name='COUNTYAREA_GEO')
#Compute intersection of each county with each HUC6
HUC6_countyscarcity_intersect = db_scarcity + "\HUC6_countyscarcity_intersect"
arcpy.Intersect_analysis(in_features = [county_scarcity_join,HUC6_dat], out_feature_class= HUC6_countyscarcity_intersect)

########################################################################################################################
# COMPUTE AVERAGE ANNUAL RAINFALL 1949-2010 FOR EACH WATERSHED

#Downloaded average daily rainfall data for the entire US from 1949 to 2010
# at http://www.engr.scu.edu/~emaurer/gridded_obs/index_gridded_obs.htm

import netCDF4 as nc
import numpy as np
import os
from netCDF4 import *
import glob
import gc

pr1949_0101='water_Scarcity\Precipitation\pr1949'
unzipped_nc = 'water_Scarcity\Precipitation\unzipped_data\\'

#Convert January 1st 1949  from NetCDF to raster layer to get extent and coordinate system
arcpy.MakeNetCDFRasterLayer_md(in_netCDF_file = 'nldas_met_update.obs.daily.pr.1949.nc',
                               variable = 'pr', x_dimension = 'longitude', y_dimension='latitude', out_raster_layer=pr1949_0101)
arcpy.env.outputCoordinateSystem = pr1949_0101
myRaster = arcpy.Raster(pr1949_0101)
mx = myRaster.extent.XMin
my = myRaster.extent.YMin

#Read multifile dataset, read out structure and dimensions
nc1949 = unzipped_nc+'nldas_met_update.obs.daily.pr.1949.nc'
f1949 = nc.Dataset(nc1949)
# print variables
f1949.variables.keys()
aprec1949 = f1949.variables['pr']
print aprec1949
#Try summing across days to get annual rainfall in each pixel
aprec1949_sum = np.sum(aprec1949, axis=0)
#Check shape of output
np.shape(aprec1949_sum)
#Initial layer in raster stack
nc_concat = aprec1949_sum

#Compute average annual rainfall by first summing over every day for each year and then computing average over 1949-2010
#Run into memory error so have to do it in two batches
#Check number of years
len([name for name in glob.glob(unzipped_nc + '\\nldas_met*')])
#Run through the first 31 years
for name in glob.glob(unzipped_nc + '\\nldas_met*')[1:31]:
    #Run through every one of the 31 years except 1949
    if name != nc1949:
        print(name)
        f = nc.Dataset(name)
        aprec = f.variables['pr']
        #For each year, compute the sum over every day of the year to get for total annual rainfall in each pixel
        nc_sum = np.sum(aprec, axis=0)
        #Add the layer to the stack of years, starting with only 1949 in the stack and rewriting the stack for the next iteration
        nc_concat = numpy.dstack((nc_concat, nc_sum))
#Check the dimensions of the stack
np.shape(nc_concat)
#For each pixel, compute the mean annual rainfall across years (axis 0 and 1 are spatial, 2 is temporal)
rainfall_avg_50_80 = np.mean(nc_concat,axis=2)
#Check shape again
np.shape(rainfall_avg_50_80)

#Second batch, create stack with 1981 and then add to stack
for name in glob.glob(unzipped_nc + '\\nldas_met*')[32:]:
        print(name)
        if name == unzipped_nc+'nldas_met_update.obs.daily.pr.1981.nc':
            f = nc.Dataset(name)
            aprec = f.variables['pr']
            nc_concat = np.sum(aprec, axis=0)
        else:
            f = nc.Dataset(name)
            aprec = f.variables['pr']
            nc_sum = np.sum(aprec, axis=0)
            nc_concat = numpy.dstack((nc_concat, nc_sum))

rainfall_avg_81_10 = np.mean(nc_concat,axis=2)
np.shape((rainfall_avg_50_80, rainfall_avg_81_10))

#Compute average annual rainfall across all years (same number of years in each stack)
rainfall_avg = np.mean((rainfall_avg_50_80, rainfall_avg_81_10),axis=0)
#For some reason,the data were flipped spatially along its central parallel, so flip it the other way
rainfall_flip = numpy.flipud(rainfall_avg)
nodatval=np.max(rainfall_avg)
np.shape(rainfall_avg)

#Convert the numpy array to a raster dataset
ras = arcpy.NumPyArrayToRaster(in_array = rainfall_flip,lower_left_corner=arcpy.Point(mx, my), x_cell_size=pr1949_0101, y_cell_size=pr1949_0101,value_to_nodata=double(nodatval))
rainfall_avg_ras = 'water_Scarcity\Precipitation\\avgrainfall'
ras.save(rainfall_avg_ras)

#Check whether county average precipitation from Devineni corresponds to our estimation
arcpy.sa.ZonalStatisticsAsTable(in_zone_data=county, zone_field="FIPS", in_value_raster=rainfall_avg_ras,
                                out_table='water_Scarcity\Precipitation\\county_AP.dbf',
                                ignore_nodata="DATA", statistics_type="MEAN")

########################################################################################################################
#Compute area of intersection of county and HUCs (unit in square miles because census blocks are in square miles)
arcpy.AddGeometryAttributes_management(Input_Features=HUC6_countyscarcity_intersect, Geometry_Properties="AREA_GEODESIC", Area_Unit= "SQUARE_MILES_US")

#Compute SIC of each census block portion
[f.name for f in arcpy.ListFields(HUC4_countyscarcity_intersect)]
#For each HUC6
arcpy.AddField_management(in_table=HUC6_countyscarcity_intersect,field_name='SICsub',field_type='DOUBLE')
with arcpy.da.UpdateCursor(HUC6_countyscarcity_intersect, ['NDC_numb', "NDImax_numb", "AVR_RAINFALL",'AREA_GEO','COUNTYAREA_GEO','SICsub']) as cursor:
    for row in cursor:
        if row[0] is not None:
            #SICcub=(NDC*Average Precipitation*County_Area)* Subcounty_Area/County Area
            row[5]=(row[0]*row[2]*row[4])*row[3]/row[4]
        else:
            row[5]=None
        cursor.updateRow(row)

#Dissolve by HUC, summing the SIC of each census block portion within the watershed
HUC6_SIC = db_scarcity + "\HUC6_SIC"
arcpy.Dissolve_management(in_features=HUC6_countyscarcity_intersect, out_feature_class=HUC6_SIC, dissolve_field=["HUC6"], statistics_fields=[['AREA_GEO', 'SUM'],['SICsub', 'SUM']])

#Calculate average annual rainfall in each HUC
arcpy.sa.ZonalStatisticsAsTable(in_zone_data=HUC6_SIC, zone_field="HUC6", in_value_raster=rainfall_avg_ras,
                                out_table='water_Scarcity\Precipitation\\HUC6_AP.dbf',
                                ignore_nodata="DATA", statistics_type="MEAN")

#Join rainfall statistics to HUC shapefile
HUC6_SIC_pr = db_scarcity + "\HUC6_SIC_pr"
arcpy.MakeFeatureLayer_management(HUC4_SIC, "HUC6_SIC_lyr")
arcpy.AddJoin_management(in_layer_or_view="HUC6_SIC_lyr", in_field="HUC6",
                         join_table='water_Scarcity\Precipitation\\HUC6_AP.dbf', join_field="HUC6", join_type="KEEP_COMMON")
arcpy.CopyFeatures_management("HUC6_SIC_lyr", HUC6_SIC_pr)

#Compute HUC geodesic area
arcpy.AddGeometryAttributes_management(Input_Features=HUC6_SIC_pr, Geometry_Properties="AREA_GEODESIC", Area_Unit= "SQUARE_MILES_US")

#Compute HUC NDC
arcpy.AddField_management(in_table=HUC4_SIC_pr,field_name='NDC_HUC',field_type='DOUBLE')
with arcpy.da.UpdateCursor(HUC4_SIC_pr, ["SUM_SICsub",'MEAN','AREA_GEO','NDC_HUC']) as cursor:
    for row in cursor:
        row[3]=row[0]/(row[1]*row[2])
        cursor.updateRow(row)
arcpy.AddField_management(in_table=HUC6_SIC_pr,field_name='NDC_HUC',field_type='DOUBLE')
with arcpy.da.UpdateCursor(HUC6_SIC_pr, ["SUM_SICsub",'MEAN','AREA_GEO','NDC_HUC']) as cursor:
    for row in cursor:
        row[3]=row[0]/(row[1]*row[2])
        cursor.updateRow(row)

#Export table as csv in arcmap
########################################################################################################################

########################################################################################################################
#B. Compute endemism weighted richness and Threatened and Extinct Species Endemism weighted richness in each HUC ##
########################################################################################################################
fishdiv = projdir+ "data/fish/FishDiversityMetrics.csv"
db = "fish/Gage_analysis_fish.gdb/"
fishdiv_db = db+"FishDiversityMetrics"

#Export csv to geodatabase
arcpy.TableToGeodatabase_conversion(fishdiv, db)
#Add new HUC8 ID field
arcpy.AddField_management(fishdiv_db, "HUC8_id", "TEXT")

#Fill in 0s before HUC8 IDs of fish div data
with arcpy.da.UpdateCursor(fishdiv_db, ['HUC8', 'HUC8_id']) as cursor:
    for row in cursor:
        if row[0] < 10000000:
            row[1] = "0" + str(row[0])
        else:
            row[1] = str(row[0])
        cursor.updateRow(row)

#Join HUC8 shapefile with fish biodiv data
HUC8div_join = db+"HUC8_fishdiv_join"
arcpy.MakeFeatureLayer_management(HUC8_dat, "HUC8_lyr")
arcpy.AddJoin_management(in_layer_or_view="HUC8_lyr", in_field="HUC_8", join_table=fishdiv_db, join_field="HUC8_id")
arcpy.CopyFeatures_management("HUC8_lyr", HUC8div_join)

#Add HUC6 field to layer in order to dissolve
arcpy.AddField_management(HUC8div_join, "HUC6_id", "TEXT")
[f.name for f in arcpy.ListFields(HUC8div_join)]
with arcpy.da.UpdateCursor(HUC8div_join, ['HUC_8', 'HUC6_id']) as cursor:
    for row in cursor:
        row[1] = row[0][0:6]
        cursor.updateRow(row)

#Dissolve HUC6 levels while averaging the endemism weighted richness
HUC6div = db+"HUC6_fishdiv"
[f.name for f in arcpy.ListFields(HUC8div_join)]
arcpy.AddField_management(HUC8div_join, "TE_EWU_numb", "DOUBLE")
with arcpy.da.UpdateCursor(HUC8div_join, ['TE_EWU', "TE_EWU_numb"]) as cursor:
    for row in cursor:
        row[1] = row[0]
        cursor.updateRow(row)
arcpy.Dissolve_management(in_features=HUC8div_join, out_feature_class=HUC6div, dissolve_field=["HUC6_id"], statistics_fields=[['TotArea_x', 'SUM'],['EWU', 'MEAN'],
                                                                                                                              ['TE_EWU_numb', 'MEAN'],['TE_Count', 'MEAN']])

#Export to table
HUC6_tab = "fish\HUC6div.csv"
[f.name for f in arcpy.ListFields(HUC6div)]
with open(HUC6_tab, "wb") as csv_file:
    writer = csv.writer(csv_file)
    #Write headers
    writer.writerow(['HUC6_id','TotArea_x', 'EWU', "TE_EWU_numb", 'TE_Count'])
    with arcpy.da.SearchCursor(HUC6div, ['HUC6_id','SUM_TotArea_x', 'MEAN_EWU', "MEAN_TE_EWU_numb", 'MEAN_TE_Count']) as cursor:
        for row in cursor:
            writer.writerow(row)

########################################################################################################################
#C. Estimate number/percentage of people in each HUC6 that live in a flood zone
########################################################################################################################
flood_db = "flood/Flood_analysis.gdb/"
pop_dat= flood_db + "Censusblock_US_merge"
ZoneA_dat= flood_db + "S_Fld_Haz_Ar_ZoneA"
LCD2011 = "F:\Data\\nlcd_2011_landcover_2011_edition_2014_10_10\\nlcd_2011_landcover_2011_edition_2014_10_10\\nlcd_2011_landcover_2011_edition_2014_10_10.img"

#Merge census blocks
#In Arcmap, merge the census blocks of the 48 conterminous state into one dataset -> "F:\Miscellaneous\Hydro_classes\Analysis\Flood\Flood_analysis.gdb\Censusblock_US_merge"
#Repair geometry of both flood zone data and popdat (a lot of self-intersections in the polygons)

#Compute area of each census block
arcpy.AddGeometryAttributes_management(Input_Features=pop_dat, Geometry_Properties="AREA_GEODESIC", Area_Unit= "SQUARE_KILOMETERS")
arcpy.AlterField_management(in_table=pop_dat, field = "AREA_GEO", new_field_name='COUNTYAREA_GEO')

#Convert LCD2011 original dataset to GRID format
arcpy.CopyRaster_management(LCD2011, flood_db+"lcd2011", pixel_type="8_BIT_UNSIGNED")

#Reclassify LCD2011 to have all urbanized pixels as 1 and all other pixels as NoData (could not get to run in Python)
lcd2011_reclass = arcpy.sa.Reclassify(in_raster=LCD2011, reclass_field="VALUE", remap= arcpy.sa.RemapRange([[21,24,1],[0,20,0],[25,100,0]]), missing_values="DATA")
lcd2011_reclass.save(flood_db + "lcd2011_reclass")

#Compute intersection of each census block with each 100-yr flood zone
censusflood_inters = flood_db + "censusflood_intersect"
arcpy.Intersect_analysis(in_features = [pop_dat,ZoneA_dat], out_feature_class= censusflood_inters)

#Lots of issues with these. Zonal statistics didn't work for most of these so had to do analysis in raster format on several different computers
#Easier to troubleshoot in Arcmap, so did most of the following analysis in Arcmap.

#Convert census blocks to raster (with lcd2011_reclass extent, field = OBJECTID, output_cell size = 30m) -> censusblock_ras
#Raster calculator: Con("lcd2011_reclass" == 1, "censusblock_ras", 0) -> output table to get the number of pixels in each census block that are urban -> censusblock_lcd_inters

#Convert censusflood_intersect to raster (with lcd2011_reclass extent, field = OBJECTID, output_cell size = 30m) -> censusflood_inters_ras_OBJECTID
#With raster calculator: Con(("censusflood_inters_ras" > 1) & ("lcd2011_reclass" == 1), "censusflood_inters_ras", 0) -> censusflood_urban (same extent as previous layer)

#Rename census block area to AREA_GEOBLOCK
#Add area of intersection between census block and flood zone to censusflood_intersect_2_proj table -> AREA_INTERS
censusflood_intersect_2_proj = flood_db+"censusflood_intersect_2_proj"
arcpy.AddGeometryAttributes_management(Input_Features=censusflood_intersect_2_proj, Geometry_Properties="AREA_GEODESIC", Area_Unit= "SQUARE_KILOMETERS")
#Export table -> censusflood_intersect_2.dbf

#Project FEMA data to Albers Conical Equal Area -> S_Fled_Haz_Ar_proj
#Convert FEMA data to raster -> S_Fld_Haz_Ar_ras
#With raster calculator:  Con((S_Fld_Haz_Ar_ras == 1) & (LCD2011_reclass == 1), censusblock, 0)  -> censusFEMAdat_lcd_inters
#Build raster attribute table and export table -> censusFEMAdat_lcd_inters_tab.dbf

#Select those census blocks with a population but no urban pixel that are not in Hawaii -> censusblock_nourban
#Intersect with FEMA zone -> censusnourban_FEMA_intersect
#Rename census block area to AREA_GEOBLOCK
#Add area of intersection between census block and FEMA data to AREA_INTERS

#Intersect census blocks with HUC6 -> Censusblock_HUC6_inters

#Compute area
censushuc6 = flood_db+"Censusblock_HUC6_inters"
arcpy.AddGeometryAttributes_management(Input_Features=censushuc6, Geometry_Properties="AREA_GEODESIC", Area_Unit= "SQUARE_KILOMETERS")