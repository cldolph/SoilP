# SoilP
This repository contains data files and R script for cleaning and pre-processing soil phopshorus (P) data downloaded from the USGS National Geochemical Survey (https://mrdata.usgs.gov/geochem/) dataset, and the National Soil Characterization dataset (https://ncsslabdatamart.sc.egov.usda.gov/). This soil P data is used together with geospatial attributes compiled from multiple datasets (NLCD, NWI, NHDv2Plus, gSSURGO, StreamCat) for predicting soil P across the Upper Mississippi River Basin (UMRB), using a random forest model. Datasets and scripts include:
1. 'Soil_P_main_script.R' - Complete script for all data cleaning, processing and modelling. This script references several 'modules' (located in the 'Soil_P_input' folder), where individual data processing steps take place. 

All other input files are in the subfolder 'Soil_P_input':

2. 'geochem.csv' - USGS National Geochemical Survey data
3. 'StreamCat_Text Files' (folder) - subset of spatial attributes from the US EPA StreamCat dataset, used in random forest modelling (https://www.epa.gov/national-5. aquatic-resource-surveys/streamcat-dataset)
4.'NCSS_alldepths_MajorElements_since2000_region_NLCD06_NHDV2100_NWI_ssurgo_catchment.txt' - soil P data from the NCSS dataset, joined to primary attribute keys from the NLCD, NHDv2100, NWI, gSSURGO and StreamCat datasets
5. 'USGS_alldepths_soil_since2000_region_NWI_NHDv2100_NLCD06_ssurgo_catchment.txt'- soil P data from the USGS NGS dataset, joined to primary attribute keys from the NLCD, NHDv2100, NWI, gSSURGO and StreamCat datasets
6. 'soilP_ras' - Raster file containing predicted soil P (in mg/kg) at the 100m grid scale across the UMRB

*In addition to these files, you will also need to download the requisite gSSURGO files (listed in module 2) from https://nrcs.app.box.com/v/soils/folder/148414960239

**Note: The above files (numbers 1-6) are inputs needed to sucessfully run the R script, process the data and generate the soil P predictive model. 


Methods for this project are described in detail in Dolph et al. (in prep). 
