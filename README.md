# SoilP
This repository contains data files and R script for cleaning and pre-processing soil phopshorus (P) data downloaded from the USGS National Geochemical Survey (https://mrdata.usgs.gov/geochem/) dataset, and the National Soil Characterization dataset (https://ncsslabdatamart.sc.egov.usda.gov/). This soil P data is used together with geospatial attributes compiled from multiple datasets (NLCD, NWI, NHDv2Plus, gSSURGO, StreamCat) for predicting soil P across the Upper Mississippi River Basin (UMRB), using a random forest model. Datasets include:
1. 'Complete_Soil_P_Script.R'
2. 'geochem.csv' - USGS National Geochemical Survey data
3. '37TABLES_StreamCat_variables_for_SOIL_P.csv - subset of spatial attributes from the US EPA StreamCat dataset, used in random forest modelling (https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset)
4.  'NCSS_alldepths_MajorElements_since2000_region_NLCD06_NHDV2100_NWI_ssurgo_catchment.txt' - soil P data from the NCSS dataset, joined to primary attribute keys from the NLCD, NHDv2100, NWI, gSSURGO and StreamCat datasets
5.   'USGS_alldepths_soil_since2000_region_NWI_NHDv2100_NLCD06_ssurgo_catchment.txt'- soil P data from the USGS NGS dataset, joined to primary attribute keys from the NLCD, NHDv2100, NWI, gSSURGO and StreamCat datasets
6. Other tables are intermediate data outputs from the R script used in later processing/modelling
