# STEP 4: LOAD IN SOIL P DATA w/SPATIAL ATTRIBUTES #
#& MERGE WITH SSURGO & STREAMCAT ATTRIBUTES #

#Soil P datasets:
#USGS National Geochem Survey
#Nat'l Soil Characterization Database

#soil P datasets have had the following pre-processing:
#samples w/P data available
#subsetted to soil samples
#surface samples <12 inch depth (<31 cm), i.e., 'plowable layer'
#samples collected since 2000
#attributes merged in GIS: 
#NHD stream type from NHDv2Plus, if within 100 m buffer of stream network
#NWI wetland type, if within wetland polygon
#NLCD 2006 Land Use category in 30m cell where sample occurs
#SSURGO mutkey ID (for merging with ssurgo attributes - see below)
#NHDv2Plus Catchment COMID (for merging with StreamCat attributes - see below)

#USGS NATIONAL GEOCHEMICAL SURVEY DATA
soilP<-read.delim('./USGS_alldepths_soil_since2000_region_NWI_NHDv2100_NLCD06_ssurgo_catchment.txt', sep=",", header=TRUE)
head(soilP)

#check date range
soilP<-soilP %>% 
  mutate(DATE=as.Date(as.character(soilP$COLL_DATE), format="%Y%m%d"))
summary(soilP$DATE)
#all samples collected since 2000
nrow(soilP) #n=6664

#Check unique sample locations
nrow(unique(soilP[c("LATITUDE","LONGITUDE")])) #n=3245 unique locations

#NATL SOIL CHARACTERIZATION DATABASE
#LOAD IN NCSS TP (from MAJOR ELEMENTS TABLE)
ncss.TP<-read.delim('./NCSS_alldepths_MajorElements_since2000_region_NLCD06_NHDv2100_NWI_ssurgo_catchment.txt', sep=",", header=TRUE)
head(ncss.TP)

ncss.TP<-ncss.TP %>% 
  mutate(P_mgkg=p_mjelm) %>% #rename P column
  mutate(DATE=as.Date(as.POSIXct(ncss.TP$Date,format='%m/%d/%Y'))) #format date

summary(ncss.TP$DATE) #check date range
nrow(ncss.TP) #559 records (but note, 1 addt'l record excluded below because of negative value)

#NEED TO HARMONIZE DEPTH INFO ACROSS THE TWO DATASETS;
#make a uniform depth column with same units across two datasets
#Depth will be based on bottom depth of the surface layer
names(soilP)
names(ncss.TP)
#Note NCSS Access database specifies the units of hzn_bot as centimeters
ncss.TP$Depth_cm<-ncss.TP$hzn_bot

#Units of USGS depth are inches; convert to centimeters
soilP$Depth_cm<-soilP$max_depth_est*2.54


#ADD IN NCSS TP DATA TO USGS TP DATA - NEED COLUMNS TO MATCH
#(note checked and for both datasets units for P are mg/kg)
names(ncss.TP)
names(soilP)
nrow(soilP)

#CREATE UNIQUE SAMPLE ID SO CAN LINK TO LAT/LONG LATER FOR MAPPING
#note there could be more than one sample per location, if sampled at multiple depths
soilP$SiteID<-(1:nrow(soilP))
head(soilP)
nrow(soilP)
n<-nrow(soilP)+nrow(ncss.TP)
n
ncss.TP$SiteID<-((nrow(soilP)+1):n)
head(ncss.TP)
nrow(ncss.TP)


#####
#subset ncss TP data to just P column (excluding soil test P measures), and attributes used by soil P
#drop all columns you don't need for both datasets
#NOTE: you need FEATUREID for catchment ID, to link soil P to streamcat attributes
#Keep SiteID for any future mapping
#(which is the same as COMID, so will need to make a new column to join datasets below by COMID)

names(ncss.TP)
names(soilP)

#Attributes to keep:
Attributes.Keep<-list('SiteID', 'P_mgkg','COMID', 'MUTKEY', 'FTYPE', 'FCODE', 'WETLAND_TYPE', 
                      'NLCD06', 'FEATUREID', 'DATE', 'Depth_cm')

soilP2<-soilP[,names(soilP) %in% Attributes.Keep]
names(soilP2)
ncss.TP.2<-ncss.TP[,names(ncss.TP) %in% Attributes.Keep]
names(ncss.TP.2)
#put columns in same order for ncss dataset:
order <- c("P_mgkg", "WETLAND_TYPE", "COMID", "FTYPE", "FCODE", "NLCD06", "MUTKEY", 
           "FEATUREID", "DATE", "Depth_cm", "SiteID")
ncss.TP.2<-ncss.TP.2[order]
names(ncss.TP.2)

#BIND BOTH P DATASETS TOGETHER
soilP.all<-rbind(soilP2, ncss.TP.2)
nrow(soilP.all)
#N=7223


###########################################################
##ADDITIONAL PREDICTOR VARIABLES

#load in additional SSURGO (soil) attributes
setwd(output_dir)
ssurgo<-read.delim('./SSURGO_attributes_unique_for_mapunit_CEAP_regionalextent.csv', sep=",", header=TRUE)
head(ssurgo)



###############################################################

##MERGE SOIL P DATA TO PREDICTOR VARIABLES


#For USGS GEOCHEM DATA + NCSS TP DATA:
soilP.att<-merge(soilP.all, ssurgo, by=c('MUTKEY'))
head(soilP.att)
names(soilP.att)
nrow(soilP.att)

#note - losing 340 records when merge to ssurgo data; 
#these are samples where the top hzdepb_r attribute in the SSURGO Chorizon table was too low
#(i.e, below 30 cm)
#(see notes in module2, line 65)
#N=6883 for all samples

####
#Create Levels for Categorical Attribute data

#set NA to 0 for FCODE
soilP.att$FCODE[is.na(soilP.att$FCODE)] <- 0
head(soilP.att)

#Create levels for streams layer;
#levels are: 
soilP.att$StreamType<-factor(soilP.att$FCODE, levels=c('0', '33600', '46003', '46006', '55800', '56600'),
                             labels=c('NonStream', 'Canal/Ditch', 'Intermittent_Stream', 'Perennial_Stream', 'Artificial_Path', 'Coastline'))


#Create categorical variable for NWI categories
names(soilP.att)
levels(factor(soilP.att$WETLAND_TYPE))
soilP.att$WetlandType<-factor(soilP.att$WETLAND_TYPE, levels=c('', 'Freshwater Emergent Wetland', 
                                                               'Freshwater Forested/Shrub Wetland','Freshwater Pond', 'Lake', 'Riverine'),
                              labels=c('NonWetland','Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland',
                                       'Freshwater Pond', 'Lake', 'Riverine' ))
head(soilP.att)
levels(soilP.att$WetlandType)


#NLCD 2006 Land Use category - note this category has some overlap with NWI
#check number of levels
levels(factor(soilP.att$NLCD06))
#15 levels should be OK
#create categorical variable
#0 - 11 - Open Water
#1 - 21 - Developed, Open Space
#2 - 22 - Developed, Low Intensity
#3 - 23 - Developed, Medium Intensity
#4 - 24 - Developed, High Intensity
#5 - 31 - Barren Land
#6 - 41 - Deciduous Forest
#7 - 42 - Evergreen Forest
#8 - 43 - Mixed Forest
#9 - 52 - Shrub/Scrub
#10 - 71 - Grassland/Herbaceous
#11 - 81 - Pasture/Hay
#12 - 82 - Cultivated Crops
#13 - 90 - Woody Wetlands
#14 - 95 - Emergent Herbaceous Wetlands
soilP.att$NLCD06Cat<-factor(soilP.att$NLCD06, levels=c('11','21','22','23','24','31','41','42','43','52',
                                                       '71','81','82','90','95'),
                            labels=c('Open Water', 'Developed, Open Space', 'Developed, Low Intensity',
                                     'Developed, Medium Intensity', 'Developed, High Intensity', 'Barren Land',
                                     'Deciduous Forest','Evergreen Forest','Mixed Forest','Shrub/Scrub','Grassland/Herbaceous',
                                     'Pasture/Hay','Cultivated Crops','Woody Wetlands','Emergent Herbaious Wetlands'))
head(soilP.att)



#Note: some soil P sites do have same mutkey, because they are located in the same map unit!
#So don't want to exclude these sites;


######################################################
#SUBSET COLUMNS to bind to StreamCat dataset:
names(soilP.att)
head(soilP.att)

Numeric.list<-c('P_mgkg',
                'p_ox', 'p_bray1', 'pbray2', 'p_olsn', 'p_nz', 'p_h2o', 'p_citx', 'p_mehlich3', 'n_no3',
                'slope_r', 'slopelenusle_r', 'elev_r', 
                'airtempa_r', 'reannualprecip_r', 'ffd_r','sieveno4_r', 'sieveno10_r', 'sieveno40_r', 'sieveno200_r', 
                'sandtotal_r', 'silttotal_r', 'claytotal_r', 'claysizedcarb_r', 'om_r', 'partdensity',
                'ksat_r', 'kffact', 'caco3_r', 'gypsum_r',
                'aws0_5', 'aws5_20', 'aws20_50', 'aws50_100', 'aws100_150', 'aws150_999', 'aws0_20', 'aws0_30',
                'aws0_100', 'aws0_150', 'aws0_999','soc0_5' , 'soc5_20' ,'soc20_50'                   
                , 'soc50_100' ,'soc100_150', 'soc150_999' ,'soc0_20'                    
                , 'soc0_30' ,'soc0_100' , 'soc0_150','soc0_999'                                                 
                , 'nccpi3corn' ,'nccpi3soy' , 'nccpi3cot','nccpi3sg'                   
                , 'nccpi3all','pctearthmc' , 'rootznemc','rootznaws'                  
                , 'droughty','pwsl1pomu', 'Depth_cm')

Categorical.list<-c('mukey', 'comppct_r', 'runoff', 'erocl', 'hydricon', 'hydricrating', 'drainagecl', 'taxpartsize',
                    'taxceactcl', 'taxreaction', 'texcl', 'pmkind', 'Wetland_Attribute', 'StreamType',
                    'WetlandType', 'NLCD06Cat')

Admin.list<-c('SiteID', 'mukey', 'site_key', 'latitude_decimal_degrees', 'longitude_decimal_degrees', 'state_name', 
              'county_code', 'county_name', 'labsampnum', 
              'hzn_top', 'hzn_bot',
              'hzn_desgn', 'hzn_prime', 'COMID','FEATUREID', 'musumcpct','COMID.x', 'COMID.y')

#musumcpct - the sum of comppct_r values for all listed components in the map unit

Big.List<-c(Numeric.list, Categorical.list, Admin.list)


#Subset data to the attributes specified above:
data<-soilP.att[,names(soilP.att) %in% Big.List]
head(data)
nrow(data)


#DECISION IN MODELLING: LOOK AT ALL DATA POINTS, OR JUST NEAR CHANNEL?
#number of near channel points - 
nrow(data[!data$StreamType=="NonStream",])


#for near channel data only
NCdata<-data[!data$StreamType=="NonStream",]
head(NCdata)
names(NCdata)
nrow(NCdata)

#How many sites in near channel environment have COMID (ie, perennial stream)
#COMID for sites in near channel
NC.COMID<-unique(na.omit(NCdata$COMID))
length(NC.COMID)

#for surface samples, n=175 sites with COMID (to merge with StreamCat variables)
#pretty small N if restrict to near channel sites with attributes

#Remainder of script uses all samples (not just near channel)

################################################
##ADD IN STREAM CAT VARIABLES 
streamcat<-fread('./StreamCat_variables_for_SOIL_P.csv', sep=",", header=TRUE)
head(streamcat)
names(streamcat)

#Make a FEATUREID column equivalent to COMID
streamcat$FEATUREID<-streamcat$COMID

#MERGE STREAMCAT TO ALL SOIL P DATA
All.Att<-merge(data, streamcat, by=c('FEATUREID'))
head(All.Att)
nrow(All.Att)


#################################################################
###EXCLUDE VARIABLES:
#1. With many NAs (threshold at 20%, see below)
#2. Because they have too many levels for random forest
#3. Because they don't contain information (e.g., all zeros)

#exclude covariates without useful information (i.e., contain all zeros):
All.Att2<-All.Att[ , colSums(All.Att != 0, na.rm = TRUE) > 0]
names(All.Att2)

#Exclude Admin variables
All.Att3<-All.Att2[,!names(All.Att2) %in% Admin.list]
names(All.Att3)

## Remove columns with more than 20% NA
#even though you will interpolate for some missing values, still excluding values that are largely missing
#tidyverse solution
All.Att4<-
  All.Att3 %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=20)
names(All.Att4)
#ID removed columns:
names(All.Att3[, which(colMeans(is.na(All.Att3)) > 0.20)])

summary(All.Att4$P_mgkg)
#note negative value for P_mgkg; likely a data entry error
#exclude negative numbers
#note this value is from the NCSS dataset
summary(ncss.TP$P_mgkg)
All.Att4<-filter(All.Att4, P_mgkg>0)
nrow(All.Att4)
summary(All.Att4$P_mgkg)

#check levels for categorical attributes 

#Check levels for factors (to see if they should be collapsed)
#First ID categorical attributes (note: not all classified as factors yet)
All.Att4 %>% 
  select_if(is.character) %>% 
  head(5)
levels(factor(All.Att4$hydricrating)) #3 levels
levels(factor(All.Att4$drainagecl)) #7 levels
levels(factor(All.Att4$taxpartsize)) #19 levels
levels(factor(All.Att4$taxceactcl)) #5 levels
levels(factor(All.Att4$taxreaction)) #5 levels
levels(factor(All.Att4$texcl)) #17 levels
levels(factor(All.Att4$pmkind)) #24 levels 

All.Att4 %>% 
  select_if(is.factor) %>% 
  head(5)

levels(All.Att4$StreamType) # 6 levels
levels(All.Att4$WetlandType) # 6 levels
levels(All.Att4$NLCD06Cat) # 15 levels 

#Write data to file 
setwd(output_dir)
write.table(All.Att4, "./Soil_P_data_for_modelling.csv", sep=",", row.names=FALSE)

