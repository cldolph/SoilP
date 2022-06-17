
#Cleaning, pre-processing and random forest modelling for predictive soil P model
#updated 5/23/22

##################
#set working directory to folder where you have saved the 'SoilP' folder
#change this on your system as needed
setwd('C:/Users/dolph/OneDrive/Documents/CEAP Project/Soil P Manuscript/SoilP')

#######################################################################################
#######################################################################################
#STEP 1: CLEANING RAW SOIL P DATA (previous separate script: 'soil_P_database_script.R'
#Soil P Data sources: 
#1. USGS National Geochemical Survey
#2. National Soil Characterization Dataset

#read in USGS geochem dataset
#from https://mrdata.usgs.gov/geochem/

geochem<-read.delim('./geochem.csv', sep=",", header=TRUE)
head(geochem)
names(geochem)
nrow(geochem)

#check SOURCE types - i.e., the geological source of the sample, as per
#https://mrdata.usgs.gov/geochem/about.php

levels(factor(geochem$SOURCE))

#subset to soil samples (excluding stream bed samples)
soil<-geochem[geochem$SOURCE=="Soil"|geochem$SOURCE=="Glacial till",]
#check just soil samples
nrow(geochem[geochem$SOURCE=="Soil",])

nrow(soil)
#n=20,261

#restrict to neccessary columns, remove blank rows; 
names(soil)
#remove NAs from Lat/Long
soil2<-soil[!is.na(soil$LATITUDE),]
nrow(soil2)
#remove unneeded columns
#need to retain sample date 
soil3<-soil2[,c(1,6,12,15,17, 29,33,37,38,43,46,48,65,72,88,227,228)]
head(soil3)

#convert date to Date format (need to convert integers to character first)
soil3$COLL_DATE<-as.character(soil3$COLL_DATE)
soil3$Date<-as.Date(soil3$COLL_DATE, format = "%Y%m%d")
head(soil3)

#remove rows with no P data
nrow(soil3)
soil4<-soil3[!is.na(soil3$P_ICP40),]
nrow(soil4) #n=19574

#ID date range of P dataset
summary(soil4$Date)
#latest year listed as 2303 - data entry error
#median date is 2003
#remove rows with no Date available
#remove rows with 2303 listed as the year (assume error)
nrow(soil4)
soil5<-soil4[!is.na(soil4$Date) & !soil4$Date>"2022-01-01",]
nrow(soil5)
summary(soil5$Date)

#goal is to model recent soil P content - 
#subset to data collected after 2000
#check how many records
nrow(soil5)
nrow(soil5[soil5$Date>"2000-01-01",])
#note that you pick up ~1000 more samples if you date back to 1990
nrow(soil5[soil5$Date>"1990-01-01",])
#Date to 2000 and later:
soil5<-soil5[soil5$Date>"2000-01-01",]
nrow(soil5) #n=9864

#convert P units from % weight to mg/kg
#multiple P_ICP40 (P in % wt) by 10,000, 
soil5$P_mgkg<-soil5$P_ICP40*10000
head(soil5)

##CLEAN UP SOIL DEPTH INFO
#note that P below the surface layer may not be representative of farming practices (as per Ringeval et al 2017)
#they define 'surface' layer as 0-0.3 meters (equivalent to ~ 30cm, or 11.8 inches)

#SOIL_HORIZ gives approx depth
#but note that some samples have depth in inches, and some just have horizon ID (A, B, etc)
#**NOTE that some of the samples are repeat samples at different depths for a given location; 
#want ability to subset to surface samples
#the format of the SOIL_HORIZ column is a mess - all different formats
# some exact depths, some ranges, some just qualitative categories (e.g., A or B), and some blank

levels(factor(soil5$SOIL_HORIZ))

#Cleaning up soil depth info
nrow(soil5)
nrow(soil5[is.na(soil5$SOIL_HORIZ),]) #check for NAs; n=0
nrow(soil5[soil5$SOIL_HORIZ=="",]) #check for blanks/missing values; n=436

#how many different categories
levels(factor(soil5$SOIL_HORIZ))
#631 different categories; 

#first take out "in" from all columns (i.e., indicating measurement in inches)
soil5$Depth<-soil5$SOIL_HORIZ
#remove "in"
soil5$Depth<- gsub('in',"",soil5$Depth)
levels(factor(soil5$Depth))
#substite "-" for "to" to make uniform across rows
soil5$Depth<- gsub(' to ', "-", soil5$Depth)
levels(factor(soil5$Depth))

#for soil horizons with letters (rather than numeric depth measurements):
#A few things to note: 
#some samples apper to be a mix of multiple horizons; e.g., "A & B/E" 
#some have depth measurements and some do not
#retain those with depth measurements and use those depths 


#Pull out all values that include actual depth measurements first:

soil5$Depth<- gsub('A, ', "", soil5$Depth)
levels(factor(soil5$Depth))
#remove Ap values with depth measurements so you can just apply numbers:
soil5$Depth<- gsub('Ap, ', "", soil5$Depth)
levels(factor(soil5$Depth))
#remove Abk values with depth measurements so you can just apply numbers:
soil5$Depth<- gsub('Abk, ', "", soil5$Depth)
levels(factor(soil5$Depth))
#remove Ak values with depth measurements so you can just apply numbers:
soil5$Depth<- gsub('Ak, ', "", soil5$Depth)
levels(factor(soil5$Depth))
#remove Akp values with depth measurements so you can just apply numbers:
soil5$Depth<- gsub('Akp, ', "", soil5$Depth)
levels(factor(soil5$Depth))
#remove Apk values with depth measurements so you can just apply numbers:
soil5$Depth<- gsub('Apk, ', "", soil5$Depth)
levels(factor(soil5$Depth))
#Other horizon categories with depth info associated:
soil5$Depth<- gsub('A & ABk, ', "", soil5$Depth)
soil5$Depth<- gsub('A & ABk1, ', "", soil5$Depth)
soil5$Depth<- gsub('A & AC, ', "", soil5$Depth)
soil5$Depth<- gsub('A & Bk1, ', "", soil5$Depth)
soil5$Depth<-gsub('A & Bt, ', "", soil5$Depth)
soil5$Depth<- gsub('A & Bw, ', "", soil5$Depth)
soil5$Depth<- gsub('A & Bw1, ', "", soil5$Depth)
soil5$Depth<- gsub('A & C, ', "", soil5$Depth)
soil5$Depth<- gsub('Ap & Bw, ', "", soil5$Depth)
soil5$Depth<- gsub('BCk, ', "", soil5$Depth)
soil5$Depth<- gsub('Bg,, ', "", soil5$Depth)
soil5$Depth<- gsub('Bk, ', "", soil5$Depth)
soil5$Depth<- gsub('Bk1, ', "", soil5$Depth)
soil5$Depth<- gsub('Bk2, ', "", soil5$Depth)
soil5$Depth<- gsub('Bkg, ', "", soil5$Depth)
soil5$Depth<- gsub('Bt, ', "", soil5$Depth)
soil5$Depth<- gsub('Bt1, ', "", soil5$Depth)
soil5$Depth<- gsub('Btk, ', "", soil5$Depth)
soil5$Depth<- gsub('Btn, ', "", soil5$Depth)
soil5$Depth<- gsub('Btnk, ', "", soil5$Depth)
soil5$Depth<- gsub('Btnk2, ', "", soil5$Depth)
soil5$Depth<- gsub('Btnky, ', "", soil5$Depth)
soil5$Depth<- gsub('Bw, ', "", soil5$Depth)
soil5$Depth<- gsub('Bw1, ', "", soil5$Depth)
soil5$Depth<- gsub('Bw2, ', "", soil5$Depth)
soil5$Depth<- gsub('Bwg, ', "", soil5$Depth)
soil5$Depth<- gsub('C, ', "", soil5$Depth)
soil5$Depth<- gsub('C1, ', "", soil5$Depth)
soil5$Depth<- gsub('C3, ', "", soil5$Depth)
soil5$Depth<- gsub('Cr, ', "", soil5$Depth)
soil5$Depth<- gsub('E & Bk1, ', "", soil5$Depth)
soil5$Depth<- gsub('E &Btny, ', "", soil5$Depth)
soil5$Depth<- gsub('E, ', "", soil5$Depth)
soil5$Depth<- gsub('2Bky, ', "", soil5$Depth)
soil5$Depth<- gsub('Bg, ', "", soil5$Depth)


#Check remaining levels:
levels(factor(soil5$Depth))

##Other samples without numeric depth info:

##assume any categories without depth & containing A are 'surface'
#(tho note some appear to be a mix of surface and subsurface)
soil5$Depth <- ifelse(grepl("A",soil5$Depth),'surface',soil5$Depth)
levels(factor(soil5$Depth))

#assign all rows without depth containing primarily "B" or "C" or "E" to 'subsurface'
soil5$Depth <- ifelse(grepl("B",soil5$Depth),'subsurface',soil5$Depth)
levels(factor(soil5$Depth))
soil5$Depth <- ifelse(grepl("C",soil5$Depth),'subsurface',soil5$Depth)
levels(factor(soil5$Depth))
soil5$Depth <- ifelse(grepl("E",soil5$Depth),'subsurface',soil5$Depth)
levels(factor(soil5$Depth))


#Other random categories to flag because numeric depth is unclear:
soil5$Depth <- ifelse(grepl("MIX",soil5$Depth),'unknown depth',soil5$Depth)
levels(factor(soil5$Depth))
soil5$Depth <- ifelse(grepl("None",soil5$Depth),'unknown depth',soil5$Depth)
levels(factor(soil5$Depth))
soil5$Depth <- ifelse(grepl("S",soil5$Depth),'unknown depth',soil5$Depth)
levels(factor(soil5$Depth)) 

#Additional clean up:
#replace commas with "-"
soil5$Depth<- gsub(',', "-", soil5$Depth)
#remove "+" symbol
soil5$Depth<- gsub('\\+', "", soil5$Depth)
#remove random characters:
soil5$Depth<- gsub('i', "", soil5$Depth)
#replace blank spaces with no space:
soil5$Depth<- gsub(' ', "", soil5$Depth)
levels(factor(soil5$Depth))


#Now remaining values are all numbers, except for the categories 'surface', 'subsurface', and 'unknown depth', and the initially missing (blank) values - 
#how many rows for each of those categories?
nrow(soil5)
nrow(soil5[soil5$Depth=="unknown depth"|soil5$Depth=="surface"|soil5$Depth=="subsurface",]) #n=2572
levels(factor(soil5$Depth))
nrow(soil5[soil5$Depth=="surface",]) #n=1320
levels(factor(soil5$Depth))
#how many blank rows 
nrow(soil5[soil5$Depth=="",]) #n=449 #pickup 13 additional blank rows


levels(factor(soil5$Depth))

#Make a column with 'NA' assigned to subsurface/surface/unknown (to be reassigned below)
soil5$surface<- gsub('unknowndepth', "NA", soil5$Depth)
soil5$surface<- gsub('subsurface', "NA", soil5$surface)
soil5$surface<- gsub('surface', "NA", soil5$surface)
levels(factor(soil5$surface))

#For samples that give a range for depth, create min and max depth columns
library(stringr)
range<-as.data.frame(str_split_fixed(soil5$surface, "-", 2))
range
head(range)
colnames(range)[1]<-"min_depth"
colnames(range)[2]<-"max_depth"
head(range)

soil6<-as.data.frame(cbind(soil5, range))
head(soil6)

#Assign depths as numeric:
soil6$min_depth<-as.numeric(soil6$min_depth)
soil6$max_depth<-as.numeric(soil6$max_depth)
head(soil6)
nrow(soil6)
#check number of NAs 
nrow(soil6[is.na(soil6$min_depth),])

#range of soil depths
summary(soil6$max_depth)


##ADDING IN MISSING SOIL DEPTH VALUES:
##Interpolating soil depth values where no specific depth was listed originally (for keeping soil depth as a covariate)
##ASSUMPTIONS:
#Use "max_depth" as the covariate for depth, while also accounting for missing values for depth
#for sites listed as 'surface' with no numeric depth measurement, assign a certain depth - 12 inches for max depth (OR <31 cm)
#for sites with no depth listed, assume surface -> And list assigned surface depth as 12 inches for max depth
#for sites listed as 'subsurface', ie, any horizon except A --> assigned 30 *inches* for max depth #because 99% of subsurface samples 
#that did contain depth information had a maximum depth at or below this level (corresponding primarily to soil horizon B). 
#depth unit for all samples was assumed to be inches in all cases because where units were listed
#in depth column they were "in"


soil.depth<-soil6
nrow(soil6)
#number of samples without depth info specified:
nrow(soil.depth[soil.depth$Depth=="surface",]) #n=1320
nrow(soil.depth[soil.depth$Depth=="unknowndepth",]) #n=37
nrow(soil.depth[soil.depth$Depth=="subsurface",]) #n=1252
nrow(soil.depth[soil.depth$Depth=="",]) #n=449


#for sites with depth listed as 'surface', add 12 to max depth:
soil.depth$max_depth_est<-soil.depth$max_depth
soil.depth$max_depth_est<-ifelse(soil.depth$Depth=="surface", 12, soil.depth$max_depth_est)
levels(factor(soil.depth$max_depth_est))
nrow(soil.depth[is.na(soil.depth$max_depth_est),])

#for sites with depth listed as 'unknowndepth', assume depth is surface, and add 12 to max depth:
soil.depth$max_depth_est<-ifelse(soil.depth$Depth=="unknowndepth", 12, soil.depth$max_depth_est)
nrow(soil.depth[is.na(soil.depth$max_depth_est),])

#for sites with depth listed as 'subsurface', assume max depth is 30 inches:
#why use 30 inches as the max depth for subsurface samples??
#30 inches is typically down to the end of the B horizon - https://homeguides.sfgate.com/what-is-a-soil-profile-13426373.html
#most of the samples with listed subsurface soil horizons listed were B


#what is average depth for samples in subsurface (below 12 inches), that do have depth info
names(soil.depth)
subsurface<-soil.depth[soil.depth$max_depth>12&!is.na(soil.depth$max_depth),]
nrow(subsurface)
summary(subsurface$max_depth)
class(subsurface$max_depth)

#mean depth of all subsurface samples in 20 inches

#max depth of all soil samples listed is 88
#let's plot a histogram
require(ggplot2)
ggplot(subsurface)+
geom_histogram(aes(x=max_depth))

#how many samples have depth >30?
nrow(subsurface[subsurface$max_depth>30,])
#1.6% of subsurface samples have max depth >30 inches; reasonable to include estimate of 30 as max depth for mostly B horizon samples

soil.depth$max_depth_est<-ifelse(soil.depth$Depth=="subsurface", 30, soil.depth$max_depth_est)
nrow(soil.depth[is.na(soil.depth$max_depth_est),])

#for sites with blanks listed under depth, assume depth is surface, and add 12 to max depth:
soil.depth$max_depth_est<-ifelse(soil.depth$Depth=="", 12, soil.depth$max_depth_est)
nrow(soil.depth[is.na(soil.depth$max_depth_est),])

soil.depth$max_depth_est<-ifelse(is.na(soil.depth$Depth), 12, soil.depth$max_depth_est)
nrow(soil.depth[is.na(soil.depth$max_depth_est),]) #why are some rows still NA?
soil.depth[is.na(soil.depth$max_depth_est),] #ok these are rows with only one depth measurement (not a range)
#assign min depth to max depth for these values
soil.depth$max_depth_est<-ifelse(is.na(soil.depth$max_depth_est), soil.depth$min_depth, soil.depth$max_depth_est)
nrow(soil.depth[is.na(soil.depth$max_depth_est),])

levels(factor(soil.depth$max_depth_est))

#Check number of total surface samples
nrow(soil.depth[soil.depth$max_depth_est<=12,]) #n=5731


#Omit Glacial till samples to focus on surface soil 
Soils<-soil.depth[soil.depth$SOURCE=="Soil",]
nrow(Soils)
#Restrict to samples collected since 2000
Soils.2000<-Soils[Soils$Date>"1999-12-31",]
nrow(Soils.2000)


#Write data with assumed depths to table:
write.table(Soils.2000, 'soil_USGSgeochem_since2000_assumed_depths.csv', sep=",", row.names=FALSE)

##########################################################################
##########################################################################

##STEP 2 PRE PROCESSING OF SSURGO ATTRIBUTES 
#Read in ArcGIS dbf attribute tables 

library(dplyr)

#'Component' Table
AR <- sf::st_read(dsn = "./gSSURGO_AR.gdb", layer = "component")
IA <- sf::st_read(dsn = "./gSSURGO_IA.gdb", layer = "component")
IL <- sf::st_read(dsn = "./gSSURGO_IL.gdb", layer = "component")
IN <- sf::st_read(dsn = "./gSSURGO_IN.gdb", layer = "component")
KS <- sf::st_read(dsn = "./gSSURGO_KS.gdb", layer = "component")
MN <- sf::st_read(dsn = "./gSSURGO_MN.gdb", layer = "component")
MO <- sf::st_read(dsn = "./gSSURGO_MO.gdb", layer = "component")
ND <- sf::st_read(dsn = "./gSSURGO_ND.gdb", layer = "component")
NE <- sf::st_read(dsn = "./gSSURGO_NE.gdb", layer = "component")
OH <- sf::st_read(dsn = "./gSSURGO_OH.gdb", layer = "component")
SD <- sf::st_read(dsn = "./gSSURGO_SD.gdb", layer = "component")
WI <- sf::st_read(dsn = "./gSSURGO_WI.gdb", layer = "component")

#Append all tables using rbind
Component<-rbind(AR, IA, IL, IN, KS, MN, MO, ND, NE, OH, SD, WI)
nrow(Component)
head(Component)

#check if cokeys are unique
length(Component$cokey)
length(unique(Component$cokey))
#Ok same length - so are unique

#'Chorizon' Table
AR2 <- sf::st_read(dsn = "./gSSURGO_AR.gdb", layer = "chorizon")
IA2 <- sf::st_read(dsn = "./gSSURGO_IA.gdb", layer = "chorizon")
IL2 <- sf::st_read(dsn = "./gSSURGO_IL.gdb", layer = "chorizon")
IN2 <- sf::st_read(dsn = "./gSSURGO_IN.gdb", layer = "chorizon")
KS2 <- sf::st_read(dsn = "./gSSURGO_KS.gdb", layer = "chorizon")
MN2 <- sf::st_read(dsn = "./gSSURGO_MN.gdb", layer = "chorizon")
MO2 <- sf::st_read(dsn = "./gSSURGO_MO.gdb", layer = "chorizon")
ND2 <- sf::st_read(dsn = "./gSSURGO_ND.gdb", layer = "chorizon")
NE2 <- sf::st_read(dsn = "./gSSURGO_NE.gdb", layer = "chorizon")
OH2 <- sf::st_read(dsn = "./gSSURGO_OH.gdb", layer = "chorizon")
SD2 <- sf::st_read(dsn = "./gSSURGO_SD.gdb", layer = "chorizon")
WI2 <- sf::st_read(dsn = "./gSSURGO_WI.gdb", layer = "chorizon")

#Append all tables using rbind
Chorizon<-rbind(AR2, IA2, IL2, IN2, KS2, MN2, MO2, ND2, NE2, OH2, SD2, WI2)
nrow(Chorizon)
head(Chorizon)
names(Chorizon)

#check for unique cokey
length(Chorizon$cokey)
length(unique(Chorizon$cokey))

#Chorizon does not have unique cokey; 
#Note multiple subcomponents within component layer for Chorizon are multiple soil horizons (at different depths)
#you can select by hzname, and/or by horizon depth (hzdept_r - top depth and hzdepb-r - bottom depth)
#from Table Column report: hzdepb_r - the distance from the top of the soil to the base of the soil horizon
#note: table report notes units as cm

#you need one set of attributes per map unit (mukey), where the attributes are weighted averages
#of the components; so can't have multiple attributes at different depths per cokey
#especially since the depths sampled in different cokeys aren't uniform 


#pick all hzdepthb_r < 31 (ie, in plowable layer) #note: this should be <31 
#corresponding to <12 inches or < 30.48 cm
#assumption here is that ssurgo attributes for P samples that are collected below the surface layer
#will closely mirror those at the top layer
#this is borne out when you examine the soil texture for multiple depths within a component (a cokey);
#texture attributes are typically similar at many of the depths

Chorizon[1:5, c(1,7, 10, 170, 171)]


nrow(Chorizon)
nrow(Chorizon[Chorizon$hzdepb_r<31,])
Chorizon.surface<-Chorizon[Chorizon$hzdepb_r<31,]
head(Chorizon.surface)

#does this still leave you with duplicates per site?
#(ie, multiple depths above 30cm)
length(Chorizon.surface$cokey)
length(unique(Chorizon.surface$cokey))

#are still multiple measures per site; multiple depths

#eliminate samples from the very shallow surface that are likely to be plant material
nrow(Chorizon.surface)
Chorizon.soil<-Chorizon.surface[Chorizon.surface$hzdepb_r >3,]
nrow(Chorizon.soil)

#choose lowermost layer, to avoid component attributes that include plant material at the surface rather than soil
# convert to data.table with key=cokey
library(data.table)
Chorizon.table <- data.table(Chorizon.soil, key="cokey")

#get the subset of data that matches this criterion
Chorizon.bottom<-unique(Chorizon.table[, .SD[hzdepb_r %in% max(hzdepb_r)], by=cokey])
head(Chorizon.bottom)

#check out depths at which samples are collected
summary(Chorizon.bottom$hzdepb_r)
names(Chorizon.bottom)
View(Chorizon.bottom[, c(1,8, 11, 171)])


#check for duplicates
length(Chorizon.bottom$cokey)
length(unique(Chorizon.bottom$cokey))
###
#OK! Now only one record per cokey (the lowermost surface measurement in the top 30cm)
###


#'Chtexture' Table (grain size)
#Can be merged to Chorizon, with Chtexturegrp
AR5 <- sf::st_read(dsn = "./gSSURGO_AR.gdb", layer = "chtexture")
IA5 <- sf::st_read(dsn = "./gSSURGO_IA.gdb", layer = "chtexture")
IL5 <- sf::st_read(dsn = "./gSSURGO_IL.gdb", layer = "chtexture")
IN5 <- sf::st_read(dsn = "./gSSURGO_IN.gdb", layer = "chtexture")
KS5 <- sf::st_read(dsn = "./gSSURGO_KS.gdb", layer = "chtexture")
MN5 <- sf::st_read(dsn = "./gSSURGO_MN.gdb", layer = "chtexture")
MO5 <- sf::st_read(dsn = "./gSSURGO_MO.gdb", layer = "chtexture")
ND5 <- sf::st_read(dsn = "./gSSURGO_ND.gdb", layer = "chtexture")
NE5 <- sf::st_read(dsn = "./gSSURGO_NE.gdb", layer = "chtexture")
OH5 <- sf::st_read(dsn = "./gSSURGO_OH.gdb", layer = "chtexture")
SD5 <- sf::st_read(dsn = "./gSSURGO_SD.gdb", layer = "chtexture")
WI5 <- sf::st_read(dsn = "./gSSURGO_WI.gdb", layer = "chtexture")

#Append all tables using rbind
Chtexture<-rbind(AR5, IA5, IL5, IN5, KS5, MN5, MO5, ND5, NE5, OH5, SD5, WI5)
nrow(Chtexture)
head(Chtexture)
names(Chtexture)

#check for unique subunits
length(Chtexture$chtkey)
length(unique(Chtexture$chtkey))
#Chtkey is unique

#'Chtexturegrp' Table (links chtexture to chorizon table)
#Can be merged to Chorizon, with Chtexturegrp
AR6 <- sf::st_read(dsn = "./gSSURGO_AR.gdb", layer = "chtexturegrp")
IA6 <- sf::st_read(dsn = "./gSSURGO_IA.gdb", layer = "chtexturegrp")
IL6 <- sf::st_read(dsn = "./gSSURGO_IL.gdb", layer = "chtexturegrp")
IN6 <- sf::st_read(dsn = "./gSSURGO_IN.gdb", layer = "chtexturegrp")
KS6 <- sf::st_read(dsn = "./gSSURGO_KS.gdb", layer = "chtexturegrp")
MN6 <- sf::st_read(dsn = "./gSSURGO_MN.gdb", layer = "chtexturegrp")
MO6 <- sf::st_read(dsn = "./gSSURGO_MO.gdb", layer = "chtexturegrp")
ND6 <- sf::st_read(dsn = "./gSSURGO_ND.gdb", layer = "chtexturegrp")
NE6 <- sf::st_read(dsn = "./gSSURGO_NE.gdb", layer = "chtexturegrp")
OH6 <- sf::st_read(dsn = "./gSSURGO_OH.gdb", layer = "chtexturegrp")
SD6 <- sf::st_read(dsn = "./gSSURGO_SD.gdb", layer = "chtexturegrp")
WI6 <- sf::st_read(dsn = "./gSSURGO_WI.gdb", layer = "chtexturegrp")

#Append all tables using rbind
Chtexturegrp<-rbind(AR6, IA6, IL6, IN6, KS6, MN6, MO6, ND6, NE6, OH6, SD6, WI6)
nrow(Chtexturegrp)
head(Chtexturegrp)
names(Chtexturegrp)

Texture<-merge(Chtexture, Chtexturegrp, by=c('chtgkey'))
nrow(Texture)

#check for duplicates at highest level of organization (chkey)
#-->yes, are multiple chkeys; chtkeys appear unique
#can be different textures per horizon
#what are the different subcomponents - see https://sdmdataaccess.nrcs.usda.gov/documents/ReturningSoilTextureRelatedAttributes.pdf

#there is a column 'rvindicator' in chtexturegrp - from Table Column Descriptions:
#'A yes/no field that indicates if a value is representative for the component'
#so select rvindicator = Yes - leaves you with unique value
#this is the 'representative' component

#Join to Texture to Chorizon, uisng chkey
names(Chorizon.bottom)
Horizon<-merge(Chorizon.bottom, Texture[Texture$rvindicator=="Yes",], by=c('chkey'))
head(Horizon)
nrow(Horizon)

#do I have duplicate measurements in Chtexture or Chtexturegrp?? (ie, subcomponets?)
length(Horizon$cokey)
length(unique(Horizon$cokey))
#still have duplicates
#with rvindicator=Yes for Texture, leaves 113 duplicate cokeys for merged Horizon table

#find duplicate rows
names(Horizon)

View(Horizon[duplicated(Horizon$chkey)|duplicated(Horizon$chkey, fromLast=TRUE),c(1,2,3,9,12,172,173,175,176,179)])

#Duplicates have same texdesc
#duplicates have same cokey and chkey and chtgkey, different chtkey
#slight differences in the different texcl per chtkey, e.g., Loam vs Silt loam

#Reduce to one record per depth component - 
#Pick first record for each component
#since all are listed as 'representative'; have to pick one 
Horizon2 <- Horizon[!duplicated(Horizon$chkey),]
nrow(Horizon)
nrow(Horizon2)
length(Horizon2$cokey)
length(unique(Horizon2$cokey))
#Ok now are unique

#'Copm' Table (Soil Parent Origin)
AR3 <- sf::st_read(dsn = "./gSSURGO_AR.gdb", layer = "copm")
IA3 <- sf::st_read(dsn = "./gSSURGO_IA.gdb", layer = "copm")
IL3 <- sf::st_read(dsn = "./gSSURGO_IL.gdb", layer = "copm")
IN3 <- sf::st_read(dsn = "./gSSURGO_IN.gdb", layer = "copm")
KS3 <- sf::st_read(dsn = "./gSSURGO_KS.gdb", layer = "copm")
MN3 <- sf::st_read(dsn = "./gSSURGO_MN.gdb", layer = "copm")
MO3 <- sf::st_read(dsn = "./gSSURGO_MO.gdb", layer = "copm")
ND3 <- sf::st_read(dsn = "./gSSURGO_ND.gdb", layer = "copm")
NE3 <- sf::st_read(dsn = "./gSSURGO_NE.gdb", layer = "copm")
OH3 <- sf::st_read(dsn = "./gSSURGO_OH.gdb", layer = "copm")
SD3 <- sf::st_read(dsn = "./gSSURGO_SD.gdb", layer = "copm")
WI3 <- sf::st_read(dsn = "./gSSURGO_WI.gdb", layer = "copm")

#Append all tables using rbind
Copm<-rbind(AR3, IA3, IL3, IN3, KS3, MN3, MO3, ND3, NE3, OH3, SD3, WI3)
nrow(Copm)
head(Copm)
names(Copm)

#'Copmgrp Table' (Needed to link Copm - parent material - table to component)
AR4 <- sf::st_read(dsn = "./gSSURGO_AR.gdb", layer = "copmgrp")
IA4 <- sf::st_read(dsn = "./gSSURGO_IA.gdb", layer = "copmgrp")
IL4 <- sf::st_read(dsn = "./gSSURGO_IL.gdb", layer = "copmgrp")
IN4 <- sf::st_read(dsn = "./gSSURGO_IN.gdb", layer = "copmgrp")
KS4 <- sf::st_read(dsn = "./gSSURGO_KS.gdb", layer = "copmgrp")
MN4 <- sf::st_read(dsn = "./gSSURGO_MN.gdb", layer = "copmgrp")
MO4 <- sf::st_read(dsn = "./gSSURGO_MO.gdb", layer = "copmgrp")
ND4 <- sf::st_read(dsn = "./gSSURGO_ND.gdb", layer = "copmgrp")
NE4 <- sf::st_read(dsn = "./gSSURGO_NE.gdb", layer = "copmgrp")
OH4 <- sf::st_read(dsn = "./gSSURGO_OH.gdb", layer = "copmgrp")
SD4 <- sf::st_read(dsn = "./gSSURGO_SD.gdb", layer = "copmgrp")
WI4 <- sf::st_read(dsn = "./gSSURGO_WI.gdb", layer = "copmgrp")

#Append all tables using rbind
Copmgrp<-rbind(AR4, IA4, IL4, IN4, KS4, MN4, MO4, ND4, NE4, OH4, SD4, WI4)
nrow(Copmgrp)
head(Copmgrp)
names(Copmgrp)

#Aggregate parent material from the start 
#(unless there are additional subcomponents within components??)

Parent<-merge(Copm, Copmgrp, by=c('copmgrpkey'))
head(Parent)
nrow(Parent)
#same number of rows as Copm; so was a 1:1 join
#check for duplicate cokeys
length(Parent$cokey)
length(unique(Parent$cokey))
#yes are duplicates, not 1:1 with cokey
#As for Texture, select rvindicator=Yes to avoid duplicates
names(Parent)
Parent2<-Parent[Parent$rvindicator=="Yes",]
#now check for duplicate cokeys
length(Parent2$cokey)
length(unique(Parent2$cokey))
#still have duplicates -
#find duplicates:
head(Parent2)
nrow(Parent2[duplicated(Parent2$cokey)|duplicated(Parent2$cokey, fromLast=TRUE),])
duplicates.Parent<-Parent2[duplicated(Parent2$cokey)|duplicated(Parent2$cokey, fromLast=TRUE),]
duplicates.Parent[1:20,]
#many duplicates: 200,000+
#out of ~430,000 rows
#pmorder - the sequence in which the parent material occurs, when more than one parent material exists for one soil profile
#Note: will select pmorder ==1; assume this is the material closes to the surface in the soil profile
#Note that when there is only one parent material, no entry is required for pmorder, so also allow NA

Parent3<-Parent2[Parent2$pmorder=="1"|is.na(Parent2$pmorder),]
nrow(Parent3)

#now check for duplicate cokeys
length(Parent3$cokey)
length(unique(Parent3$cokey))
#still a smaller number of duplicates - ~2000
duplicates.Parent3<-Parent3[duplicated(Parent3$cokey)|duplicated(Parent3$cokey, fromLast=TRUE),]
View(duplicates.Parent3[1:20,])
#are multiple pmkind in the same order
#pmgroupname seems to specify all possible combinations for each component
levels(factor(Parent3$pmgroupname)) #over 5000 levels
#Pick one pmkind for each horizon - 
#since they are all 'representative'
#this isn't perfect, but subsets 1 type to each horizon
Parent4 <- Parent3[!duplicated(Parent3$cokey),]
nrow(Parent3)
nrow(Parent4)
length(Parent4$cokey)
length(unique(Parent4$cokey))
#Now there are no duplicates for cokey

#'Valu1' Table:
#(already merged in ArcGIS)
valu1<-read.delim('./Ssurgo_merged_Valu1_table.txt', sep=",", header=TRUE)
head(valu1)
valu1$MUTKEY<-valu1$mukey
head(valu1)


#COMPLETE LIST OF SSURGO TABLES NEEDED:
names(Component) #component unit (cokey)
names(Horizon2) #component unit (cokey)
names(Parent4) #component unit (cokey)
names(valu1) #map unit (mukey)

##
##Subset to desired ssurgo attributes - ie, those likely to be related to P content (as per Records et al 2016)

#Subset Component
names(Component)
Component.list<-c('comppct_r', 'slope_r', 'slopelenusle_r','runoff',
'erocl', 'hydricon', 'hydricrating', 'drainagecl', 'elev_r', 'geomdesc',
'airtempa_r', 'reannualprecip_r', 'ffd_r', 'taxceactcl', 'taxreaction', 'taxpartsize',
'mukey', 'cokey')
Component.sub<-Component[,names(Component) %in% Component.list]
head(Component.sub)
names(Component.sub)

#Subset Horizon2
names(Horizon2)
Horizon.list<-c('cokey', 'hzdept_r', 'hzdepb_r','sieveno4_r','sieveno10_r','sieveno40_r','sieveno200_r',
'sandtotal_r','silttotal_r','claytotal_r','claysizedcarb_r','om_r','ksat_r',
'kffact','caco3_r','gypsum_r', 'freeiron_r','feoxalate_r','extral_r','aloxalate_r','pbray1_r', 'poxalate_r', 
'partdensity', 'ph2osoluble_r', 'ptotal_r', 'texcl','texture', 'texdesc')

Horizon2<-as.data.frame(Horizon2)
Horizon.sub<-Horizon2[ , names(Horizon2) %in% Horizon.list] 
names(Horizon.sub)
View(Horizon.sub[,c(1,2,3,26,27,28)])
nrow(Horizon.sub)

length(Horizon.sub$cokey)
length(unique(Horizon.sub$cokey))

#Subset Parent
names(Parent4)
head(Parent4)
nrow(Parent4)
Parent4.list<-c('cokey', 'pmkind', 'pmorigin')
Parent.sub<-Parent4[,names(Parent4) %in% Parent4.list]
head(Parent.sub)
nrow(Parent.sub)

#Merge everything to mukey; using cokey
#need comppct from component table for % contribution of each component to each mukey (see below)

#Merge Component and Horizon
Comp1<-merge(Component.sub, Horizon.sub, by=c('cokey'))
head(Comp1)
nrow(Comp1)

#Some mukeys are lost because of restriction to surface samples with depth <31 cm
#e.g., 
filter(Comp1, mukey==424124) #value is not present
filter(Component.sub, mukey==424124) #it is present in the original Component table
#so some mukeys only have attributes for bottom soil depths below 31 cm

#how many mukeys are lost?

#~10,000 mukeys lost, region wide

#check for duplicates
length(Comp1$cokey)
length(unique(Comp1$cokey))
#No duplicates


#Merge in Parent table
Comp2<-merge(Comp1, Parent.sub, by=c('cokey'))
head(Comp2)
nrow(Comp2)
#check for duplicates
length(Comp2$cokey)
length(unique(Comp2$cokey))
#No duplicates


##################################################################
#Summarize component scale attributes at the map unit scale
#For numeric variables, take weighted average for each map unit; 
#For categorical variable, take component with highest % of map unit;

#weights are 'comppct_r'
#i.e., the percent of each component in the map unit (the mukey)

#For each numerical attribute at component scale, need to calculate weighted average
#for each mukey (each group)

#check column types to make sure each is designated as correct type
#(either numeric or factor)

sapply(Comp2, class)
head(Comp2)
names(Comp2)

#use the transform method to change the in built type of each feature.
#transform numeric variables
Comp2 <- transform(
  Comp2,
  slope_r=as.numeric(slope_r),
  slopelenusle_r=as.numeric(slopelenusle_r),  
  elev_r=as.numeric(elev_r),
  airtempa_r=as.numeric(airtempa_r),
  reannualprecip_r=as.numeric(reannualprecip_r),
  ffd_r=as.numeric(ffd_r),
  sieveno4_r=as.numeric(sieveno4_r),
  sieveno10_r=as.numeric(sieveno10_r),
  sieveno40_r=as.numeric(sieveno40_r),
  sieveno200_r=as.numeric(sieveno200_r),
  sandtotal_r=as.numeric(sandtotal_r),
  silttotal_r=as.numeric(silttotal_r),
  claytotal_r=as.numeric(claytotal_r),
  claysizedcarb_r=as.numeric(claysizedcarb_r),
  om_r=as.numeric(om_r),
  partdensity=as.numeric(partdensity),
  ksat_r=as.numeric(ksat_r),
  kffact=as.numeric(kffact),
  caco3_r=as.numeric(caco3_r),
  gypsum_r=as.numeric(gypsum_r),
  freeiron_r=as.numeric(freeiron_r),
  feoxalate_r=as.numeric(feoxalate_r),
  extral_r=as.numeric(extral_r),
  aloxalate_r=as.numeric(aloxalate_r),
  pbray1_r=as.numeric(pbray1_r),
  poxalate_r=as.numeric(poxalate_r),
  ph2osoluble_r=as.numeric(ph2osoluble_r),
  ptotal_r=as.numeric(ptotal_r),
  comppct_r=as.numeric(comppct_r))

#check levels for each categorical variables
#note - random forest models can only handle categorical variables with a certain number of levels (53)

levels(factor(Comp2$runoff)) # 6 levels
levels(factor(Comp2$erocl)) # 5 levels
levels(factor(Comp2$hydricon)) # 3 levels
levels(factor(Comp2$hydricrating)) # 3 levels
levels(factor(Comp2$drainagecl)) # 8 levels
levels(factor(Comp2$geomdesc)) #4061 levels!! -->Exclude
levels(factor(Comp2$taxpartsize)) #38 levels --> Combine? Or Exclude;
#^^might be represented by other variables, e.g., % clay
levels(factor(Comp2$taxceactcl)) #5 levels
levels(factor(Comp2$taxreaction)) #7 levels
levels(factor(Comp2$texcl)) #21 levels, could substitute for taxpartsize maybe
levels(factor(Comp2$pmkind)) #65 levels -->could aggregate some levels
levels(factor(Comp2$pmorigin)) #54 levels

#Variables to exclude or modify (eg, aggregate or code dummy variables):
#geomdesc, taxpartsize

#for pmorigin, combine similar categories:
#combine all organic materials
Comp2$pmkind<-ifelse(Comp2$pmkind=="Grassy organic material", "Organic material", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Herbaceous organic material", "Organic material", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Mossy organic material", "Organic material", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Woody organic material", "Organic material", Comp2$pmkind)
#combine all types of till
Comp2$pmkind<-ifelse(Comp2$pmkind=="Basal till", "Till", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Flow till", "Till", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Lodgement till", "Till", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Melt-out till", "Till", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Subglacial till", "Till", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Supraglacial till", "Till", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Ablation till", "Till", Comp2$pmkind)
#combine mine spoil
Comp2$pmkind<-ifelse(Comp2$pmkind=="Coal extraction mine spoil", "Mine spoil-coal or metal", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Metal ore extraction mine spoil", "Mine spoil-coal or metal", Comp2$pmkind)
Comp2$pmkind<-ifelse(Comp2$pmkind=="Mine spoil or earthy fill", "Mine spoil-coal or metal", Comp2$pmkind)

levels(factor(Comp2$pmkind))

#transform categorical variables
Comp2 <- transform(
  Comp2,
runoff=as.factor(runoff),
erocl=as.factor(erocl),
hydricon=as.factor(hydricon),
hydricrating=as.factor(hydricrating),
drainagecl=as.factor(drainagecl),
taxpartsize=as.factor(taxpartsize),
taxceactcl=as.factor(taxceactcl),
taxreaction=as.factor(taxreaction),
texcl=as.factor(texcl),
pmkind=as.factor(pmkind),  
mukey=as.factor(mukey)
)

sapply(Comp2, class)


#Note: i'm guessing you will also lose some levels when you merge to soil P data (subset of the country)

#Check data:
#check that numeric variables are numeric summaries
#check that categorical variables are counts
#check for missing data ('?')
summary(Comp2)


###
#LIST OF NUMERIC & CATEGORICAL ATTRIBUTES

Numeric.list<-c('slope_r', 'slopelenusle_r', 'elev_r', 
'airtempa_r', 'reannualprecip_r', 'ffd_r','sieveno4_r', 'sieveno10_r', 'sieveno40_r', 'sieveno200_r', 
'sandtotal_r', 'silttotal_r', 'claytotal_r', 'claysizedcarb_r', 'om_r', 'partdensity',
'ksat_r', 'kffact', 'caco3_r', 'gypsum_r')

Categorical.list<-c('mukey', 'comppct_r', 'runoff', 'erocl', 'hydricon', 'hydricrating', 'drainagecl', 'taxpartsize',
'taxceactcl', 'taxreaction', 'texcl', 'pmkind')


#Variables to exclude: 
#geomdesc - too many levels
#almost no or no data; 'freeiron_r', 'feoxalate_r', 'extral_r', 
#'aloxalate_r', 'pbray1_r', 'poxalate_r', 'ph2osoluble_r', 'ptotal_r'

##SUMMARIZE ATTRIBUTES:

#NUMERIC
#calculate weighted mean for each mukey (weights are % contribution to mukey area) 
#calculate for multiple attributes at once
#note: for weighted mean approach attributes must be numeric

library(dplyr)
names(Comp2)

Mukey.mean<-Comp2 %>% group_by(mukey) %>% 
    summarise_at(Numeric.list, 
                 list(~weighted.mean(., comppct_r)))

head(Mukey.mean)

#make into a regular table
Mukey.mean<-as.data.frame(Mukey.mean)
head(Mukey.mean)

#check for duplicates
length(Mukey.mean$mukey)
length(unique(Mukey.mean$mukey))
#no duplicates

#CATEGORICAL
#select component level with the highest percentage area in the map unit
#if two component levels are tied for area, use tie breaker (see below)
#highest compcct_r for each attribute in each mutkey

#group by mukey
#apply just across columns in Categorical.list
#select attribute levels in each group based on max compcct_r

# convert to data.table with key=mukey
library(data.table)
Comp.table <- data.table(Comp2[,Categorical.list], key="mukey")
# get the subset of data that matches this criterion
Mukey.top2<-unique(Comp.table[, .SD[comppct_r %in% max(comppct_r)], by=mukey])
head(Mukey.top2)

#check for duplicates
length(Mukey.top2$mukey)
length(unique(Mukey.top2$mukey))

#Are some duplicates here - for some map units, multiple components have same area
#Need to further winnow down - see below


###
##So now you have:
#weighted average for numeric variables for each map unit
#component with highest percentage area in map unit for categorical variables

#Merge the two;
SSURGO.att<-merge(Mukey.mean, Mukey.top2, by=c('mukey'))
head(SSURGO.att)
#check for duplicates
length(SSURGO.att$mukey)
length(unique(SSURGO.att$mukey))

#Add in attributes from Valu1 table
SSURGO.all<-merge(SSURGO.att, valu1, by=c('mukey'))
head(SSURGO.all)

#check for duplicates
length(SSURGO.all$mukey)
length(unique(SSURGO.all$mukey))

#are some duplicates - due to multiple components of same area in some map units
#Choose higher slope as the tie breaker, because surface P runoff increases for soils with greater slope
#select rows with highest slope in each mukey
names(SSURGO.all)

require(dplyr)
names(SSURGO.all)
SSURGO.top<-as.data.frame(
 SSURGO.all %>% group_by(mukey) %>% top_n(1, slope_r))
head(SSURGO.top)

#check for duplicates
length(SSURGO.top$mukey)
length(unique(SSURGO.top$mukey))

#Still some duplicates above -- Choose a row at random, as a second tie breaker
SSURGO.unique <- SSURGO.top[!duplicated(SSURGO.top$mukey),]
head(SSURGO.unique)

#check for duplicates
length(SSURGO.unique$mukey)
length(unique(SSURGO.unique$mukey))
#Now have unique records

#write to table;
#these are unique attributes for each map unit, from multiple SSURGO tables

write.table(SSURGO.unique, './SSURGO_attributes_unique_for_mapunit_CEAP_regionalextent.csv', sep=",", row.names=FALSE)

#############################################################################
#############################################################################
#STEP 3: LOADING & PRE PROCESSING OF STREAMCAT ATTRIBUTES

#watershed and catchment scale variables from StreamCat

#38 StreamCat Tables

#1 StreamSize
StreamSize<-read.delim('./StreamCat_Text_Files/StreamSize_StreamCat.csv', sep=",", header=TRUE)
head(StreamSize)
nrow(StreamSize)
StreamSize<-StreamSize[,c(1,2,3)]
head(StreamSize) 
nrow(StreamSize)

#2 CanalDensity
CanalDensity<-read.delim('./StreamCat_Text_Files/CanalDensity_StreamCat.csv', sep=",", header=TRUE)
head(CanalDensity)
CanalDensity<-CanalDensity[,c(1,2,3)]
head(CanalDensity)
nrow(CanalDensity)

#3 Elevation
Elevation<-read.delim('./StreamCat_Text_Files/Elevation_StreamCat.csv', sep=",", header=TRUE)
head(Elevation)
Elevation<-Elevation[,c(1,2,3)]
head(Elevation)
nrow(Elevation)

#4 AgMidHiSlopes
AgMidHiSlopes<-read.delim('./StreamCat_Text_Files/AgMidHiSlopes_StreamCat.csv', sep=",", header=TRUE)
head(AgMidHiSlopes)
AgMidHiSlopes<-AgMidHiSlopes[,c(1:5)]
head(AgMidHiSlopes)
nrow(AgMidHiSlopes)

#5
Impervious2011<-read.delim('./StreamCat_Text_Files/ImperviousSurfaces2011_StreamCat.csv', sep=",", header=TRUE)
head(Impervious2011)
names(Impervious2011)

#6
Impervious2011RipBuf100<-read.delim('./StreamCat_Text_Files/ImperviousSurfaces2011RipBuf100_StreamCat.csv', sep=",", header=TRUE)
head(Impervious2011RipBuf100)
names(Impervious2011RipBuf100)

#7
#clay and sand content of soils: - catchment or watershed scale
Clay.Sand<-read.delim('./StreamCat_Text_Files/STATSGO_Set1_StreamCat.csv', sep=",", header=TRUE) 
head(Clay.Sand)
names(Clay.Sand)

#8
#organic matter content, mean water table depth, permeability, mean soil depth - catchment or watershed scale
Soil.Char<-read.delim('./StreamCat_Text_Files/STATSGO_Set2_StreamCat.csv', sep=",", header=TRUE)
head(Soil.Char)
names(Soil.Char)

#9
Dams<-read.delim('./StreamCat_Text_Files/Dams_StreamCat.csv', sep=",", header=TRUE)
head(Dams)
names(Dams)

#10
Toxic.ReleaseWS<-read.delim('./StreamCat_Text_Files/EPA_FRS_StreamCat.csv', sep=",", header=TRUE)
head(Toxic.ReleaseWS)
names(Toxic.ReleaseWS)
Toxic.ReleaseWS<-Toxic.ReleaseWS[,c(1,6:11)]
head(Toxic.ReleaseWS)
nrow(Toxic.ReleaseWS) 

#11
Toxic.Release.Rip<-read.delim('./StreamCat_Text_Files/EPA_FRSRipBuf100.csv', sep=",", header=TRUE)
head(Toxic.Release.Rip)
names(Toxic.Release.Rip)

#12 
Mines<-read.delim('./StreamCat_Text_Files/Mines_StreamCat.csv', sep=",", header=TRUE)
head(Mines)
names(Mines)

#13
#Mines in Buffer
MinesRipBuf100<-read.delim('./StreamCat_Text_Files/MinesRipBuf100_StreamCat.csv', sep=",", header=TRUE)
head(MinesRipBuf100)
names(MinesRipBuf100)

#14
#Note: Lithology dataset has a lot of variables - do I want to include all of these, or are some 
#more relevant than others?
Lithology<-read.delim('./StreamCat_Text_Files/Lithology_StreamCat.csv', sep=",", header=TRUE)
head(Lithology)
names(Lithology)

#15 RoadDensity
RoadDensity<-read.delim('./StreamCat_Text_Files/RoadDensity_StreamCat.csv', sep=",", header=TRUE)
head(RoadDensity)
names(RoadDensity)

#16 RoadDensityRipBuf100
RoadDensityRipBuf100<-read.delim('./StreamCat_Text_Files/RoadDensityRipBuf100_StreamCat.csv', sep=",", header=TRUE)
head(RoadDensityRipBuf100)
names(RoadDensityRipBuf100)

#17
RoadStreamCrossings<-read.delim('./StreamCat_Text_Files/RoadStreamCrossings_StreamCat.csv', sep=",", header=TRUE)
head(RoadStreamCrossings)
names(RoadStreamCrossings)

#18
Runoff<-read.delim("./StreamCat_Text_Files/Runoff_StreamCat.csv", sep=",", header=TRUE)
head(Runoff)
names(Runoff)

#19 Pesticides
Pesticides97<-read.delim('./StreamCat_Text_Files/Pesticides97_StreamCat.csv', sep=",", header=TRUE)
head(Pesticides97)
names(Pesticides97)

#20
#Baseflow Index - ratio of baseflow to total flow, as a percentage, within a watershed
Baseflow.Index<-read.delim('./StreamCat_Text_Files/BFI_StreamCat.csv', sep=",", header=TRUE)
head(Baseflow.Index)
names(Baseflow.Index)

#21 NADP 
NADP<-read.delim('./StreamCat_Text_Files/NADP_StreamCat.csv', sep=",", header=TRUE)
head(NADP)
names(NADP)

#22 Kffact
Kffact<-read.delim('./StreamCat_Text_Files/Kffact_StreamCat.csv', sep=",", header=TRUE)
head(Kffact)
names(Kffact)

#23 PRISM_1981_2010
PRISM_1981_2010<-read.delim('./StreamCat_Text_Files/PRISM_1981_2010_StreamCat.csv', sep=",", header=TRUE)
head(PRISM_1981_2010)
names(PRISM_1981_2010)

#24 NABD
NABD<-read.delim('./StreamCat_Text_Files/NABD_StreamCat.csv', sep=",", header=TRUE)
head(NABD)
names(NABD)

#25
NLCD2011<-read.delim('./StreamCat_Text_Files/NLCD2011_StreamCat.csv', sep=",", header=TRUE)
head(NLCD2011)
names(NLCD2011)

#26
NLCD2011RipBuf100<-read.delim('./StreamCat_Text_Files/NLCD2011RipBuf100_StreamCat.csv', sep=",", header=TRUE)
head(NLCD2011RipBuf100)
names(NLCD2011RipBuf100)

#27
#GeoChemPhys data 
GeoChemPhys1<-read.delim('./StreamCat_Text_Files/GeoChemPhys1_StreamCat.csv', sep=",", header=TRUE)
head(GeoChemPhys1)
names(GeoChemPhys1)

#28
#GeoChemPhys data 2
GeoChemPhys2<-read.delim('./StreamCat_Text_Files/GeoChemPhys2_StreamCat.csv', sep=",", header=TRUE)
head(GeoChemPhys2)
names(GeoChemPhys2)

#29
#GeoChemPhys data 3
GeoChemPhys3<-read.delim('./StreamCat_Text_Files/GeoChemPhys3_StreamCat.csv', sep=",", header=TRUE)
head(GeoChemPhys3)
names(GeoChemPhys3)

#30
#GeoChemPhys data 4
GeoChemPhys4<-read.delim('./StreamCat_Text_Files/GeoChemPhys4_StreamCat.csv', sep=",", header=TRUE)
head(GeoChemPhys4)
names(GeoChemPhys4)

#31
#Coal Mines
CoalMines<-read.delim('./StreamCat_Text_Files/CoalMines_StreamCat.csv', sep=",", header=TRUE)
head(CoalMines)
names(CoalMines)

#32
AgN<-read.delim('./StreamCat_Text_Files/AgriculturalNitrogen_StreamCat.csv', sep=",", header=TRUE)
head(AgN)
names(AgN)

#33
RefStreamTempPred<-read.delim('./StreamCat_Text_Files/RefStreamTempPred_StreamCat.csv', sep=",", header=TRUE)
head(RefStreamTempPred)
names(RefStreamTempPred)
#Pick 2008 variables
RefStreamTempPred<-RefStreamTempPred[, c(1,2,6,10)]
head(RefStreamTempPred)
nrow(RefStreamTempPred)

#34 WetIndx
WetIndx<-read.delim('./StreamCat_Text_Files/WetIndx_StreamCat.csv', sep=",", header=TRUE)
head(WetIndx)
names(WetIndx)

#Add in 2006 land cover info:
#35
NLCD2006<-read.delim('./StreamCat_Text_Files/NLCD2006_StreamCat.csv', sep=",", header=TRUE)
head(NLCD2006)
names(NLCD2006)

#36
NLCD2006RipBuf100<-read.delim('./StreamCat_Text_Files/NLCD2006RipBuf100_StreamCat.csv', sep=",", header=TRUE)
head(NLCD2006RipBuf100)
names(NLCD2006RipBuf100)

#37
Impervious2006<-read.delim('./StreamCat_Text_Files/ImperviousSurfaces2006_StreamCat.csv', sep=",", header=TRUE)
head(Impervious2006)
names(Impervious2006)

#38
Impervious2006RipBuf100<-read.delim('./StreamCat_Text_Files/ImperviousSurfaces2006RipBuf100_StreamCat.csv', sep=",", header=TRUE)
head(Impervious2006RipBuf100)
names(Impervious2006RipBuf100)


#Merge all selected StreamCat variables;

#Create merge function
my_merge <- function(df1, df2){                               
  merge(df1, df2, by = "COMID")
}

#create list of tables
data_list <- list(StreamSize, CanalDensity, Elevation, AgMidHiSlopes, Impervious2011, 
Impervious2011RipBuf100)

data_list1<-list(Clay.Sand, Soil.Char, Dams, Toxic.ReleaseWS,
Toxic.Release.Rip)

data_list2<-list(Mines, MinesRipBuf100, Lithology, RoadDensity, RoadDensityRipBuf100, RoadStreamCrossings,
Runoff, Pesticides97)

data_list3<-list(Baseflow.Index, NADP, Kffact, PRISM_1981_2010, NABD, NLCD2011, NLCD2011RipBuf100)

data_list4<-list(GeoChemPhys1, GeoChemPhys2, GeoChemPhys3, GeoChemPhys4, CoalMines, AgN, RefStreamTempPred, WetIndx)  

data_list5<-list(NLCD2006, NLCD2006RipBuf100, Impervious2006, Impervious2006RipBuf100)


#Attributes from 38 StreamCat Tables (+ Stream Size) out of 58 total available StreamCat tables
#What are the other StreamCat tables (not used)?
#10 Tables are NLCD and Impervious Surface data from other years not used (2001, 2011, 2016);
#used NLCD 2006
#4 are NRSA Predicted Bio and/or estimates of watershed condition from other works (Hill et al 2017; Thornbrugh et al 2018)
#remaining tables omitted include Census data, Fire tables, % forest loss tables

#apply function
#split into 4 to save memory
StreamCat.all<-Reduce(my_merge, data_list) 

StreamCat.all1<-Reduce(my_merge, data_list1)

StreamCat.all2<-Reduce(my_merge, data_list2) 

StreamCat.all3<-Reduce(my_merge, data_list3) 

StreamCat.all4<-Reduce(my_merge, data_list4) 

StreamCat.all5<-Reduce(my_merge, data_list5)

head(StreamCat.all)
nrow(StreamCat.all)

head(StreamCat.all2)
nrow(StreamCat.all2)

head(StreamCat.all3)
#Note large number of NAs for some Riparian attributes
summary(StreamCat.all3$PctDecid2011CatRp100)
nrow(StreamCat.all3)

head(StreamCat.all4)
nrow(StreamCat.all4)

head(StreamCat.all5)
nrow(StreamCat.all5)

#for some reason this produces duplicate rows

#remove duplicates using dplyr
#load tidyverse - contains dplyr
library(tidyverse)
StreamCat.all.unique<-StreamCat.all %>% distinct()
head(StreamCat.all.unique)
nrow(StreamCat.all.unique)

StreamCat.all.unique1<-StreamCat.all1 %>% distinct()
head(StreamCat.all.unique1)
nrow(StreamCat.all.unique1)

StreamCat.all.unique2<-StreamCat.all2 %>% distinct()
head(StreamCat.all.unique2)
nrow(StreamCat.all.unique2)
#This still has many duplicate rows??

StreamCat.all.unique3<-StreamCat.all3 %>% distinct()
head(StreamCat.all.unique3)
nrow(StreamCat.all.unique3)

StreamCat.all.unique4<-StreamCat.all4 %>% distinct()
head(StreamCat.all.unique4)
nrow(StreamCat.all.unique4)

StreamCat.all.unique5<-StreamCat.all5 %>% distinct()
head(StreamCat.all.unique5)
nrow(StreamCat.all.unique5)

#Merge 2 halves of the dataset
StreamCat.merge<-merge(StreamCat.all.unique, StreamCat.all.unique1, by=c("COMID"))
nrow(StreamCat.merge)

StreamCat.merge2<-merge(StreamCat.all.unique2, StreamCat.all.unique3, by=c("COMID"))
nrow(StreamCat.merge2)

StreamCat.merge3<-merge(StreamCat.merge2, StreamCat.all.unique4, by=c("COMID"))
nrow(StreamCat.merge3)

#Merge everything together:
StreamCat.ALL<-merge(StreamCat.merge, StreamCat.merge3, by=c("COMID"))
head(StreamCat.ALL)
nrow(StreamCat.ALL)
names(StreamCat.ALL)

StreamCat.ALL2<-merge(StreamCat.ALL, StreamCat.all.unique5, by=c("COMID"))
head(StreamCat.ALL2)
names(StreamCat.ALL2)
nrow(StreamCat.ALL2)


#Write to table; 
write.table(StreamCat.ALL2, './37TABLES_StreamCat_variables_for_SOIL_P.csv', sep=",", row.names=FALSE)


#####################################################################################
#STEP 4: LOAD IN SOIL P DATA w/SPATIL ATTRIBUTES & MERGE WITH SSURGO & STREAMCAT ATTRIBUTES (originally from script 'soilP_addtl_dataset_prep_and_orig_models_TPonly_11_18.R'

#Soil P datasets:
#USGS National Geochem Survey
#Nat'l Soil Characterization Database

#soil P datasets have had the following pre-processing:
#samples w/P data available
#subsetted to soil samples
#surface samples <12 inch depth (<31 cm), i.e., 'plowable layer'
#samples collected since 2000
#attributes merged in GIS: 
#NHDv2Plus - are sites w/in 100 m buffer of NHD network Y/N
#NHD stream type from NHDv2Plus, if within 100 m buffer of stream network
#NWI wetland type, if within wetland polygon
#NLCD 2006 Land Use category in 30m cell where sample occurs
#SSURGO mutkey ID (for merging with ssurgo attributes - see below)
#NHDv2Plus Catchment COMID (for merging with StreamCat attributes - see below)

#USGS NATIONAL GEOCHEMICAL SURVEY DATA
soilP<-read.delim('./USGS_alldepths_soil_since2000_region_NWI_NHDv2100_NLCD06_ssurgo_catchment.txt', sep=",", header=TRUE)
head(soilP)
nrow(soilP)
#rename wetland attribute
names(soilP)
names(soilP)
#check date range
soilP$COLL_DATE
soilP$DATE<-as.Date(as.character(soilP$COLL_DATE), format="%Y%m%d")
soilP$DATE
summary(soilP$DATE)
#all samples collected since 2000
nrow(soilP) #n=6670

#Check unique sample locations
nrow(unique(soilP[c("LATITUDE","LONGITUDE")])) #n=3245 unique locations

#NATL SOIL CHARACTERIZATION DATABASE
#LOAD IN NCSS TP (from MAJOR ELEMENTS TABLE)
ncss.TP<-read.delim('./NCSS_alldepths_MajorElements_since2000_region_NLCD06_NHDv2100_NWI_ssurgo_catchment.txt', sep=",", header=TRUE)
head(ncss.TP)
names(ncss.TP)
names(ncss.TP)
#rename P column
ncss.TP$P_mgkg<-ncss.TP$p_mjelm
class(ncss.TP$Date)
ncss.TP$DATE<-as.POSIXct(ncss.TP$Date,format='%m/%d/%Y')
ncss.TP$DATE<-as.Date(ncss.TP$DATE)
ncss.TP$DATE
summary(ncss.TP$DATE)
nrow(ncss.TP)
ncss.TP.recent<-ncss.TP
summary(ncss.TP.recent$DATE)

#NEED TO HARMONIZE DEPTH INFO ACROSS THE TWO DATASETS;
#make a uniform depth column with same units across two datasets
#Depth will be based on bottom depth of the surface layer
names(soilP)
names(ncss.TP.recent)
#Note NCSS Access database specifies the units of hzn_bot as centimeters
ncss.TP.recent$Depth_cm<-ncss.TP.recent$hzn_bot

#Units of USGS depth are inches; convert to centimeters
soilP$Depth_cm<-soilP$max_depth_est*2.54


#ADD IN NCSS TP DATA TO USGS TP DATA - NEED COLUMNS TO MATCH
#(note checked and for both datasets units for P are mg/kg)
names(ncss.TP.recent)
names(soilP)
nrow(soilP)

#CREATE UNIQUE SAMPLE ID SO CAN LINK TO LAT/LONG LATER FOR MAPPING
#note there could be more than one sample per location, if sampled at multiple depths
soilP$SiteID<-(1:nrow(soilP))
head(soilP)
nrow(soilP)
n<-nrow(soilP)+nrow(ncss.TP.recent)
n
ncss.TP.recent$SiteID<-(6671:n)
head(ncss.TP.recent)
nrow(ncss.TP.recent)


#####
#subset ncss TP data to just P column (excluding soil test P measures), and attributes used by soil P
#drop all columns you don't need for both datasets
#NOTE: you need FEATUREID for catchment ID, to link soil P to streamcat attributes
#Keep SiteID for any future mapping
#(which is the same as COMID, so will need to make a new column to join datasets below by COMID)

names(ncss.TP.recent)
names(soilP)

#Attributes to keep:
Attributes.Keep<-list('SiteID', 'P_mgkg','COMID', 'MUTKEY', 'FTYPE', 'FCODE', 'WETLAND_TYPE', 
'NLCD06', 'FEATUREID', 'DATE', 'Depth_cm')

soilP2<-soilP[,names(soilP) %in% Attributes.Keep]
names(soilP2)
ncss.TP.recent2<-ncss.TP.recent[,names(ncss.TP.recent) %in% Attributes.Keep]
names(ncss.TP.recent2)
#put columns in same order for ncss dataset:
order <- c("P_mgkg", "WETLAND_TYPE", "COMID", "FTYPE", "FCODE", "NLCD06", "MUTKEY", 
"FEATUREID", "DATE", "Depth_cm", "SiteID")
ncss.TP.recent2<-ncss.TP.recent2[order]
names(ncss.TP.recent2)

#BIND BOTH P DATASETS TOGETHER
soilP.all<-rbind(soilP2, ncss.TP.recent2)
nrow(soilP.all)
#N=7229

#Check date range:
summary(soilP.all$DATE)

##########################################################


##Option: PARSE SURFACE FROM SUBSURFACE SAMPLES 
#NOTE: You have opted to include all samples for the analysis below
#A model with only surface samples does not perform as well as a model with all samples
#You lose substantial n by limiting to surface samples
#some ssurgo attributes are specifically correlated to horizon depth
#previously you selected the top-most horizon info to use, with ssurgo attributes


#P_surface<-soilP.all[soilP.all$Depth_cm<31,]
#nrow(P_surface) #n=3538

#P_subsurface<-soilP.all[soilP.all$Depth>=31,]
#summary(P_subsurface$P_mgkg)


###########################################################
##POTENTIAL PREDICTOR VARIABLES

#load in additional SSURGO (soil) attributes
#(derived in script: 'processing_ssurgo_attributes_9_20_21'
#Note: this file includes Valu1 attributes

ssurgo<-read.delim('./SSURGO_attributes_unique_for_mapunit_CEAP_regionalextent.csv', sep=",", header=TRUE)
head(ssurgo)



###############################################################

##MERGE SOIL P DATA TO PREDICTOR VARIABLES
 

#For USGS GEOCHEM DATA + NCSS TP DATA:
soilP.att<-merge(soilP.all, ssurgo, by=c('MUTKEY'))
head(soilP.att)
names(soilP.att)
nrow(soilP.att)
#N=3347 for surface samples only
#note - losing 191 records when merge to ssurgo data; 
 #these are samples where the top hzdepb_r attribute in the Chorizon table was too low
#(i.e, below 30 cm)
#N=6890 for all samples



###################################################################
##FORMAT DATASET FOR RF MODELLING

####
#Create Levels for Attribute data

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
#SUBSET TO FINAL COLUMNS LIST
names(soilP.att)
head(soilP.att)

#retain COMID/FEATUREID to bind to STREAMCAT dataset

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
#n=535 for surface samples
#N=1046 for all depth samples

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
##ADD IN STREAM CAT VARIABLES (FOR NEAR CHANNEL SITES)
streamcat<-read.delim('./37TABLES_StreamCat_variables_for_SOIL_P.csv', sep=",", header=TRUE)
head(streamcat)
names(streamcat)

#Make a FEATUREID column equivalent to COMID
streamcat$FEATUREID<-streamcat$COMID

#MERGE STREAMCAT TO ALL SOIL P DATA
All.Att<-merge(data, streamcat, by=c('FEATUREID'))
head(All.Att)
nrow(All.Att)


#################################################################
###EXCLUDE VARIABLES: ##(OR OPTION TO INTERPOLATE FOR ALL MISSING VALUES - SEE BELOW
#1. WITH MANY/MAJORITY NAs
#2. THAT AREN'T TEMPORALLY RELEVANT (ie, wrong land use class year)
#3. BECAUSE THEY HAVE TOO MANY LEVELS FOR RANDOM FOREST

#rule for excluding NAs: if NAs are >20% of the dataset = 0.20*nrow(All.Att) = 669
summary(All.Att)
#even though you will interpolate for some missing values, still excluding values that are largely missing

#Exclude because temporal or other mismatch:
#(using 2006 variables)
Exclude.list<-c(
'PctOw2011Cat','PctIce2011Cat','PctUrbOp2011Cat' ,     
'PctUrbLo2011Cat','PctUrbMd2011Cat','PctUrbHi2011Cat'     , 
'PctBl2011Cat' ,'PctDecid2011Cat'    ,   'PctConif2011Cat'   ,   
'PctMxFst2011Cat'   ,    'PctShrb2011Cat'     ,   'PctGrs2011Cat' ,       
'PctHay2011Cat'    ,     'PctCrop2011Cat'     ,   'PctWdWet2011Cat' ,     
'PctHbWet2011Cat'    ,   'PctOw2011Ws'      ,     'PctIce2011Ws' ,        
'PctUrbOp2011Ws'   ,     'PctUrbLo2011Ws'      ,  'PctUrbMd2011Ws',       
'PctUrbHi2011Ws'    ,    'PctBl2011Ws'      ,     'PctDecid2011Ws' ,      
'PctConif2011Ws'    ,    'PctMxFst2011Ws'     ,   'PctShrb2011Ws' ,       
'PctGrs2011Ws'      ,    'PctHay2011Ws'       ,   'PctCrop2011Ws' ,       
'PctWdWet2011Ws'     ,   'PctHbWet2011Ws'    ,    'PctOw2011CatRp100' ,   
'PctIce2011CatRp100' ,   'PctUrbOp2011CatRp100' , 'PctUrbLo2011CatRp100' ,
'PctUrbMd2011CatRp100' , 'PctUrbHi2011CatRp100' , 'PctBl2011CatRp100' ,   
'PctDecid2011CatRp100' , 'PctConif2011CatRp100',  'PctMxFst2011CatRp100' ,
'PctShrb2011CatRp100'  , 'PctGrs2011CatRp100'  ,  'PctHay2011CatRp100' ,  
'PctCrop2011CatRp100' ,  'PctWdWet2011CatRp100' , 'PctHbWet2011CatRp100', 
'PctOw2011WsRp100'   ,   'PctIce2011WsRp100'   ,  'PctUrbOp2011WsRp100' , 
'PctUrbLo2011WsRp100'  , 'PctUrbMd2011WsRp100' ,  'PctUrbHi2011WsRp100' , 
'PctBl2011WsRp100'   ,   'PctDecid2011WsRp100'  , 'PctConif2011WsRp100' , 
'PctMxFst2011WsRp100'  , 'PctShrb2011WsRp100'  ,  'PctGrs2011WsRp100' ,   
'PctHay2011WsRp100'   ,  'PctCrop2011WsRp100'   , 'PctWdWet2011WsRp100'  ,
'PctHbWet2011WsRp100')

All.Att2<-All.Att[,!names(All.Att) %in% Exclude.list]
names(All.Att2)
nrow(All.Att2)
summary(All.Att2)

#Exclude Admin variables
All.Att2<-All.Att2[,!names(All.Att2) %in% Admin.list]
names(All.Att2)

## Remove columns with more than 20% NA
#tidyverse solution
library(tidyverse) 
All.Att3<-
All.Att2 %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=20)
names(All.Att3)
#ID removed columns:
names(All.Att2[, which(colMeans(is.na(All.Att2)) > 0.20)])


#Check how many records are lost if you exclude all NAs
#look at where NAs are
nrow(All.Att3[!complete.cases(All.Att3), ])
#n=3268

#Write data to file 
write.table(All.Att3, "./Soil_P_data_for_modelling.csv", sep=",", row.names=FALSE)


#####################################################################################
#STEP 5: RANDOM FOREST MODELS FOR SOIL P

#load some useful packages;
library(tidyverse)
library(tidymodels)
library(workflows)
library(tune)
library(ranger)


#Read in soil P data for modelling (prepped in script above):
P_orig<-read.delim('./Soil_P_data_for_modelling.csv', sep=",", header=TRUE)
head(P_orig)
nrow(P_orig)
#make a copy to work with
P_clean<-P_orig
names(P_clean)

#Create column for 'outliers' for soil P values > 1000 (tho note that outliers has an arbitrary definition)
P_clean$Outlier<-ifelse(P_clean$P_mgkg>1000, 1, 0)
head(P_clean)

summary(P_clean$P_mgkg)
#note negative value for P_mgkg; likely a data entry error
#exclude negative numbers
P_clean<-filter(P_clean, P_mgkg>0)
nrow(P_clean)
summary(P_clean$P_mgkg)

#Histogram of P
P.hist<-
  ggplot(P_clean, aes(P_mgkg))+
  geom_histogram(binwidth = 1,colour = "lightgray", fill = "darkgrey") +
  geom_vline(aes(xintercept=mean(P_mgkg)), linetype="dashed", color="blue")+
  xlab("P (mg/kg)") +
  ylab ("Frequency") +
  #ggtitle("Near Channel Soil P distribution")+
  #ggtitle("Soil P Samples distribution") +
  theme(axis.title = element_text(size=26))+
  theme(axis.text=element_text(size=26))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))
P.hist

# Formal calculation of skewness (e1071 package)
library(e1071) 
#The skewness for a normal distribution = zero
#and any symmetric data should have a skewness near zero. 
#Negative values for the skewness indicate data that are skewed left 
#positive values for the skewness indicate data that are skewed right. 
skewness(P_clean$P_mgkg)

#write fig to file
tiff("./Soil_totalP_histogram_usgs_ncss.tiff", units="in", width=8, height=8, res=300)
P.hist
dev.off()


#########
#Prep for modelling:
#exclude remaining covariates without useful information:
#note: also exclude remaining 2011 attributes!!
names(P_clean)

levels(factor(P_clean$taxceactcl))

Exclude.integer<-c("taxreaction", "MineDensCatRp100", "PctCarbResidCat", "PctAlkIntruVolCat", "PctSilicicCat",
                   "PctExtruVolCat", "PctSalLakeCat", "PctCoastCrsCat","PctSalLakeWs",
                   "PctCoastCrsWs", "PctIce2006Cat", "PctIce2006CatRp100", "PctImp2011Cat",
                   "PctImp2011CatRp100",
                   "PctImp2011Ws")

P_clean<-P_clean[,!(names(P_clean) %in% Exclude.integer)]
names(P_clean)

#format covariates to correct class:
P_clean <- transform(
  P_clean,
  P_mgkg=as.numeric(P_mgkg),
  slope_r=as.numeric(slope_r),
  elev_r=as.numeric(elev_r),
  airtempa_r=as.numeric(airtempa_r),
  ffd_r=as.numeric(ffd_r),
  comppct_r=as.numeric(comppct_r),
  soc150_999=as.numeric(soc150_999),
  sieveno4_r=as.numeric(sieveno4_r),
  sieveno10_r=as.numeric(sieveno10_r),
  sieveno40_r=as.numeric(sieveno40_r),
  sieveno200_r=as.numeric(sieveno200_r),
  sandtotal_r=as.numeric(sandtotal_r),
  silttotal_r=as.numeric(silttotal_r),
  claytotal_r=as.numeric(claytotal_r),
  om_r=as.numeric(om_r),
  ksat_r=as.numeric(ksat_r),
  kffact=as.numeric(kffact),
  caco3_r=as.numeric(caco3_r),
  gypsum_r=as.numeric(gypsum_r),
  aws0_5 = as.numeric(aws0_5), 
  aws5_20=as.numeric(aws5_20),
  aws20_50= as.numeric(aws20_50), 
  aws50_100=as.numeric(aws50_100), 
  aws100_150=as.numeric(aws100_150), 
  aws0_20=as.numeric(aws0_20),
  aws0_30=as.numeric(aws0_30),
  aws0_100=as.numeric(aws0_100), 
  aws0_150=as.numeric(aws0_150) , 
  aws0_999=as.numeric(aws0_999), 
  soc0_5=as.numeric(soc0_5), 
  soc5_20=as.numeric(soc5_20),
  soc20_50=as.numeric(soc20_50), 
  soc50_100=as.numeric(soc50_100),
  soc100_150=as.numeric(soc100_150), 
  soc0_20=as.numeric(soc0_20), 
  soc0_30=as.numeric(soc0_30),
  soc0_100=as.numeric(soc0_100), 
  soc0_150=as.numeric(soc0_150),
  soc0_999=as.numeric(soc0_999), 
  nccpi3corn=as.numeric(nccpi3corn),
  nccpi3soy=as.numeric(nccpi3soy), 
  nccpi3cot=as.numeric(nccpi3cot),
  nccpi3sg=as.numeric(nccpi3sg), 
  nccpi3all=as.numeric(nccpi3all),
  pctearthmc=as.numeric(pctearthmc), 
  rootznemc=as.numeric(rootznemc),
  rootznaws=as.numeric(rootznaws), 
  droughty=as.numeric(droughty),
  Depth_cm=as.numeric(Depth_cm)
)


#transform categorical variables
P_clean <- transform(
  P_clean,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  taxpartsize=as.factor(taxpartsize),
  taxceactcl=as.factor(taxceactcl),
  #taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind))

#Check levels for factors (to see if they should be collapsed later)
levels(P_clean$StreamType)
levels(P_clean$taxpartsize)
levels(P_clean$WetlandType)
levels(P_clean$NLCD06Cat)
levels(P_clean$hydricrating)
levels(P_clean$drainagecl)
levels(P_clean$taxpartsize)
levels(P_clean$texcl)
levels(P_clean$pmkind)

View(summary(P_clean))


#DATA SUMMARY (FOR PAPER)

#number of surface and subsurface samples
nrow(P_clean)
nrow(filter(P_clean, Depth_cm>30))
nrow(filter(P_clean, Depth_cm<=30))

#mean soil P for surface and subsurface
filter(P_clean, Depth_cm<=30) %>%
  summarise(mean(P_mgkg))

#number of samples with high soil P >1000 mg/kg
nrow(filter(P_clean, P_mgkg>1000))
  ggplot(filter(P_clean, P_mgkg>1000))+
           geom_point(aes(Depth_cm, P_mgkg))


###########################################
###
#SPLIT DATA INTO TRAINING/TESTING SETS
DATA<-P_clean
samplesize = 0.90*nrow(DATA)
set.seed(100)
index = sample(seq_len(nrow(DATA)), size = samplesize)
#Creating training and test set 
datatrain = DATA[index,]
datatest = DATA[-index,]

nrow(datatrain) #n=6200
nrow(datatest)  #n=689

#Check P distribution for training and test datasets

summary(datatrain$P_mgkg)
summary(datatest$P_mgkg)

nrow(datatrain[datatrain$P_mgkg>2000,]) #n=14
nrow(datatest[datatest$P_mgkg>2000,]) #n=5

#note: both datasets show 'outliers' with P>2000



########################################
###MISSING VALUES FOR REMAINING COVARIATES
#use missRanger to interpolate
#see https://cran.r-project.org/web/packages/missRanger/vignettes/missRanger.html
#also for a helpful tutorial see: https://www.youtube.com/watch?v=8VzfFPUE1Dk
#option to impute ahead of time, or within tidymodels framework
#i'm going to impute separately i think it saves computation time

library(missRanger)

#By default missRanger uses all columns in the data set to impute all columns with missings. 

#exclude P_mgkg from imputation
#first impute training dataset
#then use imputed training dataset to impute test dataset

#note: this takes a pretty long time! ~25 min, for num.trees=100
#(check system time to see how long it takes, and then run code)
#Separate out predictor variables:
names(datatrain)
Predictors<-datatrain[,c(2:268)]
names(Predictors)
nrow(Predictors)

#R with parameters set to run faster
#no pmm.k, num.trees=100; took ~ 3.5 min

start_time <- Sys.time()
start_time
dataImputed <- missRanger(Predictors,
                          formula = . ~ . ,
                          num.trees = 100, 
                          verbose = 1, seed = 111)
head(dataImputed)

end_time <- Sys.time()
end_time - start_time

nrow(dataImputed)



#rematch to BCG scores
P.DATA<-datatrain[,c(1)]
P.Predictors<-cbind(P.DATA, dataImputed)
head(P.Predictors)

summary(P.Predictors)

#WRITE IMPUTED DATA TO FILE
#(note this is training data only)
write.table(P.Predictors, "./P_Predictors_imputed.csv", sep=",", row.names=FALSE)

###
#Impute *testing data* separately (for use in model testing later)
#read in imputed training data if needed
P.Predictors<-read.delim('./P_Predictors_imputed.csv', sep=",", header=TRUE)
head(P.Predictors)
colnames(P.Predictors)[1]<-P_mgkg

names(datatest)

#recreate dataImputed
dataImputed<-P.Predictors[,c(2:268)]

nrow(datatest)
Test.impute<-rbind(datatest[,c(2:268)], dataImputed)
nrow(Test.impute)
View(Test.impute)

#Impute missing values for test data (combined with imputed training data)
start_time <- Sys.time()
start_time
data.Test.Imputed <- missRanger(Test.impute,
                                formula = . ~ . ,
                                num.trees = 100, 
                                verbose = 1, seed = 111)
head(data.Test.Imputed)

end_time <- Sys.time()
end_time - start_time

#select out test data; and attach to BCG scores
test<-data.Test.Imputed[c(1:689),]
P.test<-datatest[,c(1)]
P.test
test.prep<-cbind(P.test, test)
nrow(test.prep)
head(test.prep)

write.table(test.prep, "./P_TEST_imputed.csv", sep=",", row.names=FALSE)

names(test.prep)
#check these 3 variables
SuperfundDensCat
SuperfundDensCatR100
MineDensCat

summary(test.prep[,c(86, 92, 97)])
#Ok values not all zeroes

###########################################
##CREATE MODELS

#load packages if didn't already above;
library(tidyverse)
library(tidymodels)
library(workflows)
library(tune)
library(ranger)

#read in imputed data

#training data:
P.Predictors<-read.delim("./P_Predictors_imputed.csv",sep=",", header=TRUE)
head(P.Predictors)
nrow(P.Predictors)

#independent test data:
test.prep<-read.delim("./P_TEST_imputed.csv", sep=",", header=TRUE)
head(test.prep)
nrow(test.prep)

#if starting from here, reformat variables to correct type
#format covariates to correct class:
P.Predictors <- transform(
  P.Predictors,
  P_mgkg=as.numeric(P_mgkg),
  slope_r=as.numeric(slope_r),
  elev_r=as.numeric(elev_r),
  airtempa_r=as.numeric(airtempa_r),
  ffd_r=as.numeric(ffd_r),
  comppct_r=as.numeric(comppct_r),
  soc150_999=as.numeric(soc150_999),
  sieveno4_r=as.numeric(sieveno4_r),
  sieveno10_r=as.numeric(sieveno10_r),
  sieveno40_r=as.numeric(sieveno40_r),
  sieveno200_r=as.numeric(sieveno200_r),
  sandtotal_r=as.numeric(sandtotal_r),
  silttotal_r=as.numeric(silttotal_r),
  claytotal_r=as.numeric(claytotal_r),
  om_r=as.numeric(om_r),
  ksat_r=as.numeric(ksat_r),
  kffact=as.numeric(kffact),
  caco3_r=as.numeric(caco3_r),
  gypsum_r=as.numeric(gypsum_r),
  aws0_5 = as.numeric(aws0_5), 
  aws5_20=as.numeric(aws5_20),
  aws20_50= as.numeric(aws20_50), 
  aws50_100=as.numeric(aws50_100), 
  aws100_150=as.numeric(aws100_150), 
  aws0_20=as.numeric(aws0_20),
  aws0_30=as.numeric(aws0_30),
  aws0_100=as.numeric(aws0_100), 
  aws0_150=as.numeric(aws0_150) , 
  aws0_999=as.numeric(aws0_999), 
  soc0_5=as.numeric(soc0_5), 
  soc5_20=as.numeric(soc5_20),
  soc20_50=as.numeric(soc20_50), 
  soc50_100=as.numeric(soc50_100),
  soc100_150=as.numeric(soc100_150), 
  soc0_20=as.numeric(soc0_20), 
  soc0_30=as.numeric(soc0_30),
  soc0_100=as.numeric(soc0_100), 
  soc0_150=as.numeric(soc0_150),
  soc0_999=as.numeric(soc0_999), 
  nccpi3corn=as.numeric(nccpi3corn),
  nccpi3soy=as.numeric(nccpi3soy), 
  nccpi3cot=as.numeric(nccpi3cot),
  nccpi3sg=as.numeric(nccpi3sg), 
  nccpi3all=as.numeric(nccpi3all),
  pctearthmc=as.numeric(pctearthmc), 
  rootznemc=as.numeric(rootznemc),
  rootznaws=as.numeric(rootznaws), 
  droughty=as.numeric(droughty),
  Depth_cm=as.numeric(Depth_cm)
)


#transform categorical variables
P.Predictors <- transform(
  P.Predictors,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  taxpartsize=as.factor(taxpartsize),
  taxceactcl=as.factor(taxceactcl),
  #taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind))

#Check levels for factors (to see if they should be collapsed later)
levels(P.Predictors$StreamType)
levels(P.Predictors$taxpartsize)
levels(P.Predictors$WetlandType)
levels(P.Predictors$NLCD06Cat)
levels(P.Predictors$hydricrating)
levels(P.Predictors$drainagecl)
levels(P.Predictors$taxpartsize)
levels(P.Predictors$texcl)
levels(P.Predictors$pmkind)

View(summary(P.Predictors))


test.prep <- transform(
  test.prep,
  P_mgkg=as.numeric(P_mgkg),
  slope_r=as.numeric(slope_r),
  elev_r=as.numeric(elev_r),
  airtempa_r=as.numeric(airtempa_r),
  ffd_r=as.numeric(ffd_r),
  comppct_r=as.numeric(comppct_r),
  soc150_999=as.numeric(soc150_999),
  sieveno4_r=as.numeric(sieveno4_r),
  sieveno10_r=as.numeric(sieveno10_r),
  sieveno40_r=as.numeric(sieveno40_r),
  sieveno200_r=as.numeric(sieveno200_r),
  sandtotal_r=as.numeric(sandtotal_r),
  silttotal_r=as.numeric(silttotal_r),
  claytotal_r=as.numeric(claytotal_r),
  om_r=as.numeric(om_r),
  ksat_r=as.numeric(ksat_r),
  kffact=as.numeric(kffact),
  caco3_r=as.numeric(caco3_r),
  gypsum_r=as.numeric(gypsum_r),
  aws0_5 = as.numeric(aws0_5), 
  aws5_20=as.numeric(aws5_20),
  aws20_50= as.numeric(aws20_50), 
  aws50_100=as.numeric(aws50_100), 
  aws100_150=as.numeric(aws100_150), 
  aws0_20=as.numeric(aws0_20),
  aws0_30=as.numeric(aws0_30),
  aws0_100=as.numeric(aws0_100), 
  aws0_150=as.numeric(aws0_150) , 
  aws0_999=as.numeric(aws0_999), 
  soc0_5=as.numeric(soc0_5), 
  soc5_20=as.numeric(soc5_20),
  soc20_50=as.numeric(soc20_50), 
  soc50_100=as.numeric(soc50_100),
  soc100_150=as.numeric(soc100_150), 
  soc0_20=as.numeric(soc0_20), 
  soc0_30=as.numeric(soc0_30),
  soc0_100=as.numeric(soc0_100), 
  soc0_150=as.numeric(soc0_150),
  soc0_999=as.numeric(soc0_999), 
  nccpi3corn=as.numeric(nccpi3corn),
  nccpi3soy=as.numeric(nccpi3soy), 
  nccpi3cot=as.numeric(nccpi3cot),
  nccpi3sg=as.numeric(nccpi3sg), 
  nccpi3all=as.numeric(nccpi3all),
  pctearthmc=as.numeric(pctearthmc), 
  rootznemc=as.numeric(rootznemc),
  rootznaws=as.numeric(rootznaws), 
  droughty=as.numeric(droughty),
  Depth_cm=as.numeric(Depth_cm)
)


#transform categorical variables
test.prep <- transform(
 test.prep,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  taxpartsize=as.factor(taxpartsize),
  taxceactcl=as.factor(taxceactcl),
  #taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind))

#Check levels for factors (to see if they should be collapsed later)
levels(P.Predictors$StreamType)
levels(P.Predictors$taxpartsize)
levels(P.Predictors$WetlandType)
levels(P.Predictors$NLCD06Cat)
levels(P.Predictors$hydricrating)
levels(P.Predictors$drainagecl)
levels(P.Predictors$taxpartsize)
levels(P.Predictors$texcl)
levels(P.Predictors$pmkind)

View(summary(P.Predictors))
####



###
#DEFINE A RECIPE (ie, a model, including pre-processing)
#standardize predictors (according to correspondence with Hadi on 12/6)
#note nominal variables include factor and character variables
#not convinced you need dummy variables for rf, but trying it here
#update 3/7/22 - not using step_dummy b/c this is reported to affect model run time
#see https://community.rstudio.com/t/tidymodels-slow-hyperparameter-tuning-with-wide-data-and-group-cv-fold/108087/5
#see https://juliasilge.com/blog/sf-trees-random-tuning/ for helpful processing steps
#not collapsing categorical levels because none of them have a crazy high number of levels

#Note: this says best practice is to scale only using the training dataset - 
#https://datascience.stackexchange.com/questions/39932/feature-scaling-both-training-and-test-data
#If you use the whole dataset to figure out the feature mean and variance, 
#you're using knowledge about the distribution of the test set to set the scale of the training set - 'leaking' information.
#then you can "bake" out the centered/scaled test data later, when you need to test independent randomForest models...

#rename P column to match for train and test
names(P.Predictors)
colnames(P.Predictors)[1]<- "P_mgkg"
colnames(test.prep)[1] <-"P_mgkg"
names(test.prep)


P_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(P_mgkg ~ ., data = P.Predictors) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) 
#Other options excluded for now:
#%>%
  #step_dummy(all_nominal()) #%>%
  #step_impute_bag(all_predictors(), trees=100) #trying to impute missing values, using all other predictors

P_recipe


###
#EXTRACT THE PREPROCESSED DATASET - skip if already did this
P_train_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors) %>%
  # extract the pre-processed training dataset
  juice()
P_train_preprocessed
View(P_train_preprocessed)

#write pre-processed training data to file
write.table(as.data.frame(P_train_preprocessed), "./P_train_preprocessed.csv", sep=",", row.names=FALSE)
names(P_train_preprocessed)

#Important: How you prep the test data:
#https://stackoverflow.com/questions/62189885/what-is-the-difference-among-prep-bake-juice-in-the-r-package-recipes
#The bake() function takes a prepped recipe (one that has had all quantities estimated from training data) 
#and applies it to new_data. 
#That new_data could be the training data again...
#Or it could be the testing data. In this case, the column means from the training data are applied to the testing data, because that is what happens IRL in a modeling workflow. 
#To do otherwise is data leakage.

#So I need to bake() and not juice
P_test_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors) %>%
  # extract the pre-processed *testing) dataset
  bake(new_data=test.prep)
View(P_test_preprocessed)

#Check for NAs
which(is.na(P_test_preprocessed), arr.ind=TRUE)
summary(P.Predictors$pmkind)

#write pre-processed testing data to file, so can use later for model evaluation
write.table(as.data.frame(P_test_preprocessed), "./P_test_preprocessed.csv", sep=",", row.names=FALSE)



###


###
#SPECIFY THE MODEL

#here's how Julia Silge does it: 
#https://juliasilge.com/blog/sf-trees-random-tuning/
#do you need to tune the number of trees?

#note: tuning the number of trees is not necessary
#see https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
#"Tuning the number of trees is unnecessary; instead, simply set the number of trees to a large, computationally feasible number, and let the asymptotic behavior of LLN do the rest."

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")


###
#PUT EVERYTHING TOGETHER IN A WORKFLOW
#hm, is imputation going to add a lot to overall tuning time?

tune_wf <- workflow() %>%
  add_recipe(P_recipe) %>%
  add_model(tune_spec)


###
#TUNE THE PARAMETERS
#WHAT METRICS DO I WANT FOR REGRESSION?
#some helpful things here: https://towardsdatascience.com/dials-tune-and-parsnip-tidymodels-way-to-create-and-tune-model-parameters-c97ba31d6173

#Julia Silge method
#see https://juliasilge.com/blog/sf-trees-random-tuning/
#specify cross validation for the training dataset, note that default is k=10 folds

set.seed(234)
P_folds <- vfold_cv(P.Predictors)

#do parallel processing to make it run faster:

#doParallel::registerDoParallel()

#Check start time for initial tuning
start_time <- Sys.time()
start_time

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = P_folds,
  grid = 20,
control = control_grid(verbose = TRUE)
)
tune_res

#Check end time for initial tuning: 
end_time <- Sys.time()
end_time - start_time

#save the tune results
saveRDS(tune_res, "./rf_tune_results_INITIAL_ranger.rds")
#load in tune results!
tune_res<-readRDS("./rf_tune_results_INITIAL_ranger.rds")
tune_res

#Check out initial tuning results
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  arrange(mean) %>%
  print(n=40) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")

#best rsq is 0.452

#find the best set:
best_rmse1<-select_best(tune_res, "rmse")
best_rmse1
#mtry = 50; min_n=15

#Option: Tune one more time, narrowing in on good tuning
#Note: for now, not selecting this option (as haven't seen much increase
#in performance previously)

rf_grid <- grid_regular(
  mtry(range = c(55,65)),
  min_n(range = c(15, 20)),
  levels = 3
)

rf_grid %>% 
  print(n=9)

#Check start time for fine tuning
start_time <- Sys.time()
start_time
set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = P_folds,
  grid = rf_grid,
  control = control_grid(verbose = TRUE)
)

regular_res
#save the tune results
saveRDS(regular_res, "./rf_fine_tuned_results_ranger_standardized_surfaceonly_4_28_22.rds")


#Check end time for fine tuning: 
end_time <- Sys.time()
end_time - start_time

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "R2")

best_rmse <- select_best(regular_res, "rsq")
best_rmse
#note - not better than the initial tuned value of
#mtry =61, min_n=17


################

#FINALIZE THE WORKFLOW

best_rmse <- select_best(tune_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf

final_wf <- workflow() %>%
  add_recipe(recipe(P_mgkg ~ ., data = P_train_preprocessed)) %>%
  add_model(final_rf)

rf_fit <- final_wf %>%
  fit(P_train_preprocessed)
rf_fit

#SAVE FINAL MODEL
saveRDS(rf_fit, "./rf_fit.ranger_MODEL_training.rds")


#predict soil P for the test dataset
rf_pred <-predict(rf_fit, new_data=P_test_preprocessed)
rf_pred  
#add predictions to test data:
test.pred<-as.data.frame(cbind(P_test_preprocessed, rf_pred))

#check R2 value for actual vs predicted
reg<- with(test.pred,lm(P_mgkg~.pred))
summary(reg)

#unscale by multiplying values by the standard deviation and adding the mean
#should this be the mean and standard deviation of the test dataset, or the whole dataset?
#i think whole dataset, as that was used to scale??
test.pred$P_mgkg_unscaled<-test.pred$P_mgkg* sd(P.Predictors$P_mgkg) + mean(P.Predictors$P_mgkg)
head(test.pred)
test.pred$pred_unscaled<-test.pred$.pred* sd(P.Predictors$P_mgkg) + mean(P.Predictors$P_mgkg)
head(test.pred)
nrow(test.pred)

#View actual vs predicted soil P for test data
ActualvsPredict.plot<-
  ggplot(test.pred)+
  geom_point(aes(P_mgkg_unscaled, pred_unscaled))+
  #scaled and centered data:
  #geom_point(aes(P_mgkg, .pred))+
  geom_abline(intercept=0, slope=1)+
  xlim(0,4500)+
  ylim(0,4500)+
  theme_bw()+
  xlab("Actual P (mg/kg)") +
  ylab ("Predicted P (mg/kg)") +
  theme_bw()+
  theme(axis.title=element_text(size=24), axis.text=element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ActualvsPredict.plot

#check R2 if exclude very high P values;
reg2<- with(test.pred[test.pred$P_mgkg_unscaled<2000,],lm(P_mgkg~.pred))
summary(reg2)
#R2 is 0.5 excluding values >1000 mg/kg
#R2 is 0.52 excluding values >2000 mg/kg

#calculate rmse
RMSE = mean((test.pred$P_mgkg - test.pred$.pred)^2) %>% sqrt()
RMSE

#number of samples in test dataset with P >1000
nrow(filter(test.pred, P_mgkg_unscaled>1000))

#write fig to file
tiff("./Actual_vs_model_predicted_P_test_data.tiff", units="in", width=8, height=8, res=300)
ActualvsPredict.plot
dev.off()


############################################################
#COMPARE RANGER AND RANDOM FOREST MODELS

short_recipe<-
  recipe(P_mgkg ~ ., data = P_train_preprocessed) 

#specify model (equivalent was tune_spec)
short_model_RANGER<-
  rand_forest(mtry = 50, min_n=15, trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

short_model_RF<-
  rand_forest(
    mtry = 50, 
    min_n=15, 
    trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

#put into workflow
short_wf_ranger<-workflow()%>%
  add_recipe(short_recipe) %>%
  add_model(short_model_RANGER)

short_wf_RF<-workflow()%>%
  add_recipe(short_recipe) %>%
  add_model(short_model_RF)


#run models
ranger_fit<-short_wf_ranger %>%
  fit(P_train_preprocessed)
ranger_fit
summary(ranger_fit)

RF_fit<-short_wf_RF %>%
  fit(P_train_preprocessed)
RF_fit
summary(RF_fit)


#Check performance
ranger_pred <-predict(ranger_fit, new_data=P_test_preprocessed)
randfor_pred<-predict(RF_fit, new_data=P_test_preprocessed) 
#add predictions to test data:
ranger.pred<-as.data.frame(cbind(P_test_preprocessed, ranger_pred))
head(ranger.pred)
RF.pred<-as.data.frame(cbind(P_test_preprocessed, randfor_pred))

#check R2 value for actual vs predicted
reg2<- with(ranger.pred,lm(P_mgkg~.pred))
summary(reg2)
reg3<- with(RF.pred,lm(P_mgkg~.pred))
summary(reg3)

#the test results match very closely



#VARIABLE IMPORTANCE (using permimp)
#see https://cran.r-project.org/web/packages/permimp/vignettes/permimp-package.html
library(party)
library(permimp)

#need a randomForest object
#need to set hyperparameters to those tuned by ranger previously 
#note in tidy models for engine rand_forest:
#min_n = An integer for the minimum number of data points in a node that are required for the node to be split further.
#is this equivalent in randomForest to nodesize = Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
#note: need pre-processed (centered, scaled) train and test data (created in recipe above):

library(randomForest)

start_time <- Sys.time()
start_time

set.seed(542863)
rfP_model <- randomForest(P_mgkg ~ ., data = as.data.frame(P_train_preprocessed), mtry = 50, ntree=1000, replace = FALSE, 
                          nodesize = 15
                          , keep.forest = TRUE, keep.inbag = TRUE)
summary(rfP_model)

#SAVE FINAL RANDOM FOREST MODEL!!
#SAVE FINAL MODEL
saveRDS(rfP_model, "./FINALMODEL_randomForest.rds")

end_time <- Sys.time()
end_time - start_time


#Check performance of the model (should match performance of the random_Forest() model tuned above, using tidymodels?)
rf.pred<-predict(rfP_model, newdata=P_test_preprocessed)
head(rf.pred)

#Match actual to predicted data:
predicted.P<-cbind(as.data.frame(P_test_preprocessed), rf.pred)
head(predicted.P)

#actual vs predicted
reg.check<-with(predicted.P, lm(P_mgkg~rf.pred))
summary(reg.check)
#it matches! R2=0.30
#calculate rmse
RMSE = mean((predicted.P$P_mgkg - predicted.P$rf.pred)^2) %>% sqrt()
RMSE


##RUN CONDITIONAL PERMUTATION IMPORTANCE:
#read in saved model if needed;
rfP_model<-readRDS("./FINALMODEL_randomforest.rds")

start_time <- Sys.time()
start_time

CPI_permimpRF <- permimp(rfP_model, conditional = TRUE, progressBar = TRUE)
CPI_permimpRF
end_time <- Sys.time()
end_time - start_time

#save permimp object!
saveRDS(CPI_permimpRF, "./permimp_results.rds")





#sort by importance:
plot(CPI_permimpRF, type = "box", horizontal = TRUE)


#Rank importance values and plot a subset:
#Make a dot plot:
library(dplyr)
library(ggplot2)
VarImp<-as.data.frame(sort(CPI_permimpRF$values, decreasing=TRUE), optional=T)
colnames(VarImp)[1]<-"Imp"
rownames(VarImp)
VarImp2 <- cbind(rownames(VarImp), data.frame(VarImp, row.names=NULL))
colnames(VarImp2)[1]<-"Var"
head(VarImp2)

VarImp2[1:25,]

Importance.plot<-
  VarImp2[1:25,] %>% 
  #slice(25, log10(Imp)) %>% 
  ggplot() + aes(x=reorder(Var, Imp), y=log10(Imp)) + geom_point()+coord_flip()+
  ylab("Importance (log10 scale)")+
  xlab("Covariate")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(text = element_text(size = 14))
Importance.plot

#write fig to file
tiff("./Covariate_importance_plot_top25.tiff", units="in", width=10, height=8, res=300)
Importance.plot
dev.off()


###
