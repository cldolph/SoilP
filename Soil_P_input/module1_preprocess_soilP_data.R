

#Module for pre-processing soil P data from USGS National Geochemical Survey
#and National Soil Characterization Dataset

#Updated 9/6/22

#######################################################################################
#######################################################################################
#STEP 1: CLEANING RAW SOIL P DATA 
#Soil P Data sources: 
#1. USGS National Geochemical Survey
#2. National Soil Characterization Dataset

#read in USGS geochem dataset
#from https://mrdata.usgs.gov/geochem/

geochem<-read.delim('geochem.csv', sep=",", header=TRUE)
head(geochem)
names(geochem)
nrow(geochem)
class(geochem)

geochem<-as_tibble(geochem)
geochem

soil<-geochem %>% 
  filter(SOURCE=="Soil"|SOURCE=="Glacial till") %>%  #subset to soil samples (excluding stream bed samples)
  drop_na(LATITUDE)%>% #remove NAs from Lat/long 
  select(REC_NO, COLL_DATE, GRAINSIZE, SOIL_HORIZ, SETTING, RELIEF, VEG, LATITUDE, LONGITUDE,
          HUC_8, TYPEDESC, SOURCE, STUDY, PH, P_ICP40, C_TOT, C_ORG) %>% #remove unneeded columns
  mutate(Date = as.Date.character(COLL_DATE, format = "%Y%m%d")) %>% #format Date
  filter(!is.na(P_ICP40)) %>% #remove rows with no P data 
  mutate(P_mgkg = P_ICP40*10000)#convert P units from % weight to mg/kg

#View(soil)

#ID date range of P dataset
summary(soil$Date)
#latest year listed as 2303 - data entry error
#median date is 2003
#remove rows with no Date available
#remove rows with 2303 listed as the year (assume error)
#goal is to model recent soil P content - 
#subset to data collected after 2000
soil2<-soil %>% 
  drop_na(Date) %>% 
  filter(Date<"2022-01-01"& Date>"2000-01-01")
summary(soil2$Date)

#check how many records
nrow(soil2)
#n=9864
#note that you pick up ~1000 more samples if you date back to 1990


##CLEAN UP SOIL DEPTH INFO
#note that P below the surface layer may not be representative of farming practices (as per Ringeval et al 2017)
#they define 'surface' layer as 0-0.3 meters (equivalent to ~ 30cm, or 11.8 inches)

#SOIL_HORIZ column gives approx depth
#**NOTE that some of the samples are repeat samples at different depths for a given location; 
#want ability to subset to surface samples
#the format of the SOIL_HORIZ column is a mess - all different formats
# some exact depths (in inches), some ranges, some just qualitative categories (e.g., A or B), and some blank

#Cleaning up soil depth info
nrow(soil2)
soil2 %>% 
  filter(is.na(SOIL_HORIZ)) #check for NAs; n=0
 
soil2 %>% 
  filter(SOIL_HORIZ =="")  #check for blanks/missing values; n=436
   
#how many different categories
levels(factor(soil2$SOIL_HORIZ))
#622 different depth categories; 

#first take out "in" from all columns (i.e., indicating measurement in inches)
soil2$Depth<-soil2$SOIL_HORIZ
#remove "in"
soil2$Depth<- gsub('in',"",soil2$Depth)
#levels(factor(soil2$Depth))
#substite "-" for "to" to make uniform across rows
soil2$Depth<- gsub(' to ', "-", soil2$Depth)
#levels(factor(soil2$Depth))

#for soil horizons with letters (rather than numeric depth measurements):
#A few things to note: 
#some samples apper to be a mix of multiple horizons; e.g., "A & B/E" 
#some have depth measurements and some do not
#retain those with depth measurements and use those depths 

#Pull out all values that include actual depth measurements first:
#remove A values with depth measurements so you can just apply numbers:
soil2$Depth<- gsub('A, ', "", soil2$Depth)
#levels(factor(soil2$Depth))
#remove Ap values with depth measurements so you can just apply numbers:
soil2$Depth<- gsub('Ap, ', "", soil2$Depth)
#levels(factor(soil2$Depth))
#remove Abk values with depth measurements so you can just apply numbers:
soil2$Depth<- gsub('Abk, ', "", soil2$Depth)
#levels(factor(soil2$Depth))
#remove Ak values with depth measurements so you can just apply numbers:
soil2$Depth<- gsub('Ak, ', "", soil2$Depth)
#levels(factor(soil2$Depth))
#remove Akp values with depth measurements so you can just apply numbers:
soil2$Depth<- gsub('Akp, ', "", soil2$Depth)
#levels(factor(soil2$Depth))
#remove Apk values with depth measurements so you can just apply numbers:
soil2$Depth<- gsub('Apk, ', "", soil2$Depth)
#levels(factor(soil2$Depth))
#Other horizon categories with depth info associated:
soil2$Depth<- gsub('A & ABk, ', "", soil2$Depth)
soil2$Depth<- gsub('A & ABk1, ', "", soil2$Depth)
soil2$Depth<- gsub('A & AC, ', "", soil2$Depth)
soil2$Depth<- gsub('A & Bk1, ', "", soil2$Depth)
soil2$Depth<-gsub('A & Bt, ', "", soil2$Depth)
soil2$Depth<- gsub('A & Bw, ', "", soil2$Depth)
soil2$Depth<- gsub('A & Bw1, ', "", soil2$Depth)
soil2$Depth<- gsub('A & C, ', "", soil2$Depth)
soil2$Depth<- gsub('Ap & Bw, ', "", soil2$Depth)
soil2$Depth<- gsub('BCk, ', "", soil2$Depth)
soil2$Depth<- gsub('Bg,, ', "", soil2$Depth)
soil2$Depth<- gsub('Bk, ', "", soil2$Depth)
soil2$Depth<- gsub('Bk1, ', "", soil2$Depth)
soil2$Depth<- gsub('Bk2, ', "", soil2$Depth)
soil2$Depth<- gsub('Bkg, ', "", soil2$Depth)
soil2$Depth<- gsub('Bt, ', "", soil2$Depth)
soil2$Depth<- gsub('Bt1, ', "", soil2$Depth)
soil2$Depth<- gsub('Btk, ', "", soil2$Depth)
soil2$Depth<- gsub('Btn, ', "", soil2$Depth)
soil2$Depth<- gsub('Btnk, ', "", soil2$Depth)
soil2$Depth<- gsub('Btnk2, ', "", soil2$Depth)
soil2$Depth<- gsub('Btnky, ', "", soil2$Depth)
soil2$Depth<- gsub('Bw, ', "", soil2$Depth)
soil2$Depth<- gsub('Bw1, ', "", soil2$Depth)
soil2$Depth<- gsub('Bw2, ', "", soil2$Depth)
soil2$Depth<- gsub('Bwg, ', "", soil2$Depth)
soil2$Depth<- gsub('C, ', "", soil2$Depth)
soil2$Depth<- gsub('C1, ', "", soil2$Depth)
soil2$Depth<- gsub('C3, ', "", soil2$Depth)
soil2$Depth<- gsub('Cr, ', "", soil2$Depth)
soil2$Depth<- gsub('E & Bk1, ', "", soil2$Depth)
soil2$Depth<- gsub('E &Btny, ', "", soil2$Depth)
soil2$Depth<- gsub('E, ', "", soil2$Depth)
soil2$Depth<- gsub('2Bky, ', "", soil2$Depth)
soil2$Depth<- gsub('Bg, ', "", soil2$Depth)


#Check remaining levels:
levels(factor(soil2$Depth))

##Other samples without numeric depth info:

##assume any categories without depth & containing A are 'surface'
#(tho note some appear to be a mix of surface and subsurface)
soil2$Depth <- ifelse(grepl("A",soil2$Depth),'surface',soil2$Depth)
levels(factor(soil2$Depth))

#assign all rows without depth containing primarily "B" or "C" or "E" to 'subsurface'
soil2$Depth <- ifelse(grepl("B",soil2$Depth),'subsurface',soil2$Depth)
#levels(factor(soil2$Depth))
soil2$Depth <- ifelse(grepl("C",soil2$Depth),'subsurface',soil2$Depth)
#levels(factor(soil2$Depth))
soil2$Depth <- ifelse(grepl("E",soil2$Depth),'subsurface',soil2$Depth)
#levels(factor(soil2$Depth))


#Other random categories to flag because numeric depth is unclear:
soil2$Depth <- ifelse(grepl("MIX",soil2$Depth),'unknown depth',soil2$Depth)
#levels(factor(soil2$Depth))
soil2$Depth <- ifelse(grepl("None",soil2$Depth),'unknown depth',soil2$Depth)
#levels(factor(soil2$Depth))
soil2$Depth <- ifelse(grepl("S",soil2$Depth),'unknown depth',soil2$Depth)
#levels(factor(soil2$Depth)) 

#Additional clean up:
#replace commas with "-"
soil2$Depth<- gsub(',', "-", soil2$Depth)
#samples with "+" symbol: note these samples don't have a bottom depth specified;
#better to omit from dataset for consistency
soil2 %>% 
  filter(!str_detect(Depth, "\\+")) # 6 samples total
soil2<-soil2 %>% 
  filter(!str_detect(Depth, "\\+"))#remove from dataset
#remove random characters:
soil2$Depth<- gsub('i', "", soil2$Depth)
#replace blank spaces with no space:
soil2$Depth<- gsub(' ', "", soil2$Depth)
#levels(factor(soil2$Depth))

#Now remaining values are all numbers, except for the categories 'surface', 'subsurface', and 'unknown depth', and the initially missing (blank) values - 
#how many rows for each of those categories?
nrow(soil2)
nrow(soil2[soil2$Depth=="unknown depth"|soil2$Depth=="surface"|soil2$Depth=="subsurface",]) #n=2572
#levels(factor(soil2$Depth))
nrow(soil2[soil2$Depth=="surface",]) #n=1320
#levels(factor(soil2$Depth))
#how many blank rows 
nrow(soil2[soil2$Depth=="",]) #n=449 #pickup 13 additional blank rows


levels(factor(soil2$Depth))

#Make a column with 'NA' assigned to subsurface/surface/unknown (to be reassigned below)
soil2$surface<- gsub('unknowndepth', "NA", soil2$Depth)
soil2$surface<- gsub('subsurface', "NA", soil2$surface)
soil2$surface<- gsub('surface', "NA", soil2$surface)
#levels(factor(soil2$surface))

#For samples that give a range for depth, create min and max depth columns
#library(stringr)
range<-as.data.frame(str_split_fixed(soil2$surface, "-", 2))
#range
head(range)
colnames(range)[1]<-"min_depth"
colnames(range)[2]<-"max_depth"
#head(range)

soil3<-as.data.frame(cbind(soil2, range))
#head(soil3)

#Assign depths as numeric:
#Can ignore warning about NA's introduced
options(warn=-1)
soil3$min_depth<-as.numeric(soil3$min_depth)
soil3$max_depth<-as.numeric(soil3$max_depth)
options(warn=0)
head(soil3)
nrow(soil3)
#check number of NAs 
nrow(soil3[is.na(soil3$min_depth),])

#range of soil depths
summary(soil3$max_depth)


##ADDING IN MISSING SOIL DEPTH VALUES:
##Interpolating soil depth values where no specific depth was listed originally (for keeping soil depth as a covariate)
##ASSUMPTIONS:
#Use "max_depth" as the covariate for depth, while also accounting for missing values for depth
#for sites listed as 'surface' with no numeric depth measurement, assign a certain depth - 12 inches for max depth (i.e.,  <31 cm)
#for sites with no depth listed, assume surface -> And list assigned surface depth as 12 inches for max depth
#for sites listed as 'subsurface', ie, any horizon except A --> assigned 30 *inches* for max depth #because 99% of subsurface samples 
#that did contain depth information had a maximum depth at or below this level (corresponding primarily to soil horizon B). 
#depth unit for all samples was assumed to be inches in all cases because where units were listed
#in depth column they were "in"


soil.depth<-soil3
nrow(soil3)
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
#30 inches is typically down to the end of the B horizon
#most of the samples with listed subsurface soil horizons listed were B


#what is average depth for samples in subsurface (below 12 inches), that do have depth info
names(soil.depth)
subsurface<-soil.depth[soil.depth$max_depth>12&!is.na(soil.depth$max_depth),]
nrow(subsurface)
summary(subsurface$max_depth)
class(subsurface$max_depth)

#mean depth of all subsurface samples in 20 inches

#max depth of all soil samples listed is 88
#let's plot a histogram of soil depth
#require(ggplot2)
#ggplot(subsurface)+
 # geom_histogram(aes(x=max_depth))

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

nrow(soil.depth)
#Omit Glacial till samples to focus on surface soil 
Soils<-soil.depth[soil.depth$SOURCE=="Soil",]
nrow(Soils)


#Write data with assumed depths to table:
setwd(output_dir)
write.table(Soils, 'soil_USGSgeochem_since2000_assumed_depths.csv', sep=",", row.names=FALSE)
