#Module for pre-processing SSURGO attributes for use as covariates in the random forest model

#Updated 9/6/22

#Read in ArcGIS dbf attribute tables 

#Download and unzip state gSSURGO files from https://nrcs.app.box.com/v/soils/folder/148414960239
#to the project directory 'gSSURGO'
#states needed: AR, IA, IL, IN, KS, MN, MO, ND, NE, OH, SD, WI

#load relevant tables from state geodatabases:

#can ignore warnings about simple feature geometries not being present
options(warn=-1)

#'Component' Table

AR <- sf::st_read(dsn = "./gSSURGO/gSSURGO_AR.gdb", layer = "component")
IA <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IA.gdb", layer = "component")
IL <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IL.gdb", layer = "component")
IN <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IN.gdb", layer = "component")
KS <- sf::st_read(dsn = "./gSSURGO/gSSURGO_KS.gdb", layer = "component")
MN <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MN.gdb", layer = "component")
MO <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MO.gdb", layer = "component")
ND <- sf::st_read(dsn = "./gSSURGO/gSSURGO_ND.gdb", layer = "component")
NE <- sf::st_read(dsn = "./gSSURGO/gSSURGO_NE.gdb", layer = "component")
OH <- sf::st_read(dsn = "./gSSURGO/gSSURGO_OH.gdb", layer = "component")
SD <- sf::st_read(dsn = "./gSSURGO/gSSURGO_SD.gdb", layer = "component")
WI <- sf::st_read(dsn = "./gSSURGO/gSSURGO_WI.gdb", layer = "component")

#Append all tables using rbind
Component<-rbind(AR, IA, IL, IN, KS, MN, MO, ND, NE, OH, SD, WI)
nrow(Component)
head(Component)

#check if cokeys are unique
length(Component$cokey)
length(unique(Component$cokey))
#Ok same length - so are unique

#'Chorizon' Table
AR2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_AR.gdb", layer = "chorizon")
IA2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IA.gdb", layer = "chorizon")
IL2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IL.gdb", layer = "chorizon")
IN2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IN.gdb", layer = "chorizon")
KS2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_KS.gdb", layer = "chorizon")
MN2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MN.gdb", layer = "chorizon")
MO2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MO.gdb", layer = "chorizon")
ND2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_ND.gdb", layer = "chorizon")
NE2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_NE.gdb", layer = "chorizon")
OH2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_OH.gdb", layer = "chorizon")
SD2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_SD.gdb", layer = "chorizon")
WI2 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_WI.gdb", layer = "chorizon")

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
#from gSSURGO Table Column report: hzdepb_r - the distance from the top of the soil to the base of the soil horizon
#note: table report notes depth units as cm

#you need one set of attributes per map unit (mukey), where the attributes are weighted averages
#of the components; so can't have multiple attributes at different depths per cokey
#especially since the depths sampled in different cokeys aren't uniform 

#pick all hzdepthb_r < 31 (ie, in plowable layer) 
#corresponding to <12 inches or < 30.48 cm
#assumption here is that gSSURGO attributes for P samples that are collected below the surface layer
#will closely mirror those at the top layer
#this is borne out when you examine the soil texture for multiple depths within a component (a cokey);
#texture attributes are typically similar across multiple depths

Chorizon[1:5, c(1,7, 10, 170, 171)]

Chorizon.surface<-Chorizon %>% 
  filter(hzdepb_r<31)#restrict to plowable layer

#does this still leave you with duplicates per site?
#(ie, multiple depths above 30cm)
length(Chorizon.surface$cokey)
length(unique(Chorizon.surface$cokey))

#are still multiple measures per site; multiple depths

#eliminate samples from the very shallow surface that are likely to be plant material
nrow(Chorizon.surface)
Chorizon.soil<-Chorizon.surface %>% 
  filter(hzdepb_r>3)

#choose lowermost surface layer, to avoid component attributes that include plant material at the surface rather than soil
# convert to data.table with key=cokey
library(data.table)
Chorizon.table <- data.table(Chorizon.soil, key="cokey")

#get the subset of data that matches this criterion
Chorizon.bottom<-unique(Chorizon.table[, .SD[hzdepb_r %in% max(hzdepb_r)], by=cokey])
head(Chorizon.bottom)

#check out depths at which samples are collected
summary(Chorizon.bottom$hzdepb_r)
names(Chorizon.bottom)
#View(Chorizon.bottom[, c(1,8, 11, 171)])


#check for duplicates
length(Chorizon.bottom$cokey)
length(unique(Chorizon.bottom$cokey))
###
#OK! Now only one record per cokey (the lowermost surface measurement in the top 30cm)
###


#'Chtexture' Table (grain size)
#Can be merged to Chorizon, with Chtexturegrp
AR5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_AR.gdb", layer = "chtexture")
IA5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IA.gdb", layer = "chtexture")
IL5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IL.gdb", layer = "chtexture")
IN5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IN.gdb", layer = "chtexture")
KS5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_KS.gdb", layer = "chtexture")
MN5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MN.gdb", layer = "chtexture")
MO5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MO.gdb", layer = "chtexture")
ND5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_ND.gdb", layer = "chtexture")
NE5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_NE.gdb", layer = "chtexture")
OH5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_OH.gdb", layer = "chtexture")
SD5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_SD.gdb", layer = "chtexture")
WI5 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_WI.gdb", layer = "chtexture")

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
AR6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_AR.gdb", layer = "chtexturegrp")
IA6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IA.gdb", layer = "chtexturegrp")
IL6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IL.gdb", layer = "chtexturegrp")
IN6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IN.gdb", layer = "chtexturegrp")
KS6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_KS.gdb", layer = "chtexturegrp")
MN6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MN.gdb", layer = "chtexturegrp")
MO6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MO.gdb", layer = "chtexturegrp")
ND6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_ND.gdb", layer = "chtexturegrp")
NE6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_NE.gdb", layer = "chtexturegrp")
OH6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_OH.gdb", layer = "chtexturegrp")
SD6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_SD.gdb", layer = "chtexturegrp")
WI6 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_WI.gdb", layer = "chtexturegrp")

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

#View(Horizon[duplicated(Horizon$chkey)|duplicated(Horizon$chkey, fromLast=TRUE),c(1,2,3,9,12,172,173,175,176,179)])

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
AR3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_AR.gdb", layer = "copm")
IA3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IA.gdb", layer = "copm")
IL3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IL.gdb", layer = "copm")
IN3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IN.gdb", layer = "copm")
KS3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_KS.gdb", layer = "copm")
MN3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MN.gdb", layer = "copm")
MO3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MO.gdb", layer = "copm")
ND3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_ND.gdb", layer = "copm")
NE3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_NE.gdb", layer = "copm")
OH3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_OH.gdb", layer = "copm")
SD3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_SD.gdb", layer = "copm")
WI3 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_WI.gdb", layer = "copm")

#Append all tables using rbind
Copm<-rbind(AR3, IA3, IL3, IN3, KS3, MN3, MO3, ND3, NE3, OH3, SD3, WI3)
nrow(Copm)
head(Copm)
names(Copm)

#'Copmgrp Table' (Needed to link Copm - parent material - table to component)
AR4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_AR.gdb", layer = "copmgrp")
IA4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IA.gdb", layer = "copmgrp")
IL4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IL.gdb", layer = "copmgrp")
IN4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_IN.gdb", layer = "copmgrp")
KS4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_KS.gdb", layer = "copmgrp")
MN4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MN.gdb", layer = "copmgrp")
MO4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_MO.gdb", layer = "copmgrp")
ND4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_ND.gdb", layer = "copmgrp")
NE4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_NE.gdb", layer = "copmgrp")
OH4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_OH.gdb", layer = "copmgrp")
SD4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_SD.gdb", layer = "copmgrp")
WI4 <- sf::st_read(dsn = "./gSSURGO/gSSURGO_WI.gdb", layer = "copmgrp")

#turn warnings back on, now that all attribute tables are loaded
options(warn=0)

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
#View(duplicates.Parent3[1:20,])
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

setwd(input_dir)
#'Valu1' Table:
#(already merged in ArcGIS)
valu1<-read.delim('./gSSURGO/Ssurgo_merged_Valu1_table.txt', sep=",", header=TRUE)
head(valu1)
valu1$MUTKEY<-valu1$mukey
head(valu1)

setwd(output_dir)

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
#View(Horizon.sub[,c(1,2,3,26,27,28)])
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

#library(dplyr)
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

setwd(output_dir)
write.table(SSURGO.unique, './SSURGO_attributes_unique_for_mapunit_CEAP_regionalextent.csv', sep=",", row.names=FALSE)
