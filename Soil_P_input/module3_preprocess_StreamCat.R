#Module for loading StreamCat attributes

#watershed and catchment scale variables from U.S. EPA StreamCat
#https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset

# Get a List of all files in StreamCat folder:
filenames <- list.files("./StreamCat_Text_Files", pattern=glob2rx("*.csv"), full.names=TRUE)

# Load and merge all data sets by COMID
df_list <- lapply(filenames,fread)
df_list[1:5]

StreamCat.ALL<-df_list %>% reduce(full_join, by='COMID')
names(StreamCat.ALL)
nrow(StreamCat.ALL)
#Write to table; 
setwd(output_dir)
fwrite(StreamCat.ALL, './StreamCat_variables_for_SOIL_P.csv')

