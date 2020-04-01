

rm(list=ls())

library(stringr)

### convert climate data into MONICA format

###load example climate file

setwd("C:/Users/hampf/Documents/MONICA_simulationen/Europ_Crop_Rot/macsur-croprotations-cz_rerunStepB/step_B/local_run/test_hadgem")

clim<-read.csv("LED-HadGEM2-ES-RCP85_01.csv")


# Function for filling NA´s in CO2 column
f4 <- function(x, blank = is.na) {
  # Find the values
  if (is.function(blank)) {
    isnotblank <- !blank(x)
  } else {
    isnotblank <- x != blank
  }
  # Fill down
  x[which(isnotblank)][cumsum(isnotblank)]
}


### now read original climate data (w6d) and convert them to csv-files! 

dir<-"C:/Users/hampf/Documents/MONICA_simulationen/Europ_Crop_Rot/macsur-croprotations-cz_rerunStepB/step_B/climate_original_data_no_snow"
setwd(dir)

output_folder<-"C:/Users/hampf/Documents/MONICA_simulationen/Europ_Crop_Rot/macsur-croprotations-cz_rerunStepB/step_B/climate_converted_no_snow"

files<-list.files(dir, recursive=T, pattern=".w6d")
files<-files[1:20] # subset for testing
print(files)

for (file in files){
  df1 <- read.delim(file, sep=" ", header=T)
  name<-str_split(file, pattern="/")[[1]][2]
  name<-str_sub(name,-nchar(file),-4)
  name<-paste(name, "csv", sep="")
  print(name)
  folder_name<-str_split(file, pattern="/")[[1]][1]
  print(folder_name)
  ##read file
  df<-df1[, colSums(is.na(df1)) != nrow(df1)] ##delete columns that have only NA´s 
  df$iso.date<-clim$iso.date
  ##globrad
  df$globrad<-with(df,ifelse(is.na(RAD),X.3,RAD))  
  
  ##tmax
  df$tmax<-with(df,ifelse(is.na(TMAX),X.5,TMAX))
  df$tmax<-with(df,ifelse(is.na(tmax),X.6,tmax))
  
  ##tmin
  df$tmin<-with(df,ifelse(is.na(TMIN),X.9,TMIN))
  df$tmin<-with(df,ifelse(is.na(tmin),X.8,tmin))
  df$tmin<-with(df,ifelse(is.na(tmin),X.7,tmin))
  
  ##relative humidity
  df$relhumid<-with(df,ifelse(is.na(X.12),X.11,X.12))
  df$relhumid<-with(df,ifelse(is.na(relhumid),X.10,relhumid))
  df$relhumid<-with(df,ifelse(is.na(relhumid),X.9,relhumid))
  
  ##wind
  df$wind<-with(df,ifelse(is.na(X.15),RH,X.15))
  df$wind<-with(df,ifelse(is.na(wind),X.14,wind))
  df$wind<-with(df,ifelse(is.na(wind),X.13,wind))
  
  ##precip
  df$precip<-with(df,ifelse(is.na(X.18),WIND,X.18))
  df$precip<-with(df,ifelse(is.na(precip),X.17,precip))
  df$precip<-with(df,ifelse(is.na(precip),X.16,precip))
  
  ##CO2
  df$co2<-with(df,ifelse(is.na(X.20),X.19,X.20))
  df$co2<-f4(df$co2)
  
  ##keep only those columns that are not needed
  df<-df[, (colnames(df) %in% c("iso.date", "precip", "tmin", "tmax", "globrad", "wind", "relhumid", "co2"))] 
  
  df$tavg<-(df$tmin + df$tmax)/2
  
  ##change order of columns
  df <- df[c("iso.date", "precip", "tmin", "tavg", "tmax", "globrad", "wind", "relhumid", "co2")]
  
  #write.table(df, file=paste(dir, folder_name, name, sep="/"), row.names = FALSE, col.names = T, sep =",", quote=F)
  #write.table(df, file=paste(output_folder, name, sep="/"), row.names = FALSE, col.names = T, sep =",", quote=F)

}


