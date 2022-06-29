# this script prepares measured data from the radar antenna system components 
# as well as the Leika GNSS measurements for the processing in Reflex
################################################################################

# Version 2
# this version is adapted to the pre processing done by David 
# the procedure in its core remains the same however the format of the 
# input looks a bit different which this script is optimized for 


#install.packages("stringr")
#install.packages("dplyr")
#install.packages("chron")
#install.packages("kimisc")
#install.packages("anytime")
#install.packages("tidyverse")
#install.packages("sf")

library("stringr")
library("dplyr")
library("chron")
library("kimisc")
library("anytime")
library("tidyverse")
library("sf")

gnss_wd <- setwd("D:/Daten/GNSS")    # link to folder where GNSS Data is stored 



# Before you start the script:
# You have to delete all lines including the header in the GNSS (from Leika) file 
# after deleting, save the new file with the prefix "readable_" in front of the filename 


##### Section 1 - preparing the Leika input ####################################

Leika_radar_files <- list.files(gnss_wd, pattern = "Viedma", )       # reading in files 

#____________________________________________
# Moreno flight 1:
#     Traces: 43228
#     Start record: 12:19:28
#     End record: 13:32:00 
#____________________________________________
# Moreno flight 2:
#    Traces: 38451
#    Start record: 14:22:20
#    End record:   15:28:00  
#____________________________________________
# Moreno flight 3:
#    Traces: 8241
#    Start record: 16:02:38
#    End record:   16:16:44
#____________________________________________  



# Step 1 - adjust input 
Leika_radar_files
input <- 1

# step 2 - set traces 
traces <- 8241              

# step 3 - set start time 
Start <- "16:02:38"

# step 4 - set end time 
End <- "16:16:44"

# step 5 - Apply UTC time correction? YES/NO

timecorrection <- "NO"


#_______________________________________________________________________________
############### START SCRIPT ###################################################
############### DONT CHANGE ANYTHING BELOW HERE (unless it does not work :_)  ##
#_______________________________________________________________________________


LeikaGPS_Flight1.df <- as.data.frame(read.csv2(file = Leika_radar_files[input], header = TRUE, sep = ",")) 



utm <- LeikaGPS_Flight1.df[3:4]

install.packages("proj4")
library(proj4)
proj4string <- "+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

# Source data
xy <- data.frame(x=354521, y=7997417.8)

# Transformed data
pj <- project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)




headerGPS <- c("Stati", "GPSTime(HMS)", "DATE/MYD", "Easting(m)", "Northing(m)", "H-Ell(m)", "SDHoriz(m)", "SDHeigh (m)", "Q", "Latitude", "Longitude")


utm2 <- spTransform(utm1,CRS("+proj=longlat +datum=WGS84"))colnames(LeikaGPS_Flight1.df) <- headerGPS  # assign header 

timecol <- as.data.frame(LeikaGPS_Flight1.df[,2]) 
colnames(timecol) <- "time"

timecol <- str_sub(timecol[,1],1,nchar(timecol[,1])-3)

time <- timecol
time <- as.data.frame(chron(times=time))

#################################################

# corrects utc time or not 
if (timecorrection == "YES"){
  utc <- "00:00:18"                        
  utc <- chron(times=utc)                  
}else{
  utc <- "00:00:00"                        
  utc <- chron(times=utc) 
}    

# loops over time df 
i <- 1                                   
for (i in 1:length(nrow(time))){         
  time[1,i] <- time[1,i] - utc             
  i <- i+1                               
}  

#converts back as character for later 
j <- 1                                   
for (j in 1:length(nrow(time))){         
  time[1,j] <- as.character(time[1,j])   
  j <- j+1                               
}                                        

# cbinding to df 
LeikaGPS_Flight1.df <- cbind(LeikaGPS_Flight1.df, time)

################################# SET TIME HERE ################################
# begin and end time                                                            
StartTime <- Start    # this input needs to be adjusted to the start            

EndTime <- End      # and end of the radar measurement                          
################################################################################

indexStartTime <- as.data.frame(LeikaGPS_Flight1.df[LeikaGPS_Flight1.df$`chron(times = time)`== StartTime,]) # finding index
startSlice <- row.names(indexStartTime) # write out row that will be the start

indexEndTime <- as.data.frame(LeikaGPS_Flight1.df[LeikaGPS_Flight1.df$`chron(times = time)`== EndTime,])
endSlice <- row.names(indexEndTime) # write out row that will mark the end 

# subset based on beginning and end 
LeikaSubset <- LeikaGPS_Flight1.df[startSlice:endSlice, ] # slices the df based on the indices 

# new dataframe with only necessary entries
select <- c("Latitude", "Longitude","H-Ell(m)" , "GPSTime(HMS)")  # write rows for a subset
marriageSubset <- LeikaSubset[select]   # select rows and write them in a new df

# at this stage the leika input is prepared 

################################################################################
# claculating traces per second 
# the input to this should be set on top 
# 

time <- c(Start, End)  
x <- chron(times=time)
time_diff <- diff(x)

flight_time_sec <- hms.to.seconds(time_diff)

tracePerSecond <- traces / flight_time_sec

L <- nrow(marriageSubset)-1

trace <- as.integer(round(seq(0, L*tracePerSecond, by=tracePerSecond)))

Finale <- cbind(marriageSubset, trace)

finalesselect <- c("Latitude", "Longitude", "H-Ell(m)", "trace")

superfinale <- Finale[finalesselect]

reflexHeader <- c("latitude", "longitude", "altitude", "trace")

colnames(superfinale) <- reflexHeader


superfinale[,1] <- as.numeric(superfinale[,1])
superfinale[,2] <- as.numeric(superfinale[,2])

k<-1
for(k in 1:nrow(superfinale)){
  
  dd <- as.integer(superfinale[k,1])
  m1 <- superfinale[k,1]
  m2 <- as.numeric(as.integer(superfinale[k,1]))
  m3 <- as.numeric(m1)-as.numeric(m2)
  mm <- abs(m3)*60
  
  mmm <- as.integer(mm)
  nnn<-as.integer(10000000*(mm-mmm))
  nnn<-as.character(nnn)
  
  if (as.numeric(nnn) < 10) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 100) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 1000) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 10000) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 100000) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 1000000) {
    nnn<-as.character(paste0(0,nnn))
  }
  
  x <- paste0(dd,mmm, ".", nnn)
  
  #y <- str_sub(x,1,nchar(x)-6)
  
  #if(nchar(x)<13){
  #  paste0(x,0)
  #}
  
  
  #while (nchar(x)<13) {
  #  x<-paste0(x,0)
  #  }
  superfinale[k,1]<-x 
  
}

##### lat

k<-1

for(k in 1:nrow(superfinale)){
  
  # print(k)
  #  print(superfinale[k,1])
  # print(as.integer(superfinale[k,1]))
  
  
  dd <- as.integer(superfinale[k,2])
  m1 <- superfinale[k,2]
  m2 <- as.numeric(as.integer(superfinale[k,2]))
  m3 <- as.numeric(m1)-as.numeric(m2)
  mm <- abs(m3)*60
  mmm <- as.integer(mm)
  mmm <-as.numeric(mmm)
  nnn<-as.integer(1000000*(mm-mmm))
  nnn<-as.character(nnn)
  
  if (nchar(mmm)==1){
    mmm <- paste0(0,mmm)
  }
  
  
  
  if (as.numeric(nnn) < 10) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 100) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 1000) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 10000) {
    nnn<-as.character(paste0(0,nnn))
  }
  if (as.numeric(nnn) < 100000) {
    nnn<-as.character(paste0(0,nnn))
  }
  
  x <- paste0(dd,mmm, ".", nnn)
  x <-  str_replace(x, "-", "")
  x <- paste0("-", 0, x)
  nchar(x)
  #y <- str_sub(x,1,nchar(x)-6)
  #while (nchar(x)<13) {
  #  x<-paste0(x,0)
  
  #}
  
  x
  superfinale[k,2]<-x 
  
}

plot(superfinale$latitude)

plot(Finale$Latitude)

plot(superfinale$longitude)

plot(Finale$Longitude)


#################### saving file ###############################################

savename <- Leika_radar_files[input]

savename <- str_sub(savename,1,nchar(savename)-4)

#paste("clipped", savename, "timecorrected" , timecorrection,"csv", sep=".")

write.csv(superfinale, file = paste("clipped", savename, "timecorrected" , timecorrection,"csv", sep="."), row.names = FALSE)

