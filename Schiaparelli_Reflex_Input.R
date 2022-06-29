# Schiaparelli Konversion für Reflex
# Author Moritz Koch 
# V.1 28/6/2022


library("stringr")
library("dplyr")
library("chron")
library("kimisc")
library("anytime")
library("tidyverse")
library("sf")

wd <- setwd("C:/Users/AG-Braun/Desktop/Schiaparelli/ASCII/latlon")    # link to folder where GNSS Data is stored 
#C:\Users\AG-Braun\Desktop\Schiaparelli\ASCII\latlon

##### Section 1 - preparing the Leika input ####################################

Schiaparelli_Profile <- list.files(wd, pattern = ".csv", )       # reading in files 

for (i in 1:length(Schiaparelli_Profile)){ 

koords <-  as.data.frame(read.csv2(file = Schiaparelli_Profile[i], header = FALSE, sep = ","))
superfinale <- as.data.frame(read.csv2(file = Schiaparelli_Profile[i], header = FALSE, sep = ",")) 

# in V1 and V2 Long/Lat is stored

superfinale[,1] <- as.numeric(superfinale[,1])
superfinale[,2] <- as.numeric(superfinale[,2])

###

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


plot(superfinale$V1)
plot(superfinale$V2)

plot(koords$V1)
plot(koords$V2)



#################### saving file ###############################################

savename <- Schiaparelli_Profile[i]

savename <- str_sub(savename,1,nchar(savename)-4)

#paste("clipped", savename, "timecorrected" , timecorrection,"csv", sep=".")

write.csv(superfinale, file = paste("converted", savename, "csv", sep="."), row.names = FALSE)

}

