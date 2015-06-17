##################################################################################
##                                                                              ##
##                        Transportation and Environment Project                ##
##                                                                              ##
##################################################################################

#####################################
#          Calling Packages         #
#####################################
library(ggplot2)
library(gdata)
library(plyr)
library(gtools)
library(reshape2)
library(lubridate)
library(modeest)
library(ggmap)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
######################################
#     Putting all file together     ##
######################################

# Put your own directory 'path' where you've kept the files
# Remember to put backslashes, and one at the end of folder name.(E.g, Field_data/)

path <- "C:/Course readings etc/Transportation & Environment/Field_data/"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
  {
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep=""), skip=2))
}

#######################################
##    Selecting only the M903 value  ##
#######################################

# I have used the substr("", 1,5), both in deciding the value put into two
# variables, 'env' and later in the 'assign' command, because,
# our filenames have the patternt of 'tne_1.csv',..'tne_6.csv'. So,
# The first five palces of the filename effectively describe the filename.

# For the immedtate, I've put the value "start+2300" because for my files, 
# it's within which range the program will select the row number that has
#"Timestamp" in one of the columns. Please check your files and derive
# a number of rows, in which only the row with "Timestamp",
# just after the lightscatter value ends could be taken.

for(file in files)
{
  env <- substr(file, 1,5)
  start<- as.numeric(which(get(env)[1]=="M903")+1)
  time<- which(get(env)[1]=="Timestamp")
  immediate <- which(time>start & time<= (start+2300))
  ending <- as.numeric(time[immediate])
  assign((substr(file, 1,5)),get(env)[(start+1):(ending-3),]) 
}

##################################################################
# Changing data in executable format, and combining all of them ##
##################################################################


# It includes, 

# 1. Putting an indicator to understand 
# which row which trip, before combining them,
# hence the column "Collection_phase".

# 2. Getting the 'Date' and time (Hour_min) from the 'Timestamp'.

# 3. Correcting the latitude and longitude data because that's a bug
# in the 'Aircasting' app, no latitude of '-122.6000' is plotable on earth.

# 4. Converting the value in numeric, as it may come as of class 'factor'.

tne_1$Collection_phase <- "First"
tne_2$Collection_phase <- "Second"
tne_3$Collection_phase <- "Third"
tne_4$Collection_phase <- "Fourth"
tne_5$Collection_phase <- "Fifth"
tne_6$Collection_phase <- "Sixth"

All<- rbind(tne_1, tne_2, tne_3, tne_4, tne_5, tne_6)

All$Value<- as.numeric(as.character(All$Value))
All$Longitude <- as.numeric(as.character(All$geo.lat ))
All$Latitude<- as.numeric(as.character(All$geo.long))
All$Date <- substr(All$Timestamp, 1,10)
All$Hour_Min<- substr(All$Timestamp, 12,16)

# Taking only reqired columns

All<- All[, c(4:9)]

#####################################################################
# Extracting Maximum, Minimum, Avg, Median and Mode in each minute  #
# (Conversion to Minute level)                                      #
# The latitude and longitude are taken as the median value,         #
# so that it will be located at almost mid-location within          #
# the path traversed during that particluar minute.(60 datapoints)  #
#####################################################################

#############################################
# Creating function to extract the values   #
#############################################

Minute_func = function(All) {
  Center_lat = median(All$Latitude)
  Center_long= median(All$Longitude)
  Maximum = max(All$Value)
  Minimum = min(All$Value)
  Mean = mean(All$Value)
  Median= median(All$Value)
  Mostfreq=mfv(All$Value)[1]
  data.frame(Center_lat, Center_long, Maximum, Minimum, Mean, Median, Mostfreq)
}

######################################################################
# Using ddply fnction to create a Dataframe with minute level values.#
######################################################################

All_Minutes<- ddply(All, .(Collection_phase,Date,Hour_Min), Minute_func)

######################################################
#     Converting lightscatter "Value" to PM2.5      ##
#    Equation provided by Mr. Matt Harper, PSCAA    ##
######################################################

All_Minutes$PM2.5 <- ((All_Minutes$Mostfreq)*.276 +2.6)

######################################################
#    Extracting CSV, if someone want to use excel   ##
######################################################

write.csv(All_Minutes, "C:/Course readings etc/Transportation & Environment/Minute_level.csv")




################################################################################################
#                                      For Our Team                                            #
################################################################################################

######################################
#    Subsetting each trip            #
######################################
First<- subset(All_Minutes, All_Minutes$Collection_phase == "First")
Second <- subset(All_Minutes, All_Minutes$Collection_phase == "Second")
Third <- subset(All_Minutes, All_Minutes$Collection_phase == "Third")
Fourth <- subset(All_Minutes, All_Minutes$Collection_phase == "Fourth")
Fifth <- subset(All_Minutes, All_Minutes$Collection_phase == "Fifth")
Sixth <- subset(All_Minutes, All_Minutes$Collection_phase == "Sixth")

##################################################
#    Map for individual collection trip/phase    #
#    According to EPA Guidelines and margins     #
##################################################

Area_map <- get_map(location= c(lon = -122.3154, lat = 47.66044), 
                    zoom=15, maptype='toner')
t1 <-ggmap(Area_map)

# get the points
First_Minutes_map<- 
  ggmap(Area_map) +
  geom_point(data = First,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(First$PM2.5 >= 65.5, "red", 
                          ifelse(First$PM2.5 <= 15.4, "forestgreen",
                                 ifelse(First$PM2.5 <= 35.4,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 1- \nPM2.5 Level")

First_Minutes_map

# 2nd Trip
Second_Minutes_map<- 
  ggmap(Area_map) +
  geom_point(data = Second,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Second$PM2.5 >= 65.5, "red", 
                          ifelse(Second$PM2.5 <= 15.4, "forestgreen",
                                 ifelse(Second$PM2.5 <= 35.4,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 2- \n PM2.5 Level")

Second_Minutes_map

# 3rd Trip
Third_Minutes_map<- 
  ggmap(Area_map) +
  geom_point(data = Third,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Third$PM2.5 >= 65.5, "red", 
                          ifelse(Third$PM2.5 <= 15.4, "forestgreen",
                                 ifelse(Third$PM2.5 <= 35.4,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 3- \n PM2.5 Level")

Third_Minutes_map

# 4th Trip
Fourth_Minutes_map<- 
  ggmap(Area_map) +
  geom_point(data = Fourth,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Fourth$PM2.5 >= 65.5, "red", 
                          ifelse(Fourth$PM2.5 <= 15.4, "forestgreen",
                                 ifelse(Fourth$PM2.5 <= 35.4,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 4- \n PM2.5 Level")

Fourth_Minutes_map

# 5th Trip
Fifth_Minutes_map<- 
  ggmap(Area_map) +
  geom_point(data = Fifth,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Fifth$PM2.5 >= 65.5, "red", 
                          ifelse(Fifth$PM2.5 <= 15.4, "forestgreen",
                                 ifelse(Fifth$PM2.5 <= 35.4,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 5- \n PM2.5 Level")

Fifth_Minutes_map

# 6th Trip
Sixth_Minutes_map<- 
  ggmap(Area_map) +
  geom_point(data = Sixth,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Sixth$PM2.5 >= 65.5, "red", 
                          ifelse(Sixth$PM2.5 <= 15.4, "forestgreen",
                                 ifelse(Sixth$PM2.5 <= 35.4,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 6- \n PM2.5 Level")

Sixth_Minutes_map

grid.arrange(First_Minutes_map, Second_Minutes_map, Third_Minutes_map, 
             Fourth_Minutes_map, Fifth_Minutes_map, Sixth_Minutes_map, ncol=3)


##################################################
#    Map for individual collection trip/phase    #
#    According to increment at 5ug/m^3 Level     #
##################################################
# get the points
First_variation_map<- 
  ggmap(Area_map) +
  geom_point(data = First,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(First$PM2.5 >= 15, "red", 
                          ifelse(First$PM2.5 <= 5, "forestgreen",
                                 ifelse(First$PM2.5 <= 10,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 1- \nPM2.5 Level")

First_variation_map

# 2nd Trip
Second_variation_map<- 
  ggmap(Area_map) +
  geom_point(data = Second,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Second$PM2.5 >= 15, "red", 
                          ifelse(Second$PM2.5 <= 5, "forestgreen",
                                 ifelse(Second$PM2.5 <= 10,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 2- \n PM2.5 Level")

Second_variation_map

# 3rd Trip
Third_variation_map<- 
  ggmap(Area_map) +
  geom_point(data = Third,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Third$PM2.5 >= 15, "red", 
                          ifelse(Third$PM2.5 <= 5, "forestgreen",
                                 ifelse(Third$PM2.5 <= 10,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 3- \n PM2.5 Level")

Third_variation_map

# 4th Trip
Fourth_variation_map<- 
  ggmap(Area_map) +
  geom_point(data = Fourth,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Fourth$PM2.5 >= 15, "red", 
                          ifelse(Fourth$PM2.5 <= 5, "forestgreen",
                                 ifelse(Fourth$PM2.5 <= 10,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 4- \n PM2.5 Level")

Fourth_variation_map

# 5th Trip
Fifth_variation_map<- 
  ggmap(Area_map) +
  geom_point(data = Fifth,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Fifth$PM2.5 >= 15, "red", 
                          ifelse(Fifth$PM2.5 <= 5, "forestgreen",
                                 ifelse(Fifth$PM2.5 <= 10,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 5- \n PM2.5 Level")

Fifth_variation_map

# 6th Trip
Sixth_variation_map<- 
  ggmap(Area_map) +
  geom_point(data = Sixth,  # Got the points
             mapping= aes(x = Center_long, y = Center_lat), 
             fill= ifelse(Sixth$PM2.5 >= 15, "red", 
                          ifelse(Sixth$PM2.5 <= 5, "forestgreen",
                                 ifelse(Sixth$PM2.5 <= 10,"yellow", "orange"))),
             color= "forestgreen",pch=21, size=6, alpha=.9) +
  ggtitle("Trip 6- \n PM2.5 Level")

Sixth_variation_map

grid.arrange(First_variation_map, Second_variation_map, Third_variation_map, 
             Fourth_variation_map, Fifth_variation_map, Sixth_variation_map, ncol=3)


#######################################
#   Comparing Morning Vs. Afternoon   #
#######################################

Morning<- rbind(First, Third, Fifth)
Morning$Time <- "Morning"

# Putting indexes-
Morning$Index <- 1
i=1
for (i in 1:nrow(Morning)){
  Morning[i,13] <- i
  i=i+1
}
Afternoon <- rbind(Second, Fourth, Sixth)
Afternoon$Time <- "Afternoon"

# Putting Indexes-
Afternoon$Index <- 1
i=1
for (i in 1:nrow(Afternoon)){
  Afternoon[i,13] <- i
  i=i+1
}

# Combining them
Morning_Afternoon <- rbind(Morning, Afternoon)

# Plotting them
Morning_afternoon_Compare <-
  ggplot(Morning_Afternoon,aes(x=Index,y=PM2.5))+
  geom_line(aes(color=Time))+
  geom_point(aes(color=Collection_phase))+
  theme_hc(bgcolor = "darkunica") +
  scale_color_hc("darkunica")+
  guides(size=F, colour = guide_legend(override.aes = list(size=3)))+
  ggtitle("Comparison- Morning Vs. Afternoon")+
  theme(plot.title = element_text(size=25, face="bold"))+
  theme(legend.title = element_text(size= 20))+
  theme(legend.text = element_text(size= 16))
  
Morning_afternoon_Compare
