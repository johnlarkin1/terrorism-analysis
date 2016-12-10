# Author: John Larkin
# Date: 11/30/2016
# Institution: Swarthmore College
# Class: International Politics
# 
# **************************************
#   Program:
#       terrorism_analysis.R
#
#   Purpose: 
#       The purpose of this project is multidisciplinary Primarily, this script and analysis
#       will be done to support my final research paper on self-selecting and lone 
#       terrorists in the United States. The intent was to create visualizations and analytics
#       that offer another perspective outside of the theoretical. This project will also serve
#       to rehash and refresh R skills previously developed. 
#   http://127.0.0.1:26372/graphics/plot_zoom_png?width=748&height=823
#   Helpful Links:
#       This blog post from a Stanford phd student helped to identify useful columns:
#       http://www.shorttails.io/a-timeline-of-terrorism/#fn:1
#
# **************************************

# Wipe everything
rm(list = ls())

library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)

# ***************************************** #
# **********  Getting the Data!  ********** #
# ***************************************** #

# Do you want to use the CSV file or the XLSX file? Not sure of optimizations yet. 
use_csv <- TRUE 
# NOTE: the csv version seems to load without any warnings which is a def + :) 

# Let's load in the data
directory = "~/Desktop/College/Academics/Senior/Fall/POLS4_IntPolitics/Final_Essay/Analysis/"
if(use_csv) {
    filename = "globalterrorismdb_0616dist.csv"
    file_location = paste(directory,filename, sep = "")
    dat <- read.csv(file_location)
} else {
    filename = "globalterrorismdb_0616dist.xlsx"
    file_location = paste(directory,filename, sep = "")
    dat <- read_excel(file_location)
}

# Let's pick off the data that we think is relevant
# This data is really diverse. Let's interpret what it all means.
#
# GTD ID AND DATE
# ---------------
#   GTD ID - this is the unique ID (string)
#   iyear - year of incident (int)
#   imonth - month of incident (int)
#   iday - day of incident (int)
#   approxdate - this is when the exact date isn't determined 
#       if an element is not determined then a 0 is placed in the date, year or month
#   extendedevent - 1 if lasted longer than 24 hrs; 0 otherwise (boolean)
#   resolution - only exists if lasted longer than 24 hours. this tells when the event actually ended (R date variable)
#
# INCIDENT INFORMATION
# --------------------
#   summary - explanation of what's going on (text variable) 
#  
#   Note: the next few variables are 1 if the criterion is met. 0 if not.
#   crit1 - POLITICAL, ECONOMIC, RELIGIOUS, OR SOCIAL GOAL (CRIT1)
#   crit2 - INTENTION TO COERCE, INTIMIDATE OR PUBLICIZE TO LARGER AUDIENCE(S) (CRIT2)
#   crit3 - OUTSIDE INTERNATIONAL HUMANITARIAN LAW (CRIT3)
#   
#   doubtterr - 1 if there is a doubt; -9 if before this category was invented; zero if not (bool)
#               only applies if there was a doubt
#   alternative - only there if doubtterr was 1. (can drop col)
#   alternativetxt - can drop col
#   multiple - can drop col
#   related incidents   
# .... this is going to be easier if we keep only the columns that we like
#
#
#   want to keep eventid, iyear, imonth, iday, country, country_txt, region, region_txt, provstate, city, summary, crit1,crit2,crit3
#   want to keep success, suicide, attack type, attacktype1_txt, weaptype1, targtype1, targtype1_txt,targsubtype1, targsubtype1_txt,
#   want to keep gname, gsubname, CLAIMED - this is if a group or person claimed responsibility
#
#   see corresponding appendix

# NOTE: Environment view does not show the entire dataframe

# let's get rid of bad rows

terrordata_polish1 <- dat[dat$doubtterr == 0,] # let's just make sure we have terrorist events (i.e. removing 1's and 9's)
terrordata_polish2 <- terrordata_polish1[!is.na(terrordata_polish1$iyear),]

# let's just keep the best columns
valid_col1 <- c("eventid", "iyear", "imonth", "iday", "country", "country_txt", "region", "region_txt", "provstate", "city")
valid_col2 <- c("summary", "crit1", "crit2", "crit3", "success", "suicide", "attacktype1", "attacktype1_txt", "weaptype1", "targtype1")
valid_col3 <- c("targtype1_txt", "targsubtype1", "targsubtype1_txt", "gname", "gsubname", "claimed", "nkillus", "nkillter")
valid_col4 <- c("nkill", "nwound", "weaptype1_txt")
total_valid_col <- c(valid_col1, valid_col2, valid_col3, valid_col4)
terrordata_clean <- terrordata_polish2[,total_valid_col]

# Yay dimensionality reduction!
dimension <- dim(terrordata_clean)
print(paste('The dimension of our new simplified data set is:', dimension[1], dimension[2]))

total_deaths_and_inj <- terrordata_clean$nkill + terrordata_clean$nwound
exact_date_if_av <- ISOdate(terrordata_clean$iyear, terrordata_clean$imonth, terrordata_clean$iday)
ter_clean_adv <- data.frame(terrordata_clean, total_dth_and_inj = total_deaths_and_inj, exact_date = exact_date_if_av)

# ****************************************** #
# **********  Plotting the Data!  ********** #
# ****************************************** #


######## Graph 1 - Barplot of Attacks

uniqueyears <- unique(ter_clean_adv$iyear)
uniqueyears <- uniqueyears[(uniqueyears %% 10 == 0)]
uniqueyears <- as.character(uniqueyears)

attacksbarplot <- ggplot(data=ter_clean_adv,aes(x=as.factor(iyear)),fill=variable) + geom_bar(aes(fill = ter_clean_adv$iyear)) + theme_bw(base_size=35) + xlab("Year") + ylab("Number of Attacks") 
attacksbarplot <- attacksbarplot + ggtitle("Barplot of Number of Attacks vs Year")
attacksbarplot <- attacksbarplot + theme(
                        axis.title = element_text(size = 15),
                        legend.position="none", 
                        axis.text.y = element_text(size=14),
                        axis.text.x = element_text(size=14), 
                        #axis.ticks=element_blank(),
                        #panel.grid.major = element_blank(),
                        panel.grid=element_line(colour="white", size=0.5),
                        panel.border=element_blank(),
                        panel.background = element_rect(fill = "grey")
                        )
attacksbarplot <- attacksbarplot + scale_x_discrete(breaks = uniqueyears)
attacksbarplot
extension <- "plots/"
location <- paste(directory,extension, "attacksbarplot", sep="")
png(location,width=1000,height=700)
attacksbarplot
dev.off()


######## Graph 2 - Locations
outlier_removed1 <- ter_clean_adv[ter_clean_adv$total_dth_and_inj < 2000,]
outlier_removed <- outlier_removed1[!is.na(outlier_removed1$total_dth_and_inj),]
location_attack <- ggplot(data = outlier_removed, aes(x = iyear, y = total_dth_and_inj, color = as.factor(region_txt))) + geom_point() + theme_grey()
extension <- "plots/"
location <- paste(directory,extension, "location_attack", sep="")
png(location,width=1000,height=700)
location_attack
dev.off()

######## Graph 3 - Guns in locations?
# let's get solely the acts with exact dates
exact_date_data <- outlier_removed[!is.na(outlier_removed$exact_date),]

names_of_y <- levels(exact_date_data$weaptype1_txt)
names_of_y[12] <- "Vehicle"

levels(exact_date_data$weaptype1_txt)[levels(exact_date_data$weaptype1_txt) == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)"] <- "Vehicle"

weapon_plot <- ggplot(data = exact_date_data, aes(x = exact_date, y = weaptype1_txt, color = region_txt)) + geom_point() + theme(
    axis.title = element_text(size=15))
location <- paste(directory,extension, "weapon_plot", sep="")
png(location,width=1000,height=700)
weapon_plot
dev.off()

######## Graph 3 - Heatmap of Location and Weapon type

new_data_for_hm <- exact_date_data
new_data_for_hm <- subset(new_data_for_hm, select =-c(targtype1, targtype1_txt, targsubtype1_txt, gname, gsubname))
new_data_for_hm <- subset(new_data_for_hm, select =-c(targsubtype1, crit1, crit2, crit3, nkillter, nkillus))
new_data_for_hm <- subset(new_data_for_hm, select =-c(summary, city, provstate, imonth, iday))
new_data_for_hm <- subset(new_data_for_hm, select =-c(iyear, exact_date, eventid, country_txt))
new_data_for_hm <- subset(new_data_for_hm, select =-c(region_txt, attacktype1_txt, weaptype1_txt))

collap_hm_data <- aggregate(new_data_for_hm, by=list(countryID = new_data_for_hm$country), FUN = sum, na.rm = TRUE)

# let's essentially build a dictionary that just has the country code and the country text
key_val_country <- data.frame(countryID =exact_date_data$country, country_txt = exact_date_data$country_txt) 
dups <- duplicated(key_val_country)
key_val_country <- key_val_country[!dups, ]

# let's see if we can combine back our data
merged_country_hm_data <- merge(collap_hm_data, key_val_country, by='countryID')

rownames(merged_country_hm_data) <- merged_country_hm_data$country_txt
dat_matrix <- data.matrix(merged_country_hm_data)

heatmp <- heatmap(dat_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
location <- paste(directory,extension, "heatmp", sep="")
png(location,width=1000,height=700)
heatmp
dev.off()

######## Graph 4 - Heatmap of Region and Weapon type
new_data_for_hm <- exact_date_data
new_data_for_hm <- subset(new_data_for_hm, select =-c(targtype1, targtype1_txt, targsubtype1_txt, gname, gsubname))
new_data_for_hm <- subset(new_data_for_hm, select =-c(targsubtype1, crit1, crit2, crit3, nkillter, nkillus))
new_data_for_hm <- subset(new_data_for_hm, select =-c(summary, city, provstate, imonth, iday))
new_data_for_hm <- subset(new_data_for_hm, select =-c(iyear, exact_date, eventid, country_txt))
new_data_for_hm <- subset(new_data_for_hm, select =-c(region_txt, attacktype1_txt, weaptype1_txt))
collap_hm_data <- aggregate(new_data_for_hm, by=list(regionID = new_data_for_hm$region), FUN = sum, na.rm = TRUE)

# let's essentially build a dictionary that just has the country code and the country text
key_val_region <- data.frame(regionID =exact_date_data$country, region_txt = exact_date_data$region_txt) 
dups <- duplicated(key_val_region)
key_val_region <- key_val_region[!dups, ]

# let's see if we can combine back our data
merged_region_hm_data <- merge(collap_hm_data, key_val_region, by='regionID')

rownames(merged_region_hm_data) <- merged_region_hm_data$region_txt
dat_matrix <- data.matrix(merged_region_hm_data)

heatmp <- heatmap(dat_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
location <- paste(directory,extension, "heatmp2", sep="")
png(location,width=1000,height=700)
heatmp
dev.off()


######## Graph 5 - Heatmap of Region and Weapon type
melt_merged_region <- subset(merged_region_hm_data, select =-c(country, region))
melt_merged_region <- melt(melt_merged_region, id.vars = "region_txt")
heat_matrix <- ggplot(melt_merged_region, aes(x=variable, y=region_txt)) + geom_tile(aes(fill=value)) +
    scale_fill_gradient(low = "white",high = "steelblue") +
    theme(axis.ticks = element_blank(),
         axis.text.x = element_text(angle = 330, hjust = 0))
location <- paste(directory,extension, "heatmatrix", sep="")
png(location,width=1000,height=700)
heat_matrix
dev.off()

######## Graph 6 - Histogram Based on Region and Count of Attacks
# let's break our data up 
regions <- as.data.frame(table(ter_clean_adv$region_txt))

locationbarplot <- ggplot(data=regions,aes(x=Var1,y=Freq,fill=variable)) + geom_bar(aes(fill = Freq), stat="identity") + theme_bw(base_size=35) + xlab("Location") + ylab("Number of Attacks") 
locationbarplot <- locationbarplot + ggtitle("Barplot of Number of Attacks vs Location")
locationbarplot <- locationbarplot + theme(
    axis.title = element_text(size = 15),
    legend.position="none", 
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14, angle = 45, hjust = 1), 
    #axis.ticks=element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid=element_line(colour="white", size=0.5),
    panel.border=element_blank(),
    panel.background = element_rect(fill = "grey")
)
locationbarplot

### Save everything

resolutionval <- 600
ggsave(
    filename = "attacksbarplot.png",
    plot = attacksbarplot,
    path = location <- paste(directory,extension, sep=""),
    width = 12,
    height = 8,
    dpi = resolutionval
)

ggsave(
    filename = "locationbarplot.png",
    plot = locationbarplot,
    path = location <- paste(directory,extension, sep=""),
    width = 12,
    height = 8,
    dpi = resolutionval
)

ggsave(
    filename = "location_attack.png",
    plot = location_attack,
    path = location <- paste(directory,extension, sep=""),
    width = 12,
    height = 8,
    dpi = resolutionval
)

ggsave(
    filename = "heat_matrix.png",
    plot = heat_matrix,
    path = location <- paste(directory,extension, sep=""),
    width = 12,
    height = 8,
    dpi = resolutionval
)

ggsave(
    filename = "weapon_plot.png",
    plot = weapon_plot,
    path = location <- paste(directory,extension, sep=""),
    width = 12,
    height = 8,
    dpi = resolutionval
)