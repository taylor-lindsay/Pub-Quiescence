
# Astrangia Quiescence under Climate Change 
    # Author: T. Lindsay 
# Objective: merge data from newport & WHOI buoys from 2000 - 2025

# Temperature Data: 
    # https://www.ndbc.noaa.gov/station_history.php?station=nwpr1 
    # 


# Set up  -----------------------------------------------------------------

# Set working directory 
setwd('~/Desktop/GITHUB/Draft_Quiescence/Temp_Data/')

# Libraries
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyr)
library("lubridate")
library(hms)
library(readr) # for reading txt file


# BUOY Water Temp --------------------------------------------------------------

# Read the data, skipping the first two comment lines

# NEWPORT DATA 
N05 <- read.table("temp_buoys/nwpr1h2005.txt", header = FALSE, skip = 2)
N06 <- read.table("temp_buoys/nwpr1h2006.txt", header = FALSE, skip = 2)
N07 <- read.table("temp_buoys/nwpr1h2007.txt", header = FALSE, skip = 2)
N08 <- read.table("temp_buoys/nwpr1h2008.txt", header = FALSE, skip = 2)
N09 <- read.table("temp_buoys/nwpr1h2009.txt", header = FALSE, skip = 2)
N10 <- read.table("temp_buoys/nwpr1h2010.txt", header = FALSE, skip = 2)
N11 <- read.table("temp_buoys/nwpr1h2011.txt", header = FALSE, skip = 2)
N12 <- read.table("temp_buoys/nwpr1h2012.txt", header = FALSE, skip = 2)
N13 <- read.table("temp_buoys/nwpr1h2013.txt", header = FALSE, skip = 2)
N14 <- read.table("temp_buoys/nwpr1h2014.txt", header = FALSE, skip = 2)
N15 <- read.table("temp_buoys/nwpr1h2015.txt", header = FALSE, skip = 2)
N16 <- read.table("temp_buoys/nwpr1h2016.txt", header = FALSE, skip = 2)
N17 <- read.table("temp_buoys/nwpr1h2017.txt", header = FALSE, skip = 2)
N18 <- read.table("temp_buoys/nwpr1h2018.txt", header = FALSE, skip = 2)
N19 <- read.table("temp_buoys/nwpr1h2019.txt", header = FALSE, skip = 2)
N20 <- read.table("temp_buoys/nwpr1h2020.txt", header = FALSE, skip = 2)
N21 <- read.table("temp_buoys/nwpr1h2021.txt", header = FALSE, skip = 2) 
N22 <- read.table("temp_buoys/nwpr1h2022.txt", header = FALSE, skip = 2)
N23 <- read.table("temp_buoys/nwpr1h2023.txt", header = FALSE, skip = 2)
N24 <- read.table("temp_buoys/nwpr1h2024.txt", header = FALSE, skip = 2)

N25_1 <- read.table("temp_buoys/nwpr202501.txt", header = FALSE, skip = 2)
N25_2 <- read.table("temp_buoys/nwpr202502.txt", header = FALSE, skip = 2)
N25_3 <- read.table("temp_buoys/nwpr202503.txt", header = FALSE, skip = 2)
N25_4 <- read.table("temp_buoys/nwpr202504.txt", header = FALSE, skip = 2)
N25_5 <- read.table("temp_buoys/nwpr202505.txt", header = FALSE, skip = 2)
N25_6 <- read.table("temp_buoys/nwpr202506.txt", header = FALSE, skip = 2)
N25_7 <- read.table("temp_buoys/nwpr202507.txt", header = FALSE, skip = 2)
N25_8 <- read.table("temp_buoys/nwpr202508.txt", header = FALSE, skip = 2)
#N25_9 <- read.table("temp_buoys/nwpr202409.txt", header = FALSE, skip = 2)
#N25_10 <- read.table("temp_buoys/nwpr202410.txt", header = FALSE, skip = 2)
#N25_11 <- read.table("temp_buoys/nwpr202411.txt", header = FALSE, skip = 2)
#N25_12 <- read.table("temp_buoys/nwpr202412.txt", header = FALSE, skip = 2)

# WHOI DATA 
W05 <- read.table("temp_buoys/bzbm3h2005.txt", header = FALSE, skip = 2)
W06 <- read.table("temp_buoys/bzbm3h2006.txt", header = FALSE, skip = 2)
W07 <- read.table("temp_buoys/bzbm3h2007.txt", header = FALSE, skip = 2)
W08 <- read.table("temp_buoys/bzbm3h2008.txt", header = FALSE, skip = 2)
W09 <- read.table("temp_buoys/bzbm3h2009.txt", header = FALSE, skip = 2)
W10 <- read.table("temp_buoys/bzbm3h2010.txt", header = FALSE, skip = 2)
W11 <- read.table("temp_buoys/bzbm3h2011.txt", header = FALSE, skip = 2)
W12 <- read.table("temp_buoys/bzbm3h2012.txt", header = FALSE, skip = 2)
W13 <- read.table("temp_buoys/bzbm3h2013.txt", header = FALSE, skip = 2)
W14 <- read.table("temp_buoys/bzbm3h2014.txt", header = FALSE, skip = 2)
W15 <- read.table("temp_buoys/bzbm3h2015.txt", header = FALSE, skip = 2)
W16 <- read.table("temp_buoys/bzbm3h2016.txt", header = FALSE, skip = 2)
W17 <- read.table("temp_buoys/bzbm3h2017.txt", header = FALSE, skip = 2)
W18 <- read.table("temp_buoys/bzbm3h2018.txt", header = FALSE, skip = 2)
W19 <- read.table("temp_buoys/bzbm3h2019.txt", header = FALSE, skip = 2)
W20 <- read.table("temp_buoys/bzbm3h2020.txt", header = FALSE, skip = 2)
W21 <- read.table("temp_buoys/bzbm3h2021.txt", header = FALSE, skip = 2) 
W22 <- read.table("temp_buoys/bzbm3h2022.txt", header = FALSE, skip = 2)
W23 <- read.table("temp_buoys/bzbm3h2023.txt", header = FALSE, skip = 2)
W24 <- read.table("temp_buoys/bzbm3h2024.txt", header = FALSE, skip = 2)

W25_1 <- read.table("temp_buoys/bzbm202501.txt", header = FALSE, skip = 2)
W25_2 <- read.table("temp_buoys/bzbm202502.txt", header = FALSE, skip = 2)
W25_3 <- read.table("temp_buoys/bzbm202503.txt", header = FALSE, skip = 2)
W25_4 <- read.table("temp_buoys/bzbm202504.txt", header = FALSE, skip = 2)
W25_5 <- read.table("temp_buoys/bzbm202505.txt", header = FALSE, skip = 2)
W25_6 <- read.table("temp_buoys/bzbm202506.txt", header = FALSE, skip = 2)
W25_7 <- read.table("temp_buoys/bzbm202507.txt", header = FALSE, skip = 2)
W25_8 <- read.table("temp_buoys/bzbm202508.txt", header = FALSE, skip = 2)
#N25_9 <- read.table("temp_buoys/nwpr202409.txt", header = FALSE, skip = 2)
#N25_10 <- read.table("temp_buoys/nwpr202410.txt", header = FALSE, skip = 2)
#N25_11 <- read.table("temp_buoys/nwpr202411.txt", header = FALSE, skip = 2)
#N25_12 <- read.table("temp_buoys/nwpr202412.txt", header = FALSE, skip = 2)

# bind all from same years 
longN <- rbind(N05, N06, N07, N08, N09, N10, N11, N12, N13, N14, N15, N16, N17, N18, N19, N20, N21, N22, N23, N24, N25_1, N25_2, N25_3, N25_4, N25_5, N25_6, N25_7, N25_8)
longW <- rbind(W05, W06, W07, W08, W09, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21, W22, W23, W24, W25_1, W25_2, W25_3, W25_4, W25_5, W25_6, W25_7, W25_8)  

# add site name 
longN <- longN %>% mutate(site = "Newport")
longW <- longW %>% mutate(site = "WHOI")

long <-rbind(longN, longW)
colnames(long) <- c("YY", "MM", "DD", "hh", "mm", "WDIR", "WSPD", "GST", "WVHT", "DPD", "APD", "MWD", "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE", "Site")

# save only relevant data 
long <- long %>% dplyr::select(c("Site", "YY", "MM", "DD", "hh", "mm", "WTMP"))

# correct formatting in lubridate 
long$datetime <- make_datetime(year = long$YY, month = long$MM, day = long$DD, 
                               hour = long$hh, min = long$mm)
long$date <- make_date(year = long$YY, month = long$MM, day = long$DD)
long$month_day <- format(make_date(year = long$YY, month = long$MM, day = long$DD), "%m-%d") 

# remove errors 
long <- long %>% filter(WTMP < 999)

# daily water temp means 
long_daily <- long %>%
  group_by(DD, MM, YY, month_day, Site) %>%
  summarise(mean_WTMP = mean(WTMP)) 

# check data 
ggplot(long_daily, aes(x=month_day, y=mean_WTMP, color = YY)) + 
  geom_point(linewidth = 0.5) +
  facet_wrap(~Site)

ggplot(long_daily, aes(x=as.character(YY), y=mean_WTMP)) + 
  geom_boxplot()+
  facet_wrap(~Site)

write.csv(long, "Buoy_temp_data.csv", row.names = FALSE)


