
# Astrangia Quiescence under Climate Change 
    # Author: T. Lindsay 

# Data collection: 
    # T. Lindsay & J. Girard
    # Winter 2025
    # Fort Wetherill State Park 
# Temperature Data: 
    # https://www.ndbc.noaa.gov/station_history.php?station=nwpr1 

# DONE: SET UP  -----------------------------------------------------------------

# WORKING DIRECTORY 
setwd('~/Desktop/GITHUB/Draft_Quiescence/')

# LIBRARIES
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyr)
library("lubridate")
library(hms)
library(readr) # for reading txt file
library(cowplot)
library(MASS) # for ordinal stats 
library(scales) # for ggplot scale sig figs 
library(lme4)
library("lmerTest")

# DONE: BUOY Water Temp --------------------------------------------------------------

# READ & PREP BUOY DATA

# Read the data, skipping the first two comment lines

# NEWPORT DATA 
N05 <- read.table("Temp_Data/temp_buoys/nwpr1h2005.txt", header = FALSE, skip = 2)
N06 <- read.table("Temp_Data/temp_buoys/nwpr1h2006.txt", header = FALSE, skip = 2)
N07 <- read.table("Temp_Data/temp_buoys/nwpr1h2007.txt", header = FALSE, skip = 2)
N08 <- read.table("Temp_Data/temp_buoys/nwpr1h2008.txt", header = FALSE, skip = 2)
N09 <- read.table("Temp_Data/temp_buoys/nwpr1h2009.txt", header = FALSE, skip = 2)
N10 <- read.table("Temp_Data/temp_buoys/nwpr1h2010.txt", header = FALSE, skip = 2)
N11 <- read.table("Temp_Data/temp_buoys/nwpr1h2011.txt", header = FALSE, skip = 2)
N12 <- read.table("Temp_Data/temp_buoys/nwpr1h2012.txt", header = FALSE, skip = 2)
N13 <- read.table("Temp_Data/temp_buoys/nwpr1h2013.txt", header = FALSE, skip = 2)
N14 <- read.table("Temp_Data/temp_buoys/nwpr1h2014.txt", header = FALSE, skip = 2)
N15 <- read.table("Temp_Data/temp_buoys/nwpr1h2015.txt", header = FALSE, skip = 2)
N16 <- read.table("Temp_Data/temp_buoys/nwpr1h2016.txt", header = FALSE, skip = 2)
N17 <- read.table("Temp_Data/temp_buoys/nwpr1h2017.txt", header = FALSE, skip = 2)
N18 <- read.table("Temp_Data/temp_buoys/nwpr1h2018.txt", header = FALSE, skip = 2)
N19 <- read.table("Temp_Data/temp_buoys/nwpr1h2019.txt", header = FALSE, skip = 2)
N20 <- read.table("Temp_Data/temp_buoys/nwpr1h2020.txt", header = FALSE, skip = 2)
N21 <- read.table("Temp_Data/temp_buoys/nwpr1h2021.txt", header = FALSE, skip = 2) 
N22 <- read.table("Temp_Data/temp_buoys/nwpr1h2022.txt", header = FALSE, skip = 2)
N23 <- read.table("Temp_Data/temp_buoys/nwpr1h2023.txt", header = FALSE, skip = 2)
N24 <- read.table("Temp_Data/temp_buoys/nwpr1h2024.txt", header = FALSE, skip = 2)

N25_1 <- read.table("Temp_Data/temp_buoys/nwpr202501.txt", header = FALSE, skip = 2)
N25_2 <- read.table("Temp_Data/temp_buoys/nwpr202502.txt", header = FALSE, skip = 2)
N25_3 <- read.table("Temp_Data/temp_buoys/nwpr202503.txt", header = FALSE, skip = 2)
N25_4 <- read.table("Temp_Data/temp_buoys/nwpr202504.txt", header = FALSE, skip = 2)
N25_5 <- read.table("Temp_Data/temp_buoys/nwpr202505.txt", header = FALSE, skip = 2)
N25_6 <- read.table("Temp_Data/temp_buoys/nwpr202506.txt", header = FALSE, skip = 2)
N25_7 <- read.table("Temp_Data/temp_buoys/nwpr202507.txt", header = FALSE, skip = 2)
N25_8 <- read.table("Temp_Data/temp_buoys/nwpr202508.txt", header = FALSE, skip = 2)

# WHOI DATA 
W05 <- read.table("Temp_Data/temp_buoys/bzbm3h2005.txt", header = FALSE, skip = 2)
W06 <- read.table("Temp_Data/temp_buoys/bzbm3h2006.txt", header = FALSE, skip = 2)
W07 <- read.table("Temp_Data/temp_buoys/bzbm3h2007.txt", header = FALSE, skip = 2)
W08 <- read.table("Temp_Data/temp_buoys/bzbm3h2008.txt", header = FALSE, skip = 2)
W09 <- read.table("Temp_Data/temp_buoys/bzbm3h2009.txt", header = FALSE, skip = 2)
W10 <- read.table("Temp_Data/temp_buoys/bzbm3h2010.txt", header = FALSE, skip = 2)
W11 <- read.table("Temp_Data/temp_buoys/bzbm3h2011.txt", header = FALSE, skip = 2)
W12 <- read.table("Temp_Data/temp_buoys/bzbm3h2012.txt", header = FALSE, skip = 2)
W13 <- read.table("Temp_Data/temp_buoys/bzbm3h2013.txt", header = FALSE, skip = 2)
W14 <- read.table("Temp_Data/temp_buoys/bzbm3h2014.txt", header = FALSE, skip = 2)
W15 <- read.table("Temp_Data/temp_buoys/bzbm3h2015.txt", header = FALSE, skip = 2)
W16 <- read.table("Temp_Data/temp_buoys/bzbm3h2016.txt", header = FALSE, skip = 2)
W17 <- read.table("Temp_Data/temp_buoys/bzbm3h2017.txt", header = FALSE, skip = 2)
W18 <- read.table("Temp_Data/temp_buoys/bzbm3h2018.txt", header = FALSE, skip = 2)
W19 <- read.table("Temp_Data/temp_buoys/bzbm3h2019.txt", header = FALSE, skip = 2)
W20 <- read.table("Temp_Data/temp_buoys/bzbm3h2020.txt", header = FALSE, skip = 2)
W21 <- read.table("Temp_Data/temp_buoys/bzbm3h2021.txt", header = FALSE, skip = 2) 
W22 <- read.table("Temp_Data/temp_buoys/bzbm3h2022.txt", header = FALSE, skip = 2)
W23 <- read.table("Temp_Data/temp_buoys/bzbm3h2023.txt", header = FALSE, skip = 2)
W24 <- read.table("Temp_Data/temp_buoys/bzbm3h2024.txt", header = FALSE, skip = 2)

W25_1 <- read.table("Temp_Data/temp_buoys/bzbm202501.txt", header = FALSE, skip = 2)
W25_2 <- read.table("Temp_Data/temp_buoys/bzbm202502.txt", header = FALSE, skip = 2)
W25_3 <- read.table("Temp_Data/temp_buoys/bzbm202503.txt", header = FALSE, skip = 2)
W25_4 <- read.table("Temp_Data/temp_buoys/bzbm202504.txt", header = FALSE, skip = 2)
W25_5 <- read.table("Temp_Data/temp_buoys/bzbm202505.txt", header = FALSE, skip = 2)
W25_6 <- read.table("Temp_Data/temp_buoys/bzbm202506.txt", header = FALSE, skip = 2)
W25_7 <- read.table("Temp_Data/temp_buoys/bzbm202507.txt", header = FALSE, skip = 2)
W25_8 <- read.table("Temp_Data/temp_buoys/bzbm202508.txt", header = FALSE, skip = 2)

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
buoy <- long %>% filter(WTMP < 999)

# READ BUOY DATA 
#buoy <- read.csv("Temp_Data/Buoy_temp_data.csv")

# DAILY WATER TEMP MEANS  
buoy_daily <- buoy %>%
  group_by(DD, MM, YY, month_day, Site, date) %>%
  summarise(mean_WTMP = mean(WTMP)) 

# CREATE FILE FOR X AXIS LABELS  
first_of_month <- unique(buoy_daily$month_day[grepl("-01$", buoy_daily$month_day)])

# DONE: HOBO Water Temp ---------------------------------------------------------

# READ HOBO DATA 
hobos <- read.csv("Temp_Data/temp_hobos/HOBOS_winter25.csv")

# CONVERT TO DATETIME  
hobos$datetime <- mdy_hm(hobos$datetime)
hobos$month_day <- format(hobos$datetime, "%m-%d")

# CREATE DATE COLUMN 
hobos$date <- as_date(hobos$datetime)

# REORGANIZE DATAFRAME 
hobo_temp <- hobos %>%
  dplyr::select(date, datetime, month_day, temp_deep, temp_shallow) %>%
  pivot_longer(c(temp_deep, temp_shallow), names_to = "depth", values_to = "temp")

# PLOT  
ggplot(hobo_temp, aes(x=datetime, y=temp, color = depth)) + 
  geom_line()

# DAILY WATER TEMPERATURE MEANS 
hobo_temp_mean <- hobo_temp %>%
  group_by(depth, month_day) %>%
  summarise(mean_WTMP = mean(temp, na.rm = TRUE)) 

# DONE: FIG1. NEWPORT ENVIRO 2025 ---------------------------------------------------------

# BUOY DATA FOR 2025 
NOAA_2025 <- buoy_daily %>%
  filter(YY == 2025,) %>%
  filter(Site == "Newport") %>%
  mutate(depth = "temp_surface") %>%
  ungroup() %>%
  dplyr::select(month_day, depth, mean_WTMP) 

# MERGE HOBO & BUOY DATA  
combined2 <- bind_rows(hobo_temp_mean, NOAA_2025)
combined2 <- combined2 %>% filter(month_day < "04-25") %>% filter(month_day > "01-08") 

combined2$depth<- factor(combined2$depth, levels = c("temp_surface", "temp_shallow", "temp_deep"))

# PLOT FIG 1B
temps_line <- ggplot(combined2, aes(x=month_day, y=mean_WTMP, group = depth, color = depth)) + 
  # DATA 
  geom_line(size = 1) +
  # AESTHETICS 
  theme_bw() +
  theme(legend.position = c(.9, .15), 
        legend.background = element_rect(color = "black", fill = "white"),
        axis.title.y = element_blank()) + 
  scale_x_discrete(breaks = first_of_month) + 
  labs(x="Month & Day", color="Depth (m)") +
  scale_color_manual(
    values = c("temp_surface" = "lightblue", "temp_shallow" = "#4488FF", "temp_deep" = "darkblue"),
    labels = c("temp_surface" = "0", "temp_shallow" = "6", "temp_deep" = "11.5")) 
#geom_hline(yintercept = 5, linetype="dotted", linewidth=0.4, color = "gray") 
#ylim(1, 14) + 
temps_line

# PREP DATA FOR BOXPLOTS 
boxplot_prep <- combined2 %>%
  filter(depth != "temp_2014") %>%
  filter(month_day >= "01-08") %>%
  filter(month_day <= "04-25") #%>%
#filter(depth != "temp_surface")

# CREATE TREATMENT COMPARISONS & REORDER 
treatment_comparisons <- list( c("temp_deep","temp_shallow"), c("temp_surface","temp_deep"), c("temp_shallow", "temp_surface"))
boxplot_prep$depth <- factor(boxplot_prep$depth, levels = c("temp_surface", "temp_shallow", "temp_deep"))

# PLOT FIG 1A
temp_boxplot <- ggplot(boxplot_prep, aes(x=depth, y=mean_WTMP, fill=depth)) + 
  geom_boxplot() +
  stat_compare_means(method = "t.test",  comparisons = treatment_comparisons, label = "p.format") +
  theme_bw() +
  scale_fill_manual(values = c("temp_surface" = "lightblue", "temp_shallow"= "#4488FF", "temp_deep"= "darkblue")) +
  labs(x = "Depth (m)", y = "Water Temperature (˚C)") + ### from 1/8 - 4/25 
  scale_x_discrete(labels = c("temp_surface" = "0", "temp_shallow" = "6", "temp_deep" = "11.5")) + 
  theme(#text = element_text( size=20), 
    legend.position = "none") #+
  #ylim(1, 13) 
temp_boxplot

# COMBINE FIG 1
g_treat1 <- plot_grid(temp_boxplot, temps_line,
                      ncol = 2, align = "h", rel_widths = c(1, 3),
                      labels = c("A", "B"), label_size = 12, label_fontface = "bold",  label_x = 0, label_y = 1)
# SAVE FIG 1
ggsave("FIG1_enviro_2025.jpg", plot = g_treat1, path = 'FIGURES/', width = 8, height = 6)

# DONE: FIG 2. 3 YEAR COMPARISON ---------------------------------------------------

# SELECT WINTER DATES ONLY 
winter_dates <- buoy_daily %>%
  filter(
    # For YY 2013, 2020, 2024: include Oct 1 - Dec 31
    (month_day >= "11-01" & month_day <= "12-31") |
      # For YY 2014, 2021, 2024: include Jan 1 - May 30
      (month_day >= "01-01" & month_day <= "06-10")
  )

# MUTATE DATES TO BE CHRONOLOGICAL 
winter_dates <- winter_dates %>%
  mutate(
    custom_day = case_when(
      MM == 10 ~ DD,
      MM == 11 ~ DD + 31,
      MM == 12 ~ DD + 61,
      MM == 1 ~ DD + 92,
      MM == 2 ~ DD + 123,
      MM == 3 ~ DD + 152,
      MM == 4 ~ DD + 183,
      MM == 5 ~ DD + 213,
      MM == 6 ~ DD + 244,
      TRUE ~ NA_real_ ))

# FILTER TO OUR FOCUS YEARS 
yrs_clean_daily <- winter_dates %>%
  filter(
    (YY %in% c(2014, 2013, 2024, 2025) & Site == "Newport") |
      (YY %in% c(2020, 2021) & Site == "WHOI")
  )

# PICK RELEVANT DATES: OCT 1 - JUNE 30 
yrs_clean_daily <- yrs_clean_daily %>%
  filter(
    # For YY 2013, 2020, 2024: include Oct 1 - Dec 31
    (YY %in% c(2013, 2020, 2024) & month_day >= "11-01" & month_day <= "12-31") |
      # For YY 2014, 2021, 2024: include Jan 1 - May 30
      (YY %in% c(2014, 2021, 2024) & month_day >= "01-01" & month_day <= "06-10")
  )

# GROUP THE SAME YEARS TOGETHER 
yrs_clean_daily <- yrs_clean_daily %>%
  mutate(
    yr_group = case_when(
      YY %in% c(2013, 2014) ~ "2013-14 (RI)",
      YY %in% c(2020, 2021) ~ "2020-21 (MA)", 
      YY %in% c(2024, 2025) ~ "2024-25 (RI)",
      TRUE ~ NA_character_  # For any other years not specified
    )
  ) %>% 
  mutate(
    WK = ((custom_day - 1) %/% 7) + 1,  # Create week bins
    WK = ((WK - 1) * 7) + 1   )          # Convert to first day of each week

# CALCULATE WEEKLY MEANS 
week_means <- yrs_clean_daily %>%
  group_by(yr_group,WK) %>%
  summarise(mean_WTMP = mean(mean_WTMP))  

# PULL FIRST OF THE MONTH LABELS FOR THE GRAPH  
first_days <- yrs_clean_daily %>%
  filter(DD == 1) %>%                # Select rows where day is 1
  distinct(custom_day, month_day) %>% # Remove duplicates
  arrange(custom_day)                 # Ensure proper order

# DATAFRAME FOR SHADED 'COLD' PERIODS 
shade_data <- data.frame(
  yr_group = c("2024-25 (RI)", "2020-21 (MA)", "2013-14 (RI)"), # Match your facet groups
  xmin = c(106, 79, 71),     # Start days for shaded regions
  xmax = c(168, 183, 189),     # End days for shaded regions
  ymin = -Inf,                      # Extend to bottom of plot
  ymax = Inf                        # Extend to top of plot
)

# CREATE DATA FRAME FOR QUIESCENCE LINES 
line_data <- data.frame(
  yr_group = c("2024-25 (RI)", "2020-21 (MA)", "2013-14 (RI)"),
  xstart = c(135, 78, 81),     # Start x position for each line
  xend = c(178, 182, 243),     # End x position for each line
  y = 15                       # y position (all at 15)
)

# ORDER YEARS NEWEST-OLDEST
yrs_clean_daily$yr_group <- factor(yrs_clean_daily$yr_group, levels = c("2024-25 (RI)", "2020-21 (MA)", "2013-14 (RI)"))

# PLOT 
faceted_3yr <- ggplot(yrs_clean_daily, aes(x = custom_day, y = mean_WTMP)) +
  # DATA 
  geom_rect(data = shade_data, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  geom_segment(data = line_data, 
               aes(x = xstart, xend = xend, y = y, yend = y),
               color = "blue", linewidth = 1, alpha = 0.7, linetype="dashed") +
  geom_line(linewidth = 1) +
  #AESTHETICS 
  facet_wrap(~ yr_group, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Month & Day", y = "Surface Water Temperature (˚C)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold", size = 12)) +
  scale_x_continuous(breaks = first_days$custom_day, labels = first_days$month_day)
faceted_3yr

# SAVE
ggsave("FIG2_enviro_3years.jpg", plot = faceted_3yr, path = 'FIGURES/', width = 5, height = 8)

# DONE: FIG 3. 2025 QUIESCENCE -----------------------------------------------------------

# LOAD DATA 
raw <- read.csv('Quiescence_raw.csv')
raw$Date <- mdy(raw$Date)

# BASIC QUESTIONS 

# WHAT ARE THE EARLIEST AND LATEST DATES OF ANY QUIESCENCE? 
qui <- raw %>% filter(rating > 0)
max(qui$Date) # 4/10/25
min(qui$Date) # 2/12/25

# CALCULATE % QUIESCENCE
raw_percent <- raw %>% 
  group_by(Date, rating) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(Date, rating, fill = list(count = 0)) %>%
  mutate(percent = count/40*100) %>%
  mutate(
    Date = as.POSIXct(Date),
    rating = as.factor(rating))  # Convert rating to factor

# PLOT FIG 3A
plot1_all <- ggplot(raw_percent, aes(x = Date, y = percent, fill = rating)) +
  geom_area(position = "stack") + 
  theme_bw() +
  labs(x = "Date", y = "Percent of Corals in Quiescence", fill = "Rating") +
  scale_y_continuous(expand = c(0, 0)) +  # Remove y-axis padding
  scale_x_datetime(expand = c(0, 0)) +    # Remove x-axis padding
  scale_fill_manual(
    values = c(
      "0" = "#F1F1F1",
      "1" = "gray", 
      "2" = "gray41",
      "3" = "black")) +
  theme(legend.position = c(.91, .77), 
        legend.background = element_rect(color = "black", fill = "white")) 
plot1_all

# MEREGE ANY RATING OF 1 OR GERATER INTO 'QUIESCENT' 
pres <- raw %>%
  mutate(presence = if_else(rating == 0, 0, 1)) %>%
  #mutate(qual = case_when(rating == 0 ~ 0, rating %in% c(1, 2) ~ 1, rating == 3 ~ 2)) %>%
  mutate(id = paste(depth_cat, morphotype, sep = "_")) %>%
  group_by(sampling_day, Date, morphotype, depth_cat, id) %>% #, qual
  summarise(pres_abs = sum(presence))

# REORDER 
pres$id <- factor(pres$id, levels = c("shallow_Sym","shallow_Apo", "deep_Sym", "deep_Apo"))

#### PLOT BY BOTH DEPTH AND MORPHOTYPE

# SUMMARIZE
pres_all <- pres %>%
  group_by(Date, morphotype, depth_cat, sampling_day) %>%
  summarise(percent_q = sum(pres_abs))

# RELABEL FACETS 
new_labels <- c("deep" = "Deep (11.5 m)","shallow" = "Shallow (6 m)")

# REORDER FACETS 
pres_all$depth_cat <- factor(pres_all$depth_cat, levels = c("shallow", "deep"), ordered = TRUE)

# PLOT 
quies_all <- ggplot(pres_all, aes(x = Date, y = percent_q, color=morphotype)) + 
  geom_point(size=3) +
  scale_y_continuous(labels = label_number(accuracy = 1),
                     limits = c(0,10)) + # Two decimal places
  geom_line(aes(group=morphotype), linetype="dashed", linewidth=1) +
  facet_wrap(~depth_cat, labeller = labeller(depth_cat = new_labels)) +
  # Aesthetics 
  scale_color_manual(values = c("#e4d2ba", "#8a6136"),
                     labels = c("Apo" = "Aposymbiotic", "Sym" = "Symbiotic")) + 
  theme_bw() +
  labs(x="Date", y= "Number of Corals in Quiescence", color = "Morphotype") + 
  #ylim(0,10) +
  theme(legend.position = c(.75, .85), 
        legend.background = element_rect(color = "black", fill = "white")) 
quies_all

ggsave("FIGS1_quies_all.jpg", plot = quies_all, path = 'FIGURES/', width = 5, height = 4)

#### PLOT BY MORPHOTYPE

# PREP MORPHOTYPE DATA
pres_morphotype <- pres %>%
  group_by(Date, morphotype) %>%
  summarise(percent_q = sum(pres_abs))

# PLOT FIG 3B
plot2_morphotype <- ggplot(pres_morphotype , aes(x = Date, y = percent_q, color=morphotype)) + 
  geom_point(size=3) +
  geom_line(aes(group=morphotype), linetype="dashed", linewidth=1) +
  # Aesthetics 
  scale_color_manual(values = c("#e4d2ba", "#8a6136"),
                     labels = c("Apo" = "Aposymbiotic", "Sym" = "Symbiotic")) + 
  theme_bw() +
  labs(x="Date", y= "Number of Corals in Quiescence", color = "Morphotype") + 
  ylim(0,20) +
  theme(legend.position = c(.65, .85), 
        legend.background = element_rect(color = "black", fill = "white")) 
plot2_morphotype

##### PLOT BY DEPTH 

# PREP DEPTH DATA 
pres_depth <- pres %>%
  group_by(Date, depth_cat) %>%
  summarise(percent_q = sum(pres_abs))

# REORDER 
pres_depth$depth_cat <- factor(pres_depth$depth_cat, levels = c("shallow", "deep"), ordered = TRUE)

# PLOT FIG 3C
plot3_depth <- ggplot(pres_depth , aes(x = Date, y = percent_q, color=depth_cat)) + 
  geom_point(size=3) +
  geom_line(aes(group=depth_cat), linetype="dashed", linewidth=1) +
  # Aesthetics 
  scale_color_manual(values = c("#4488FF", "darkblue"),     
                     labels = c("shallow" = "6.0", "deep" = "11.5")) + 
  theme_bw() +
  labs(x="Date", y= "Number of Corals in Quiescence", color = "Depth (m)") +
  ylim(0,20) +
  theme(legend.position = c(.75, .85), 
        legend.background = element_rect(color = "black", fill = "white")) 
plot3_depth

# MERGE FIG 3 SUBPLOTS 
plot_quiescence <- plot_grid(plot2_morphotype, plot3_depth, labels = c("B", "C"),
                             ncol = 2)
plot_quiescence2 <- plot_grid(plot1_all, plot_quiescence,  labels = c("A", ""),
                              ncol = 1)

# SAVE FIG 3
ggsave("FIG3_quiescence.jpg", plot = plot_quiescence2, path = 'FIGURES/', width = 5, height = 8)

# STATS - ORDINAL MODEL -----------------------------------------------------------

# GET ONE VALUE FOR EACH DEEP AND SHALLOW, ONE WEEK LEADING UP TO THE SAMPLING DAY
# IMPORT WEEKLY DATA  
pre_temp <- read.csv("Temp_Data/temp_hobos/HOBOS_winter25_week.csv")

# READ AS DATETIME 
pre_temp$datetime <- mdy_hm(pre_temp$datetime)
pre_temp$month_day <- format(pre_temp$datetime, "%m-%d")

# NEW DATE COLUMN
pre_temp$date <- as_date(pre_temp$datetime)

# PIVOT DATAFRAME 
pre_temp_long <- pre_temp %>%
  pivot_longer(c(deep, shallow), names_to = "depth_cat", values_to = "temp")

# CREATE BOXPLOT TO CHECK DATA 
ggplot(pre_temp_long, aes(x= as.character(pre_7day), y=temp)) +
  geom_boxplot() +
  facet_wrap(~depth_cat)

# CALCULATE 7 DAY MEAN
mean_7d <- pre_temp_long %>%
  group_by(depth_cat, pre_7day) %>%
  summarise(mean_WTMP = mean(temp, na.rm = TRUE)) %>%
  mutate(sampling_day = pre_7day) %>%
  mutate(sampling_time = "7d") %>%
  dplyr::select(!pre_7day)

# ADD BACK TO RAW DATA 
raw <- full_join(raw, mean_7d)

# COMBINE WEEK MEAN WITH DATA 
temp_quies <- full_join(mean_7d, pres_all)

# PLOT 
ggplot(temp_quies, aes(x = mean_WTMP, y = percent_q, color=morphotype)) +
  geom_smooth(method="lm")+
  geom_jitter(aes(shape = depth_cat), width = 0.05, height = 0.05) +
  theme_bw()
  #ylim(-0.2,7.5)


### ORDINAL STATS 

# ENSURE RANKED VARIABLE IS PROPERLY ORDERED 
raw$rating <- factor(raw$rating, levels = c(0, 1, 2, 3), ordered = TRUE)

# CHECK STRUCTURE 
str(raw$rating)

# RUN MODEL
model <- polr(rating ~ depth + morphotype + mean_WTMP, #+ morphotype * mean_WTMP + morphotype * depth + depth * mean_WTMP + depth * morphotype * mean_WTMP, 
              data = raw, Hess = TRUE)

# GET SUMMARY WITH P-VALUES
summary(model)

# MODEL OUTPUTS
coefficient_table <- coef(summary(model))
p_values <- pnorm(abs(coefficient_table[, "t value"]), lower.tail = FALSE) * 2
cbind(coefficient_table, "p value" = p_values)

# ODDS RATIOS
exp(coef(model))
# For morphotype Sym, this will be ~2.73 meaning:
# Sym morphotype has about 2.732 times higher odds of being in a higher rank category compared to Apo morphotype

# COMPARE AGAINST NULL MODEL
null_model <- polr(rating ~ 1, data = raw, Hess = TRUE)
anova(null_model, model) # depth + morphotype is not more important than null 

# STATS - RI vs. MA TEMPS----------------------------------------------

# CALCULATE 20-YEAR MEAN TEMPS 
site_means <- buoy_daily %>%
  group_by(Site) %>%
  summarise(Mean_Temp = mean(mean_WTMP, na.rm = TRUE))
site_means

# ADD INTERACTIONS BETWEEN SITE AND YEAR 
simple_model <- lmer(mean_WTMP ~ Site + YY + (1 | MM), data = buoy_daily)
summary(simple_model)

# MODEL RI ONLY
nb_data <- buoy_daily %>% filter(Site == "Newport")
nb_model <- lmer(mean_WTMP ~ YY + (1 | MM), data = nb_data)
summary(nb_model)

# MODEL MA ONLY 
whoi_data <- buoy_daily %>% filter(Site == "WHOI")
whoi_model <- lmer(mean_WTMP ~ YY + (1 | MM), data = whoi_data)
summary(whoi_model)

# Key Findings

# WHOI is warmer than NB p < 2e-16
# WHOI is warming faster than NB  p < 2e-16 
# WHOI warming rate = 0.06208°C per year, p = 0.00589
# NB is still warming significantly = 0.00994°C per year, p < 2e-16

# Site	  mean temp   Warming Rate	  Significance  20-Year Warming 
# NB	    12.1        0.0099°C/year	  p = 0.00589	  ~0.20°C
# WHOI	  12.3        0.0621°C/year	  p < 2e-16	    ~1.24°C

# NOT USING: GAANT ------------------------------------------------------------------
winter_weeks <- winter_dates %>% 
  mutate(
    WK = ((custom_day - 1) %/% 7) + 1)

# calculate weekly means 
week_means <- winter_weeks %>%
  group_by(YY,Site, WK) %>%
  summarise(mean_WTMP = mean(mean_WTMP))  

##### FIX 
# pull first of the month labels for the graph 
first_days <- yrs_clean_daily %>%
  filter(DD == 1) %>%                # Select rows where day is 1
  distinct(custom_day, month_day) %>% # Remove duplicates
  arrange(custom_day)                 # Ensure proper order
yrs_clean_daily$yr_group <- factor(yrs_clean_daily$yr_group, levels = c("2024-25 (RI)", "2020-21 (MA)", "2013-14 (RI)"))

buoy <- buoy %>%
  mutate(
    day_of_year = yday(as.Date(paste(YY, MM, DD, sep = "-"))),
    # Calculate weeks within each year separately
    WK = ((day_of_year - 1) %/% 7) + 1
  #  WK = ((WK - 1) * 7) + 1
  )

week_means <- week_means  %>%
  mutate(temp_category = ifelse(mean_WTMP < 6, "cold", "hot"))

ggplot(buoy_Weekly, aes(x=YY, y=mean_WTMP)) +
  geom_point() + 
  facet_wrap(~Site)

# Assuming your dataframe is called 'df'
cold_periods <- week_means %>%
  # Filter only cold records
  filter(temp_category == "cold") %>%
  # Sort by year, site, and week
  arrange(YY, Site, WK) %>%
  # Group by year and site
  group_by(YY, Site) %>%
  # Identify consecutive weeks by checking if difference from previous week is 1
  mutate(
    gap = WK - lag(WK, default = first(WK) - 1) > 1,
    group_id = cumsum(gap)
  ) %>%
  # Group by the consecutive groups
  group_by(YY, Site, group_id) %>%
  # Get start and end weeks for each consecutive cold period
  summarise(
    start_date = min(WK),
    end_date = max(WK),
    .groups = 'drop'
  ) %>%
  # Remove the temporary grouping column
  dplyr::select(-group_id)

first_days2 <- winter_weeks %>%
  filter(DD == 1) %>%                # Select rows where day is 1
  distinct(WK, month_day) %>% # Remove duplicates
  arrange(WK)                 # Ensure proper order

# Create the Gantt chart
gaant <- ggplot(cold_periods, aes(x = start_date, y = as.factor(YY))) +
  geom_segment(
    aes(xend = end_date, yend = as.factor(YY), color = Site),
    linewidth = 6,  # Thickness of the bars
    alpha = 0.7
  ) +
 scale_x_continuous(breaks = first_days2$WK, labels = first_days2$month_day) +
  facet_wrap(~ Site, scales = "free_y", ncol = 2) +
  labs(x = "Week Number", y = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )
gaant

ggsave("gaant.jpg", plot = gaant, path = 'Other_Graphs/', width = 15, height = 8)


# NOT USING: TRACK 6˚ TEMPS---------------------------------------------------------------------

### HOW MANY WEEKS ARE > 6˚C?
week_means <- week_means %>%
  mutate(WYY = ifelse(WK >= 14, YY, YY + 1))

# Count cold weeks per year for each site
cold_weeks_count <- week_means %>%
  group_by(WYY, Site) %>%
  summarise(
    total_weeks = n(),
    cold_weeks = sum(mean_WTMP < 6, na.rm = TRUE),
    cold_week_percentage = (cold_weeks / total_weeks) * 100,
    .groups = 'drop'
  ) %>%
  filter(total_weeks >= 30)

ggplot(cold_weeks_count, aes(x=WYY, y = cold_weeks)) + 
  facet_wrap(~Site, scales = "free") + 
  geom_point() +
  geom_smooth(method = "lm")

ggplot(cold_weeks_count, aes(x=WYY, y = cold_weeks, color = Site)) + 
  #facet_wrap(~S ite, scales = "free") + 
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_bw()

# There are some weeks missing from the dataset -2005, 2012 WHOI etc.
ggplot(buoy_daily, aes(x=month_day, y= mean_WTMP)) +
  facet_wrap(~ YY + Site) + 
  geom_point()

ggplot(buoy_daily, aes(x=date, y=mean_WTMP, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm")


# NOT USING: LIGHT DATA ------------------------------------------

# light 
hobo_light <- hobos %>%
  dplyr::select(date, datetime, month_day, light_deep, light_shallow) %>%
  pivot_longer(c(light_deep, light_shallow), names_to = "depth", values_to = "light")

ggplot(hobo_light, aes(x=datetime, y=light, color = depth)) + 
  geom_line()

# Light 
hobo_light_mean <- hobo_light %>%
  group_by(date, depth) %>%
  summarise(mean_light = mean(light, na.rm = TRUE)) 

ggplot(hobo_light_mean, aes(x=date, y=mean_light, color = depth)) + 
  geom_line() 

# NOT USING: FIRST ENVIRO PLOT -------------------------------------------------------------

important_years <- buoy_daily %>% filter(YY %in% c(2014, 2021, 2025)) # can try with 2023 and 2024

# graph daily water temp over many years 
annual_w <- ggplot() +    
  geom_line(data = buoy_daily, aes(x = month_day, y = mean_WTMP, group=as.factor(YY)),color="lightgray", size=0.5) +  
  #geom_line(data = other_years, aes(x = month_day, y = mean_WTMP, group=as.factor(YY)),color="#b056a8", size=0.5) +  
  #geom_line(data = important_years, aes(x = month_day, y = mean_WTMP, group=as.factor(YY), color=as.factor(YY), fill=as.factor(YY)), size = 1) +  
  geom_smooth(data = important_years, aes(x = month_day, y = mean_WTMP, group=as.factor(YY), color=as.factor(YY), fill=as.factor(YY)), method = "loess", span=0.09, linewidth = 1.5) + 
  scale_x_discrete(breaks = first_of_month) + 
  scale_color_manual(values = c("2014" = "#300A5BFF", "2023" = "#b056a8", "2024" = "#D64B40FF","2025" = "orange", "2021" = "blue")) + 
  scale_fill_manual(values = c("2014" = "#300A5BFF", "2023" = "#b056a8", "2024" = "#D64B40FF","2025" = "orange", "2021" = "blue")) + 
  
  geom_segment(aes(x = "01-01", xend = "06-01", y = 22, yend = 22), color = "#300A5BFF", linewidth = 1, linetype = "dashed") +
  geom_text(aes(x = "03-05", y = 22.5, label = "2014 (RI)"), color = "black", size=5, family = "sans", fontface = "plain", check_overlap = TRUE) +
  
  geom_segment(aes(x = "01-01", xend = "03-31", y = 20, yend = 20), color = "blue", linewidth = 1, linetype = "dashed") +
  geom_text(aes(x = "03-05", y = 20.5, label = "2021 (MA)"), color = "black", size=5, family = "sans", fontface = "plain", check_overlap = TRUE) + 
  
  geom_segment(aes(x = "02-12", xend = "03-26", y = 18, yend = 18), color = "orange", linewidth = 1, linetype = "dashed") +
  geom_text(aes(x = "03-05", y = 18.5, label = "2025 (RI)"), color = "black", size=5, family = "sans", fontface = "plain", check_overlap = TRUE) + 
  
  labs(x="Month & Day", y = "Surface Water Temperature (˚C)", color="Year", fill = "Year") + 
  theme_bw() +
  theme(legend.position = c(.9, .9), 
        legend.background = element_rect(color = "black", fill = "white"),)
annual_w

# save 
#ggsave("FIG2_enviro_yearsA.jpg", plot = annual_w, path = 'FIGURES/', width = 8, height = 8)


# NOT USING: SECOND ENVIRO PLOT  ------------------------------------------

# PLOT!!!!! 
annual_wb <- ggplot(yrs_clean_daily, aes(x = custom_day, y = mean_WTMP, color = yr_group)) +
  geom_line(linewidth = 1) + 
  # geom_smooth(method = "loess", span=0.09, linewidth = 1.5) +
  scale_x_continuous(breaks = first_days$custom_day, labels = first_days$month_day) +
  scale_color_manual(values = c("2013-14 (RI)" = "#4A1A7BFF","2020-21 (MA)" = "#48D1CCFF","2024-25 (RI)" = "#D64B40FF")) + 
  scale_fill_manual(values = c("2013-14 (RI)" = "#4A1A7BFF","2020-21 (MA)" = "#48D1CCFF","2024-25 (RI)" = "#D64B40FF")) + 
  
  geom_segment(aes(x = 71, xend = 189, y = 17, yend = 17), color = "#AA92E9FF", linewidth = 6, linetype = "solid", alpha = 0.02) + # GRACE, 2014: Dec 20 - may 30 (estimates) days 81 through 243
  geom_segment(aes(x = 81, xend = 243, y = 17, yend = 17), color = "#4A1A7BFF", linewidth = 1, linetype = "dashed") + # GRACE, 2014: Dec 20 - may 30 (estimates) days 81 through 243
  #geom_text(aes(x = "03-05", y = 22.5, label = "2014 (RI)"), color = "black", size=5, family = "sans", fontface = "plain", check_overlap = TRUE) +
  geom_segment(aes(x = 78, xend = 182, y = 18, yend = 18), color = "#90FBF6FF", linewidth = 6, linetype = "solid", alpha = 0.02) + # GRACE, 2014: Dec 20 - may 30 (estimates) days 81 through 243
  geom_segment(aes(x = 79, xend = 183, y = 18, yend = 18), color = "#18B1ACFF", linewidth = 1, linetype = "dashed") + # BROWN, 2021: Dec 18 - Mar 31 days 79 thru 183
  #geom_text(aes(x = "03-05", y = 20.5, label = "2021 (MA)"), color = "black", size=5, family = "sans", fontface = "plain", check_overlap = TRUE) + 
  geom_segment(aes(x = 106, xend = 168, y = 19, yend = 19), color = "#FF9F9FFF", linewidth = 6, linetype = "solid", alpha = 0.02) + # GRACE, 2014: Dec 20 - may 30 (estimates) days 81 through 243
  geom_segment(aes(x = 135, xend = 178, y = 19, yend = 19), color = "#D64B40FF", linewidth = 1, linetype = "dashed") + # LINDSAY, 2025: Feb 12 - Mar 26 days 135 - 178
  #geom_text(aes(x = "03-05", y = 18.5, label = "2025 (RI)"), color = "black", size=5, family = "sans", fontface = "plain", check_overlap = TRUE) + 
  theme_bw() +
  theme(legend.position = c(.85, .15), 
        legend.background = element_rect(color = "black", fill = "white"),) + 
  labs(x="Month & Day", y = "Surface Water Temperature (˚C)", color="Year", fill = "Year")  

# graph daily water temp over many years 

annual_wb

# save 
ggsave("FIG3_enviro_yearsB.jpg", plot = annual_wb, path = 'FIGURES/', width = 6, height = 5)




