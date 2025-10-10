
# Astrangia Quiescence under Climate Change 
    # Author: T. Lindsay 

# Data collection: 
    # T. Lindsay & J. Girard
    # Winter 2025
    # Fort Wetherill State Park 
# Temperature Data: 
    # https://www.ndbc.noaa.gov/station_history.php?station=nwpr1 

# Set up  -----------------------------------------------------------------

# Set working directory 
setwd('~/Desktop/GITHUB/Draft_Quiescence/')

# Libraries
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

# BUOY Water Temp --------------------------------------------------------------

# Read the BUOY data 

buoy <- read.csv("Temp_Data/Buoy_temp_data.csv")

# daily water temp means 
buoy_daily <- buoy %>%
  group_by(DD, MM, YY, month_day, Site, date) %>%
  summarise(mean_WTMP = mean(WTMP)) 

# create file for x axis labels 
first_of_month <- unique(buoy_daily$month_day[grepl("-01$", buoy_daily$month_day)])

# HOBO Water Temp ---------------------------------------------------------

# import HOBO data 
hobos <- read.csv("Temp_Data/temp_hobos/HOBOS_winter25.csv")

# read as datetime 
hobos$datetime <- mdy_hm(hobos$datetime)
hobos$month_day <- format(hobos$datetime, "%m-%d")

# Create a new column with just the date
hobos$date <- as_date(hobos$datetime)

# temp 
hobo_temp <- hobos %>%
  dplyr::select(date, datetime, month_day, temp_deep, temp_shallow) %>%
  pivot_longer(c(temp_deep, temp_shallow), names_to = "depth", values_to = "temp")

# plot 
ggplot(hobo_temp, aes(x=datetime, y=temp, color = depth)) + 
  geom_line()

# calculate daily means 
hobo_temp_mean <- hobo_temp %>%
  group_by(depth, month_day) %>%
  summarise(mean_WTMP = mean(temp, na.rm = TRUE)) 

# FIG1. Enviro Newport 2025 ---------------------------------------------------------

# add surface data 
NOAA_2025 <- buoy_daily %>%
  filter(YY == 2025,) %>%
  filter(Site == "Newport") %>%
  mutate(depth = "temp_surface") %>%
  ungroup() %>%
  dplyr::select(month_day, depth, mean_WTMP) 

# Combine data 
combined2 <- bind_rows(hobo_temp_mean, NOAA_2025)
combined2 <- combined2 %>% filter(month_day < "04-25") %>% filter(month_day > "01-08") 

combined2$depth<- factor(combined2$depth, levels = c("temp_surface", "temp_shallow", "temp_deep"))

# temp line graph without 2014 data 
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

# boxplots 
boxplot_prep <- combined2 %>%
  filter(depth != "temp_2014") %>%
  filter(month_day >= "01-08") %>%
  filter(month_day <= "04-25") #%>%
#filter(depth != "temp_surface")

# create treatment comparisons & organize data 
treatment_comparisons <- list( c("temp_deep","temp_shallow"), c("temp_surface","temp_deep"), c("temp_shallow", "temp_surface"))
boxplot_prep$depth <- factor(boxplot_prep$depth, levels = c("temp_surface", "temp_shallow", "temp_deep"))

# plot boxplot of water temps at 3 depths 
temp_boxplot <- ggplot(boxplot_prep, aes(x=depth, y=mean_WTMP, fill=depth)) + 
  geom_boxplot() +
  stat_compare_means(method = "t.test",  comparisons = treatment_comparisons, label = "p.format") +
  theme_bw() +
  scale_fill_manual(values = c("temp_surface" = "lightblue", "temp_shallow"= "#4488FF", "temp_deep"= "darkblue")) +
  labs(x = "Depth (m)", y = "Water Temperature (˚C)") + ### from 1/8 - 4/25 
  scale_x_discrete(labels = c("temp_surface" = "0", "temp_shallow" = "6", "temp_deep" = "11.5")) + 
  theme(#text = element_text( size=20), 
    legend.position = "none") +
  ylim(1, 13) 
temp_boxplot

# surface:deep p=0.038
# shallow:surface p = 0.083
# deep:shallow p = 0.66s

g_treat1 <- plot_grid(temp_boxplot, temps_line,
                      ncol = 2, align = "h", rel_widths = c(1, 3),
                      labels = c("A", "B"), label_size = 12, label_fontface = "bold",  label_x = 0, label_y = 1)
ggsave("FIG1_enviro_2025.jpg", plot = g_treat1, path = 'FIGURES/', width = 8, height = 6)


# FIG 2. Quiescence Data -----------------------------------------------------------

# LOAD DATA 
raw <- read.csv('Quiescence_raw.csv')
raw$Date <- mdy(raw$Date)

# BASIC QUESTIONS 

# What are the earliest & latest date? 
qui <- raw %>% filter(rating > 0)
max(qui$Date) # 4/10/25
min(qui$Date) # 2/12/15

# PLOT OVERALL QUIESCENCE 
raw_percent <- raw %>% 
  group_by(Date, rating) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(Date, rating, fill = list(count = 0)) %>%
  mutate(percent = count/40*100) %>%
  mutate(
    Date = as.POSIXct(Date),
    rating = as.factor(rating))  # Convert rating to factor

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

# merge any rating of 1 or greater into "quiescent" 
pres <- raw %>%
  mutate(presence = if_else(rating == 0, 0, 1)) %>%
  #mutate(qual = case_when(rating == 0 ~ 0, rating %in% c(1, 2) ~ 1, rating == 3 ~ 2)) %>%
  mutate(id = paste(depth_cat, ecotype, sep = "_")) %>%
  group_by(sampling_day, Date, ecotype, depth_cat, id) %>% #, qual
  summarise(pres_abs = sum(presence))

pres$id <- factor(pres$id, levels = c("shallow_Sym","shallow_Apo", "deep_Sym", "deep_Apo"))

# BY ECOTYPE
pres_ecotype <- pres %>%
  group_by(Date, ecotype) %>%
  summarise(percent_q = sum(pres_abs))

plot2_ecotype <- ggplot(pres_ecotype , aes(x = Date, y = percent_q, color=ecotype)) + 
  geom_point(size=3) +
  geom_line(aes(group=ecotype), linetype="dashed", linewidth=1) +
  # Aesthetics 
  scale_color_manual(values = c("#e4d2ba", "#8a6136"),
                     labels = c("Apo" = "Aposymbiotic", "Sym" = "Symbiotic")) + 
  theme_bw() +
  labs(x="Date", y= "Number of Corals in Quiescence", color = "Ecotype") + 
  ylim(0,20) +
  theme(legend.position = c(.65, .85), 
        legend.background = element_rect(color = "black", fill = "white")) 
plot2_ecotype

# BY DEPTH
pres_depth <- pres %>%
  group_by(Date, depth_cat) %>%
  summarise(percent_q = sum(pres_abs))

pres_depth$depth_cat <- factor(pres_depth$depth_cat, levels = c("shallow", "deep"), ordered = TRUE)

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

plot_quiescence <- plot_grid(plot2_ecotype, plot3_depth, labels = c("B", "C"),
                             ncol = 2)
plot_quiescence2 <- plot_grid(plot1_all, plot_quiescence,  labels = c("A", ""),
                              ncol = 1)

ggsave("FIG2_quiescence.jpg", plot = plot_quiescence2, path = 'FIGURES/', width = 5, height = 8)

# STATS - Ordinal Quiescence -----------------------------------------------------------

# get one value for each deep and shallow for the 1 week leading up to sampling day 

# import HOBO data 
pre_temp <- read.csv("Temp_Data/temp_hobos/HOBOS_winter25_week.csv")

# read as datetime 
pre_temp$datetime <- mdy_hm(pre_temp$datetime)
pre_temp$month_day <- format(pre_temp$datetime, "%m-%d")

# Create a new column with just the date
pre_temp$date <- as_date(pre_temp$datetime)

# temp 
pre_temp_long <- pre_temp %>%
  pivot_longer(c(deep, shallow), names_to = "depth_cat", values_to = "temp")

#boxplot
ggplot(pre_temp_long, aes(x= as.character(pre_7day), y=temp)) +
  geom_boxplot() +
  facet_wrap(~depth_cat)

#7day mean
mean_7d <- pre_temp_long %>%
  group_by(depth_cat, pre_7day) %>%
  summarise(mean_WTMP = mean(temp, na.rm = TRUE)) %>%
  mutate(sampling_day = pre_7day) %>%
  mutate(sampling_time = "7d") %>%
  dplyr::select(!pre_7day)

#48hr mean
mean_48hr <- pre_temp_long %>%
  group_by(depth_cat, pre_48hr) %>%
  summarise(mean_WTMP = mean(temp, na.rm = TRUE)) %>%
  mutate(sampling_day = pre_48hr) %>%
  mutate(sampling_time = "48hr") %>%
  dplyr::select(!pre_48hr)

# 24hr mean 
mean_24hr <- pre_temp_long %>%
  group_by(depth_cat, pre_24hr) %>%
  summarise(mean_WTMP = mean(temp, na.rm = TRUE)) %>%
  mutate(sampling_day = pre_24hr) %>%
  mutate(sampling_time = "24hr") %>%
  dplyr::select(!pre_24hr)

# combine & look 
mean_pre_temps <- rbind(mean_7d, mean_48hr, mean_24hr)  
ggplot(mean_pre_temps, aes(x=sampling_day, y=mean_WTMP, color=depth_cat, shape=sampling_time)) +
  geom_point()

# add this data to the raw data
raw <- full_join(raw, mean_7d)

### ORDINAL STATS 

# Ensure your ranked variable is properly ordered
raw$rating <- factor(raw$rating, levels = c(0, 1, 2, 3), ordered = TRUE)

# Check the structure
str(raw$rating)

# Run model
model <- polr(rating ~ depth + ecotype + mean_WTMP, #+ ecotype * mean_WTMP + ecotype * depth + depth * mean_WTMP + depth * ecotype * mean_WTMP, 
              data = raw, Hess = TRUE)

# Get summary with p-values
summary(model)

coefficient_table <- coef(summary(model))
p_values <- pnorm(abs(coefficient_table[, "t value"]), lower.tail = FALSE) * 2
cbind(coefficient_table, "p value" = p_values)

# Odds ratios
exp(coef(model))
# For ecotypeSym, this will be ~2.14, meaning:
# Sym ecotype has about 2.14 times higher odds of being in a higher rank category compared to Apo ecotype

# Compare against null model
null_model <- polr(rating ~ 1, data = raw, Hess = TRUE)
anova(null_model, model) # depth + ecotype is not more important than null 


# FIG 3. 3 year comparison ---------------------------------------------------

#
winter_dates <- buoy_daily %>%
  filter(
    # For YY 2013, 2020, 2024: include Oct 1 - Dec 31
    (month_day >= "11-01" & month_day <= "12-31") |
      # For YY 2014, 2021, 2024: include Jan 1 - May 30
      (month_day >= "01-01" & month_day <= "06-10")
  )

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

### FOCUS YEARS 

# daily water temp means 
yrs_clean_daily <- winter_dates %>%
  filter(
    (YY %in% c(2014, 2013, 2024, 2025) & Site == "Newport") |
      (YY %in% c(2020, 2021) & Site == "WHOI")
  )

# pick relevant dates october 1 to june 30
yrs_clean_daily <- yrs_clean_daily %>%
  filter(
    # For YY 2013, 2020, 2024: include Oct 1 - Dec 31
    (YY %in% c(2013, 2020, 2024) & month_day >= "11-01" & month_day <= "12-31") |
      # For YY 2014, 2021, 2024: include Jan 1 - May 30
      (YY %in% c(2014, 2021, 2024) & month_day >= "01-01" & month_day <= "06-10")
  )

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

# calculate weekly means 
week_means <- yrs_clean_daily %>%
  group_by(yr_group,WK) %>%
  summarise(mean_WTMP = mean(mean_WTMP))  

# pull first of the month labels for the graph 
first_days <- yrs_clean_daily %>%
  filter(DD == 1) %>%                # Select rows where day is 1
  distinct(custom_day, month_day) %>% # Remove duplicates
  arrange(custom_day)                 # Ensure proper order

yrs_clean_daily$yr_group <- factor(yrs_clean_daily$yr_group, levels = c("2024-25 (RI)", "2020-21 (MA)", "2013-14 (RI)"))

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

# ? GAANT ------------------------------------------------------------------

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

# ? Light attenuation  --------------------------------------------------------------

### Use initial light measurements to make a light attenuation curve? 

# STATS - Are WHOI and NB different? ----------------------------------------------

library(lme4)
library("lmerTest")

#20-year Mean Temps 
site_means <- buoy_daily %>%
  group_by(Site) %>%
  summarise(Mean_Temp = mean(mean_WTMP, na.rm = TRUE))
site_means

# Add interaction between Site and Year
simple_model <- lmer(mean_WTMP ~ Site + YY + (1 | MM), data = buoy_daily)
summary(simple_model)

# Model for NB site only
nb_data <- buoy_daily %>% filter(Site == "Newport")
nb_model <- lmer(mean_WTMP ~ YY + (1 | MM), data = nb_data)
summary(nb_model)

# Model for WHOI site only  
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

# Light data (not really usable) ------------------------------------------

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


# FIG2. NOT USING >> EnviroYears -------------------------------------------------------------

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






