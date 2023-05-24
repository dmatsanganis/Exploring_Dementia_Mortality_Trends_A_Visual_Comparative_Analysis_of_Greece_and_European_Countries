##            Analytics Practicum II              ##
##           Assignment 1 - Project I             ##
##       Dimitris Matsanganis FT f2822212         ##
####################################################

######################## START: Import Libraries ###############################

library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(png)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gridExtra)
library(magrittr)
library(magick)
library(gganimate)
library(viridis)
library(plotly)

######################### END: Import Libraries ################################

######################## START: Data Preprocessing #############################

# The custom dataset (mainly added age columns) retrieved from the 
# instruction's link: 
# https://ec.europa.eu/eurostat/databrowser/view/
# HLTH_CD_ARO__custom_6254082/default/table?lang=en

# Load the dataset.
df_dementia_totals = read.csv("hlth_cd_aro__custom_6254082_linear.csv", 
                     stringsAsFactors=FALSE)


# However, we have to further modify the Eurostat produced dataset in order to 
# remove unimportant - at least for our analysis - columns, in order to enhance 
# clarity of the dataframe, as well as, decrease the file's size to optimize
# performance.

# First, we have to remove the not used columns, DATAFLOW, LAST.UPDATE, freq, 
# unit, icd10, resid, OBS_FLAG.  
df_dementia_totals$DATAFLOW = NULL
df_dementia_totals$LAST.UPDATE = NULL
df_dementia_totals$freq = NULL
df_dementia_totals$unit = NULL
df_dementia_totals$icd10 = NULL
df_dementia_totals$resid = NULL
df_dementia_totals$OBS_FLAG = NULL

# Also remove the 2021 entries, since we do not have
# data for most of the countries.
# Thus, we filter out the 2021 entries from df_dementia.
df_dementia_totals = subset(df_dementia_totals, TIME_PERIOD != 2021)

# Now we need to standardize the data. 
# After researching the Eurostat website, we found out that we can export the 
# following csv, which provides the Death Rates
# (unit here is RATE not NB as before). 
# We select only Dementia and the counties as well as the EU totals.
# Also we choose the counties to have the full name for easier implemention 
# and better visualization on the plots.
# You can find the dataset with our choices in the following link:
# https://ec.europa.eu/eurostat/databrowser/view/HLTH_CD_ASDR2__
# custom_6255253/default/table?lang=en
# * Remove the spaces who separate the link. *

# Therefore, we will stick to this dataset but we will keep the previous one 
# if we need it on our analysis (death totals).

# Load the dataset.
df_dementia = read.csv("hlth_cd_asdr2__custom_6255253_linear.csv", 
                       stringsAsFactors=FALSE)


# Again, we have to further modify the Eurostat produced dataset in order to 
# remove unimportant - at least for our analysis - columns, in order to enhance 
# clarity of the dataframe, as well as, decrease the file's size to optimize
# performance.

# First, we have to remove the not used columns, DATAFLOW, LAST.UPDATE, freq, 
# unit, icd10, resid, OBS_FLAG.  
df_dementia$DATAFLOW = NULL
df_dementia$LAST.UPDATE = NULL
df_dementia$freq = NULL            # Annual to all.
df_dementia$OBS_FLAG = NULL

# All are Dementia since there was the only one selected
df_dementia$icd10 = NULL 

# All units are RATE, thus we can remove it without affecting our analysis
# (in the first dataset is Number since referring to the number
# of deaths due to dementia).
df_dementia$unit = NULL 

# The initial dataset have some different names, thus we need to change them.
df_dementia$geo[df_dementia$geo == 'Czechia'] = 'Czech Rep.'
df_dementia$geo[df_dementia$geo == 'Germany (until 1990 former territory of the FRG)'] = 'Germany'
df_dementia$geo[df_dementia$geo == 'European Union - 28 countries (2013-2020)'] = 'European Union'
df_dementia$geo[df_dementia$geo == 'Türkiye'] = 'Turkey'

######################### END: Data Preprocessing ##############################

############################# START: Plot 1 ####################################

# Dementia Death Rate in Greece (2011-2020).
df_greece_total = subset(df_dementia, 
                         subset = geo =='Greece' & 
                           sex == 'Total' & age == 'Total')

# Read the Greek flag image file.
greek_flag = readPNG("greek_flag.png")

# Plot the ggplot.
ggplot(df_greece_total, aes(x = factor(TIME_PERIOD), y = OBS_VALUE)) +
  theme_classic() +
  labs(x = 'Year', y = 'Death Rate') +
  ggtitle('Standardised Dementia Death Rate in Greece vs EU (2011-2020)') +
  theme(
    axis.title = element_text(size = 16, face = 'bold'),
    plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = 'aliceblue')
  ) +
  geom_col(aes(label = OBS_VALUE), fill = 'dodgerblue3') +
  geom_line(group = 1, size = 1.3, color = 'black', linetype = 'solid') +
  geom_text(aes(label = round(OBS_VALUE, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, color = 'black', size = 4) +
  scale_fill_manual(values = c('dodgerblue3')) +
  scale_y_continuous(breaks = seq(0, 19, 2), 
                     expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  coord_cartesian(clip = 'off') +
  annotation_raster(greek_flag, xmin = 8.25, xmax = 8.55, ymin = 17.2, ymax = 17.8)

############################## END: Plot 1 #####################################

############################# START: Plot 2 ####################################

# Standardised Dementia Death Rate in Greece vs EU (2011-2020) Plot.

# Dementia Death Rate in Greece (2011-2020).
df_greece_total = subset(df_dementia, 
                         subset = geo =='Greece' & 
                           sex == 'Total' & age == 'Total')

# We need to compute the EU average for the 2019 and 2020 since it is not 
# computed, we will take into the computation only the available data 
# (UK is not available for these years, as well as Turkey for 2020 - which 
# are not EU for now but UK's data were counted as being in EU).
# We decide to exclude Turkey from the computation, but took into it the UK, 
# since the Eurostat referring to 28 counties.

# Make a dataframe to compute the mean of the EU countries.
eu_countries = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                 "Czech Rep.", "Denmark", "Estonia", "Finland", "France",
                 "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                 "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                 "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                 "Spain", "Sweden", "United Kindom")


# Get the totals.
df_dementia_eu_2019 = subset(df_dementia, subset = sex == "Total" 
                             & age == "Total" & TIME_PERIOD == 2019)

# Calculate the mean value for the EU countries in 2019.
mean_eu_2019 = mean(df_dementia_eu_2019$OBS_VALUE, na.rm = TRUE)

# Assign the mean value to the rows where geo is 
# "European Union" in the df_dementia dataframe.
df_dementia[df_dementia$geo == "European Union" & 
              df_dementia$TIME_PERIOD == 2019, "OBS_VALUE"] = mean_eu_2019

# Get the totals.
df_dementia_eu_2020 = subset(df_dementia, subset = sex == "Total" 
                             & age == "Total" & TIME_PERIOD == 2020)

# Calculate the mean value for the EU countries in 2020.
mean_eu_2020 = mean(df_dementia_eu_2020$OBS_VALUE, na.rm = TRUE)

# Assign the mean value to the rows where geo is 
# "European Union" in the df_dementia dataframe.
df_dementia[df_dementia$geo == "European Union" & 
              df_dementia$TIME_PERIOD == 2020, "OBS_VALUE"] = mean_eu_2020

# Dementia Death Rate in EU (2011-2020).
df_eu_total = subset(df_dementia, 
                     subset = geo =='European Union' & 
                     sex == 'Total' & age == 'Total')

# The reduction to EU average maybe due to the absence of UK which 
# had the highest values last years.

# Filter the dataframe for Greece and EU.
df_greece_eu_total = subset(df_dementia, subset = (geo == 'Greece' | 
                                                      geo == 'European Union') 
                             & sex == 'Total' & age == 'Total')

# Read the Greek flag & EU image file.
greek_flag = readPNG("greek_flag.png")
eu_flag = readPNG("eu_flag.png")

# Plot the ggplot.
ggplot(df_greece_eu_total, aes(x = factor(TIME_PERIOD),
                               y = OBS_VALUE, fill = geo)) +
  theme_classic() +
  labs(x = 'Year', y = 'Death Rate') +
  ggtitle('Standardised Dementia Death Rate in Greece vs EU (2011-2020)') +
  theme(
    axis.title = element_text(size = 16, face = 'bold'),
    plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
    plot.background = element_rect(fill = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = 'aliceblue'),
    legend.position = 'right',
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  geom_col(position = 'dodge', width = 0.7, color = 'black') +
  geom_text(aes(label = round(OBS_VALUE, 2)), 
            position = position_dodge(width = 0.7), # Adjust the position to be dodged
            vjust = -0.5, # Adjust vertical position to be above the bars
            color = 'black', size = 4) +
  scale_fill_manual(
    values = c('darkblue', 'dodgerblue3'),
    labels = c('EU', 'Greece'),
    guide = guide_legend(override.aes = list(shape = c(22, NA))),
  ) +
  scale_y_continuous(
    breaks = seq(0, 42, 4),
    expand = expansion(mult = c(0.05, 0.05)),
    limits = c(0, 42) 
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  coord_cartesian(clip = 'off') +
  annotation_raster(greek_flag, xmin = 8.3, xmax = 8.5,
                    ymin = 44.7, ymax = 46.25) +
  annotation_raster(eu_flag, xmin = 8.55, xmax = 8.75,
                    ymin = 44.7, ymax = 46.25)

# Remove the not further useful dataframes and variables for clarity purposes.
remove(df_dementia_eu_2019, df_dementia_eu_2020, df_eu_total, df_greece_total,
       df_greece_eu_total, eu_countries, mean_eu_2019, mean_eu_2020)

############################## END: Plot 2 #####################################

############################# START: Plot 3 ####################################

# Dementia Death Rate Change in Greece and Similar EU Countries (2013-2014) Plot

# Now, we will compare the 2013 - 2014 time periods with countries similar to
# Greece, to further explore this abnormal jump regarding the dementia death 
# rates. The similar counties were selected after presented similarities either 
# demographically, topographically, and/or socioeconomically. 
# We have selected, 9 countries, EU avg. and Greece.
similar_to_GR_countries = subset(df_dementia, sex == "Total" & age == "Total" 
                                 & (TIME_PERIOD == 2013 | TIME_PERIOD == 2014)
                                 & (geo == "Greece"     | geo == "Italy" | 
                                    geo == "Portugal"   | geo == "Spain" |
                                    geo == "Austria"    | geo == "Croatia" | 
                                    geo == "Czech Rep." | geo == "Belgium"|
                                    geo == "Cyprus"     | geo == "Hungary"))

# Sort it alphabetically.
similar_to_GR_countries = similar_to_GR_countries[order(similar_to_GR_countries$geo),]

# Extract the EU means for 2013 and 2014, which 
# will be used as vertical lines indicators.
eu_avg_2013 = subset(df_dementia, sex == "Total" & age == "Total" 
                     & (TIME_PERIOD == 2013) & (geo == "European Union"))   

eu_avg_2014 = subset(df_dementia, sex == "Total" & age == "Total" 
                     & (TIME_PERIOD == 2014) & (geo == "European Union")) 

# Identify pairs in the dataframe - the same country's statistics.
similar_to_GR_countries$paired = rep(1:(20/2), each = 2)

# Plot.
ggplot(similar_to_GR_countries, 
       aes(x = OBS_VALUE, 
           y = reorder(geo, log(OBS_VALUE)))) +
  ggtitle("Dementia Death Rate Change in Greece and Similar EU Countries (2013-2014)") +
  geom_line(aes(group = paired), color = "black") +
  geom_point(aes(color = factor(TIME_PERIOD)), size = 5) +
  scale_color_manual(values = c("darkslateblue", "darkolivegreen4")) +
  scale_x_continuous(breaks = seq(0, 41, 4),
                     expand = expansion(mult = c(0.05, 0.05)),
                     limits = c(0, 41))+
  labs(y = "Country", x = "Dementia Death Rate") +
  geom_vline(xintercept = eu_avg_2013$OBS_VALUE, linetype = "dashed", 
             size = 0.5, alpha = 0.8, color = "darkslateblue") +
  geom_vline(xintercept = eu_avg_2014$OBS_VALUE, linetype = "dashed",
             size = 0.5, alpha = 0.8, color = "darkolivegreen4") +
  geom_text(x = eu_avg_2013$OBS_VALUE - 0.7, y = "Austria", label = "EU MEAN 2013",
            angle = 90, size = 3.5, color = "darkslateblue") +
  geom_text(x = eu_avg_2014$OBS_VALUE + 0.6, y = "Austria", label = "EU MEAN 2014", 
            angle = 90, size = 3.5, color = "darkolivegreen4") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom") 

# Remove the not further useful dataframes and variables for clarity purposes.
remove(eu_avg_2013, eu_avg_2014, similar_to_GR_countries)

############################## END: Plot 3 #####################################

############################# START: Plot 4 ####################################

# Percentage Change in Greece vs EU Dementia Death Rate (2013 vs 2014) Plot.

# Create a subset dataset with the 2013 and 2014 changes.
percentage_change_eu = subset(df_dementia,
                              subset = (TIME_PERIOD == 2013 |
                                          TIME_PERIOD == 2014)
                              & age == "Total" & sex == "Total")

# Group the data by country and calculate percentage change.
percentage_change_eu = percentage_change_eu %>%
  group_by(geo) %>%
  mutate(percentage_change = (OBS_VALUE - lag(OBS_VALUE)) / lag(OBS_VALUE) * 100)

# There are some counties with NA values (3), we will remove them and keep only 
# the countries with available data in both countries (32/35).
# Thus we remove the NA values.
percentage_change_eu = na.omit(percentage_change_eu)

# Plot.
ggplot(percentage_change_eu, 
       aes(x = reorder(factor(geo), percentage_change),
           y = percentage_change)) +
  geom_col(data = percentage_change_eu,
           aes(fill = ifelse(geo == "Greece" | geo == "European Union", 
                             "firebrick4", "darkslateblue"))) +
  geom_text(aes(label = paste0(round(percentage_change, 1), "%"), x = factor(geo)), 
            vjust = 0.3, size = 4,
            hjust = ifelse(percentage_change_eu$percentage_change > 0, -0.2 , 1.2), 
            color = ifelse(percentage_change_eu$geo %in% c("Greece", "European Union"),
                           c("firebrick4", "firebrick4"), "black")) +
  scale_fill_identity() +
  coord_flip() +
  labs(x = "Country", y = "Percentage Change", 
       title = "Percentage Change in Greece vs Europe Dementia Death Rate (2013-2014)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
        plot.background = element_rect(fill = "white"))

# Remove the not further useful dataframes and variables for clarity purposes.
remove(percentage_change_eu, greek_flag, eu_flag)

############################## END: Plot 4 #####################################

############################# START: Plot 5 ####################################

# Percentage Change in Greece vs Europe Dementia Death Rate (2012-2020) Plot.

# Create a subset dataset with Greece and EU's changes.
percentage_change_eugr = subset(df_dementia,
                                subset = (geo == "Greece" |
                                          geo == "European Union")
                                & age == "Total" & sex == "Total")

# Group the data by country and calculate percentage change.
percentage_change_eugr = percentage_change_eugr %>%
  group_by(geo) %>%
  mutate(percentage_change = (OBS_VALUE - lag(OBS_VALUE)) / lag(OBS_VALUE) * 100)

# Remove the NA values 
# (2011 for both since there are no prior records to compute the change).
percentage_change_eugr = na.omit(percentage_change_eugr)

# Plot.
ggplot(percentage_change_eugr, 
       aes(x = factor(TIME_PERIOD), y = percentage_change)) +
  theme_classic() +
  xlab("Year") + 
  ylab("Percentage Change") +
  ggtitle("Percentage Change in Greece vs Europe Dementia Death Rate (2012-2020)") +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
        panel.grid.major = element_line(size = 0.5, color = "gray"),
        plot.background = element_rect(fill = "white")) +
  coord_flip() +
  geom_col(aes(label = percentage_change), width = 0.8,
           fill = ifelse(percentage_change_eugr$geo == "Greece",
                         "firebrick4", "dodgerblue4")) +
  geom_text(aes(label = paste0(round(percentage_change, 1), "%")), 
            vjust = 0.3, size = 3.5,
            hjust = ifelse(percentage_change_eugr$percentage_change > 0, -0.2 , 1.2), 
            color = "black") +
  facet_wrap(~geo, ncol = 1)

# Remove the not further useful dataframes and variables for clarity purposes.
remove(percentage_change_eugr)

############################## END: Plot 5 #####################################

############################# START: Plot 6 ####################################

# Dementia Death Rates Through the Years (2011-2020) - Trellis Plot

# Create the dataframe for the Trellis plot and 
# sort the dataframe alphabetically.
df_trellis = subset(df_dementia, subset = age == "Total" & sex == "Total") %>%
  arrange(geo)

# Some countries present NAs through some years. 
# Like Liechtenstein(2012, 2013, 2015),
# Romania (2012, 2014, 2015, 2016),
# Slovenia (2012, 2013, 2014),
# Turkey(2020), United Kingdom (2019, 2020).
df_trellis = na.omit(df_trellis)

# Plot.
ggplot(data = df_trellis, aes(factor(TIME_PERIOD), OBS_VALUE)) +
  geom_line(group = 1, color = ifelse(df_trellis$geo == "Greece", 'dodgerblue3', 
                                      ifelse(df_trellis$geo == "European Union", 
                                             "firebrick4", "black")), size = 1) +
  geom_point(color = ifelse(df_trellis$geo == "Greece", 'dodgerblue3', 
                              ifelse(df_trellis$geo == "European Union", 
                                   "firebrick4", "black")), size = 2) +
  geom_area(fill = ifelse(df_trellis$geo == "Greece", 'dodgerblue3', 
                          ifelse(df_trellis$geo == "European Union", 
                                 "firebrick4", "black")), alpha = 0.4) +                              
  facet_wrap(~ geo, ncol = 5) +
  labs(x = "Year",
       y = "Dementia Death Rates",
       title = "Dementia Death Rates Through the Years (2011-2020) - Trellis Plot") +
  theme(axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.text = element_text(size = 10),
        plot.background = element_rect(fill = "white")) +
  scale_x_discrete(breaks = seq(2012, 2020, by = 2), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 0)) +
  theme(legend.position = "none")

# Remove the not further useful dataframes and variables for clarity purposes.
remove(df_trellis)

############################## END: Plot 6 #####################################

############################# START: Plot 7 ####################################

# Average Standardised Dementia Death Rate (2011-2020) Plot.

# Define the European Union member countries.
european_union = c("Albania", "Austria", "Belarus", "Belgium", 
                   "Bosnia and Herz.", "Bulgaria", "Croatia", "Cyprus", 
                   "Czech Rep.", "Denmark", "Estonia", "Finland", "France", 
                   "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                   "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", 
                   "Macedonia", "Malta", "Moldova", "Montenegro", "Netherlands",
                   "Norway", "Poland", "Portugal", "Romania", "Serbia", 
                   "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                   "Turkey", "Ukraine","United Kingdom")

# Get the totals.
df_dementia_map = subset(df_dementia, subset = sex == "Total" & age == "Total")

# Find the average for each country for the 2011 - 2020 periods.
df_dementia_map_avg = aggregate(df_dementia_map$OBS_VALUE, 
                                list(factor(df_dementia_map$geo)), 
                                FUN = mean, na.rm = TRUE)

# Rename the 2 columns.
names(df_dementia_map_avg)[1] = "geo"
names(df_dementia_map_avg)[2] = "OBS_VALUE"
df_dementia_map_avg$geo = factor(df_dementia_map_avg$geo,
                                 levels = (df_dementia_map_avg$geo)
                                 [order(df_dementia_map_avg$OBS_VALUE)])


world_map = ne_countries(scale = 'medium', returnclass = 'sf')

# Crop the world map to only include the European Union member countries.
european_union_map = world_map %>% filter(name %in% european_union)
bbox_eu = st_bbox(c(xmin = -23, ymin = 30, xmax = 50, ymax = 68),
                    crs = st_crs(european_union_map))
european_union_map = st_crop(european_union_map, bbox_eu)

# Prepare the data for the map.
european_union_map = left_join(european_union_map, df_dementia_map_avg,
                               by = c("name" = "geo"))

ggplot(data = european_union_map) +
  geom_sf(mapping = aes(fill = OBS_VALUE, colour = "")) +
  theme_stata() +
  theme(legend.background = element_rect(fill='NA', color="NA"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = 'bold'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(name = "Dementia Death Rate",
                      na.value = 'lightgray', 
                      low = "#fdc70c",
                      high = "#e93e3a") +
  labs(title = "Average Standardised Dementia Death Rate (2011-2020)") + 
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(angle = 0, vjust = 0.5, 
                                   hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   hjust = 0.5, size = 10),
        title = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white'),
        plot.title.position = "plot") +
  scale_colour_manual(values = NA) +
  guides(colour = guide_legend("Not availiable data", 
                               override.aes = list(fill = "lightgray")))+
  scale_y_continuous(labels = function(x) paste0(x, "\u00B0 N")) +
  scale_x_continuous(limits = c(-23, 40),
                     labels = function(x) {
                       ifelse(x >= 0, paste0(x, "\u00B0 E"), 
                              paste0(abs(x), "\u00B0 W"))
                     })

# Remove the not further useful dataframes and variables for clarity purposes.
remove(df_dementia_map, df_dementia_map_avg, bbox_eu,
       european_union, european_union_map, world_map)

############################## END: Plot 7 #####################################

############################# START: Plot 8 ####################################

# Dementia Death Rate Trendlines Greece vs EU by Gender (2011-2020) Plot.

# Create a subset dataset with Greece and EU's values for both sexes
df_trendlines = subset(df_dementia, subset = (geo == "Greece" |
                                              geo == "European Union")
                       & age == "Total"  
                       & (sex == "Males" | sex == "Females"))

# Plot.
ggplot(data = df_trendlines, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line(aes(color = geo), size = 1) +
  geom_point(aes(color = ifelse(OBS_VALUE == max(OBS_VALUE), "Higher", "Other"))) +
  geom_area(aes(fill = geo), alpha = 0.4, position = "identity") +
  scale_color_manual(values = c("Other" = "black", "Higher" = "firebrick4")) +
  scale_fill_manual(values = c("firebrick4", "darkblue")) +
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Dementia Death Rate Trendlines Greece vs EU by Gender (2011-2020)") +
  facet_wrap(~sex, nrow = 2, scales = "free_y") +
  theme_stata() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  scale_x_continuous(
    breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    expand = expansion(mult = 0.1)
  ) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  geom_text(
    aes(
      label = sprintf("%0.2f", OBS_VALUE),
      color = ifelse(OBS_VALUE == max(OBS_VALUE), "Higher", "Other")
    ),
    vjust = -1,
    hjust = -0.1
  ) +
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  scale_color_manual(values = c("Higher" = "firebrick4")) +
  coord_cartesian(ylim = c(0, 45))

# Remove the not further useful dataframes and variables for clarity purposes.
remove(df_trendlines)

############################## END: Plot 8 #####################################

############################# START: Plot 9 ####################################

# Dementia Death Rate Greece vs EU by Age (2011-2020) Plot.

# Create the subset dataset focuses on age categories for Greece and EU.
df_trendlines_age = subset(df_dementia, 
                           subset = (geo == "Greece" | geo == "European Union") 
                           & (age == "65 years or over" |
                              age == "Less than 65 years")
                           & sex == "Total")

df_trendlines_over = subset(df_dementia, subset = (geo == "Greece" |
                                                   geo == "European Union")
                            & age == "65 years or over"
                            & sex == "Total")


df_trendlines_less = subset(df_dementia, subset = (geo == "Greece" |
                                                   geo == "European Union")
                            & age == "Less than 65 years"
                            & sex == "Total")

# Plot.
# Row graph for "65 years or over" category.
plot_over = ggplot(data = df_trendlines_over, 
                   aes(x = TIME_PERIOD, y = OBS_VALUE, fill = geo)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  geom_text(aes(label = round(OBS_VALUE, 1), y = OBS_VALUE + 0.5), 
            position = position_dodge(width = 0.5), size = 3, vjust = -0.5) + 
  scale_fill_manual(values = c("Greece" = "darkblue", 
                               "European Union" = "firebrick4")) +
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Dementia Death Rate Greece vs EU\nAge: 65 years or over (2011-2020)") +
  theme_stata() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  scale_x_continuous(
    breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    expand = expansion(mult = 0.1)
  ) +
  scale_y_continuous(breaks = seq(0, 220, by = 40)) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  coord_cartesian(ylim = c(0, 220))

# Row graph for "Less than 65 years" category.
plot_less = ggplot(data = df_trendlines_less, 
                   aes(x = TIME_PERIOD, y = OBS_VALUE, fill = geo)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  geom_text(aes(label = round(OBS_VALUE, 1), y = OBS_VALUE + 0.5), 
            position = position_dodge(width = 0.5), size = 3, vjust = -0.5) + 
  scale_fill_manual(values = c("Greece" = "darkblue", 
                               "European Union" = "firebrick4")) +
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Dementia Death Rate Greece vs EU\nAge: Less than 65 years (2011-2020)") +
  theme_stata() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"  
  ) +
  scale_x_continuous(
    breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    expand = expansion(mult = 0.1)
  ) +
  scale_y_continuous(breaks = seq(0, 45, by = 15)) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  coord_cartesian(ylim = c(0, 45))

# Arrange the two row graphs.
grid.arrange(plot_over, plot_less, nrow = 2)

# Remove the not further useful dataframes and variables for clarity purposes.
remove(df_trendlines_age, df_trendlines_less, df_trendlines_over,
       plot_less, plot_over)

############################## END: Plot 9 #####################################

############################# START: Plot 10 ###################################

# Dementia Death Rate Greece vs EU by Gender
# Age: 65 years or over (2011-2018).

# Create the subset dataset.
df_gender_over65 = subset(df_dementia,subset = (geo == "Greece" |
                                                geo == "European Union")
                          & age == "65 years or over"
                          & (sex == "Males" | sex == "Females")
                          & TIME_PERIOD < 2019)

# We remove the 2019 and 2020 since the EU ones are not correct.

# Plot.
ggplot(df_gender_over65, aes(x = factor(TIME_PERIOD), y = OBS_VALUE)) +
  geom_point(aes(colour = geo), size = 5, show.legend = FALSE) +
  geom_line(aes(group = geo, colour = geo), size = 1.5) +
  geom_text(aes(label = OBS_VALUE), vjust = -1) + 
  facet_grid(sex ~ age) +
  labs(
    colour = "Country"
  ) +
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Dementia Death Rate Greece vs EU by Gender\nAge: 65 years or over (2011-2018)") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 15),
  ) +
  labs(color = "") +
  scale_color_manual(values = c("Greece" = "darkblue", 
                                "European Union" = "firebrick4")) +
  scale_y_continuous(breaks = seq(0, 230, by = 30)) +
  coord_cartesian(ylim = c(0, 230))

# Remove the not further useful dataframes and variables for clarity purposes.
remove(df_gender_over65)

############################## END: Plot 10 ####################################

############################# START: Plot 11 ###################################

# Comparison of Dementia Death Rates between Greece, EU, and Selected Countries Scatterplot (2011-2020).

# Create a subset dataset with Greece and EU's selected similar
# countries values for both sexes and ages categories.
df_scatterplot = subset(df_dementia, subset = (geo %in% c("Greece", 
                                                          "European Union",
                                                          "Cyprus", "Spain", 
                                                          "Portugal"))
                        & age != "Total"  
                        & sex != "Total")

# Removes NAs (Portugal 2011).
df_scatterplot = na.omit(df_scatterplot)

# Subsets.
df_scatterplot_o65 = subset(df_scatterplot, subset =  age == "65 years or over")  

# Sort the dataset by 'geo'.
df_scatterplot_o65 = df_scatterplot_o65[order(df_scatterplot_o65$geo), ]

# Define colors, shapes, and line types based on conditions.
colors65 = ifelse(df_scatterplot_o65$geo == "Greece", "darkblue",
                ifelse(df_scatterplot_o65$geo == "European Union", "firebrick4",
                       ifelse(df_scatterplot_o65$geo == "Cyprus", "chocolate3",
                              ifelse(df_scatterplot_o65$geo == "Spain", "darkolivegreen4",
                                     ifelse(df_scatterplot_o65$geo == "Portugal", "darkorchid1", NA)))))
shapes65 = ifelse(df_scatterplot_o65$sex == "Males", "dashed", "solid")

# Subsets.
df_scatterplot_u65 = subset(df_scatterplot, subset =  age != "65 years or over")  

# Sort the dataset by 'geo'.
df_scatterplot_u65 = df_scatterplot_u65[order(df_scatterplot_u65$geo), ]

# Define colors, shapes, and line types based on conditions.
colors = ifelse(df_scatterplot_u65$geo == "Greece", "darkblue",
                ifelse(df_scatterplot_u65$geo == "European Union", "firebrick4",
                       ifelse(df_scatterplot_u65$geo == "Cyprus", "chocolate3",
                              ifelse(df_scatterplot_u65$geo == "Spain", "darkolivegreen4",
                                     ifelse(df_scatterplot_u65$geo == "Portugal", "darkorchid1", NA)))))
shapes = ifelse(df_scatterplot_u65$sex == "Males", "dashed", "solid")

# Create scatterplots.
plot_o65 = ggplot(df_scatterplot_o65, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  theme_stata() +
  geom_point(aes(color = colors65, shape = shapes65), size = 4) +
  scale_color_manual(values = c("chocolate3", "darkblue", "darkolivegreen4", "darkorchid1", "firebrick4"), 
                     labels = c("Cyprus", "Greece", "Spain", "Portugal", "European Union"),
                     name = "Country:") +
  scale_shape_manual(values = c("square", "triangle"),
                    labels = c("Males", "Females"),
                    name = "Sex:") +
  scale_x_continuous(
    breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    expand = expansion(mult = 0.1)
  ) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"  
  ) + 
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Comparison of Dementia Death Rates between Greece, EU, and Selected Countries Scatterplot\nAge: 65 years or over (2011-2020)") +
  scale_y_continuous(breaks = seq(0, 300, by = 50)) +
  coord_cartesian(ylim = c(0, 300)) 

# Create scatterplots.
plot_u65 = ggplot(df_scatterplot_u65, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  theme_stata() +
  geom_point(aes(color = colors, shape = shapes), size = 4) +
  scale_color_manual(values = c("chocolate3", "darkblue", "darkolivegreen4", "darkorchid1", "firebrick4"), 
                     labels = c("Cyprus", "Greece", "Spain", "Portugal", "European Union"),
                     name = "Country:") +
  scale_shape_manual(values = c("square", "triangle"),
                     labels = c("Males", "Females"),
                     name = "Sex:") +
  scale_x_continuous(
    breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    expand = expansion(mult = 0.1)
  ) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) + 
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Comparison of Dementia Death Rates between Greece, EU, and Selected Countries Scatterplot\nAge: Less than 65 years (2011-2020)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, 0.8)) 

# Arrange the two row graphs.
grid.arrange(plot_o65, plot_u65, nrow = 2)

# Remove the not further useful dataframes and variables for clarity purposes.
remove(colors, shapes, df_scatterplot, colors65, shapes65, df_scatterplot_o65,
       df_scatterplot_u65, plot_o65, plot_u65)

############################## END: Plot 11 ####################################

############################# START: Plot 12 ###################################

# Dementia Death Rates in Greece by Age and Sex

# Subset the dataset for Greece only.
df_greece = subset(df_dementia, geo == "Greece" & age != "Total" 
                   & sex != "Total")

# Filter out NAs, if any.
df_greece = na.omit(df_greece)

# Define shapes and colors based on age and sex.
shapes = ifelse(df_greece$age == "65 years or over", "square", "triangle")
colors = ifelse(df_greece$sex == "Males", "darkblue", "firebrick4")

# Create scatterplot for Greece.
ggplot(df_greece, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_point(aes(shape = shapes, color = colors), size = 3.5) +
  scale_shape_manual(values = c("square", "triangle"),
                     labels = c("65 years or over", "Less than 65 years"),
                     name = "Age") +
  scale_color_manual(values = c("darkblue", "firebrick4"),
                     labels = c("Males", "Females"),
                     name = "Sex") +
  scale_x_continuous(
    breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
    expand = expansion(mult = 0.1)
  ) +
  theme_stata() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
  ) + 
  xlab("Year") +
  ylab("Death Rate") +
  ggtitle("Dementia Death Rates in Greece by Age and Sex") +
  scale_y_continuous(breaks = seq(0, 92, by = 15)) +
  coord_cartesian(ylim = c(0, 92))

# Remove the not further useful dataframes and variables for clarity purposes.
remove(colors, shapes, df_greece)

############################## END: Plot 12 ####################################

############################## START: GIF ######################################

# Scatterplot GIF for all EU countries for all years.

# Subset the dataset for the GIF.
df_gif = subset(df_dementia, geo != "European Union" & age == "Total" 
                & sex == "Total")

# Remove NAs (13 obs.).
df_gif = na.omit(df_gif)

# Country palette.
country_palette = viridis(length(unique(df_gif$geo)))

# Create a vector with specific colors assigned to each country, close to its flag.
country_colors = c("#ED2939", "#FF0000", "#009D57", "#F5A3C7", "#1269C7", 
                   "#FFD700", "#CE1126", "#D21034", "#008C45", "#E30A17", 
                   "#169B62", "#1269C7", "#FCD116", "#B22234", "#DC143C", 
                   "#1F75FE", "#DC143C", "#003580", "#005CBF", "#FF4E00", 
                   "#002395", "#AE1C28", "#FF0000", "#D30A2B", "#0033A0", 
                   "#00933C", "#00247D", "#FF0000", "#ED2939", "#FF0000", 
                   "#E8112D", "#FF0000", "#FF0000", "#FF0000", "#000000")
                             
# Create a vector with abbreviations assigned to each country.
country_abbreviations = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", 
                          "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IS", "IT", 
                          "LI", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT",
                          "RO", "RS", "SE", "SI", "SK", "TR", "UK")

# Match country names and assign color and abbreviation values for the GIF.
df_gif$color = country_colors[match(df_gif$geo, c("Austria", "Belgium", 
                                                  "Bulgaria", "Switzerland", 
                                                  "Cyprus", "Czech Rep.",
                                                  "Germany", "Denmark", 
                                                  "Estonia", "Greece", 
                                                  "Spain", "Finland", 
                                                  "France", "Croatia", 
                                                  "Hungary", "Ireland", 
                                                  "Iceland", "Italy", 
                                                  "Liechtenstein", "Lithuania",
                                                  "Luxembourg", "Latvia", 
                                                  "Malta", "Netherlands",
                                                  "Norway", "Poland", 
                                                  "Portugal", "Romania", 
                                                  "Serbia", "Sweden", 
                                                  "Slovenia", "Slovakia",
                                                   "Turkey", "United Kingdom"))]

df_gif$abbreviation = country_abbreviations[match(df_gif$geo, 
                                                  c("Austria", "Belgium", 
                                                    "Bulgaria", "Switzerland", 
                                                    "Cyprus", "Czech Rep.", 
                                                    "Germany", "Denmark", 
                                                    "Estonia", "Greece",
                                                    "Spain", "Finland", 
                                                    "France", "Croatia", 
                                                    "Hungary", "Ireland",
                                                    "Iceland", "Italy", 
                                                    "Liechtenstein", "Lithuania",
                                                    "Luxembourg", "Latvia", 
                                                    "Malta", "Netherlands",
                                                    "Norway", "Poland", 
                                                    "Portugal", "Romania", 
                                                    "Serbia", "Sweden", 
                                                    "Slovenia", "Slovakia",
                                                    "Turkey", "United Kingdom"))]

# Create the initial scatter plot, Greece has the GR instead of a data point.
anim_plot = ggplot(df_gif, aes(x = TIME_PERIOD, y = OBS_VALUE, 
                               color = color, label = abbreviation)) +
  geom_point(show.legend = FALSE, alpha = 0.7, size = 3.5, color = df_gif$color) +
  geom_point(data = subset(df_gif, geo == "Greece"), size = 0, color = "#1269C7") +
  geom_text(show.legend = FALSE, vjust = 0.5, hjust = -0.5, color = "black", size = 3, fontface = "bold") +
  geom_text(data = subset(df_gif, geo == "Greece"), size = 4, color = "#1269C7", fontface = "bold") +
  ggtitle("Dementia Death Rate Comparisons through the years") +
  scale_size(range = c(2, 2)) +
  labs(x = "Year", y = "Dementia Death Rate") +
  scale_x_continuous(breaks = unique(df_gif$TIME_PERIOD)) +
  scale_color_manual(values = unique(df_gif$color), guide = "none") +
  theme_stata() +
  ylim(0, 90) 

# Add transition effect and frame title.
gif_plot = anim_plot + transition_time(TIME_PERIOD) +
  labs(title = "Year: {frame_time}")

# Show the animated scatter plot.
animate(gif_plot, width = 800, height = 600, res = 100, fps = 10)

# Export the animated scatter plot as a GIF.
anim_save("scatterplot.gif", animate(gif_plot, width = 800,
                                               height = 600, 
                                               res = 100,
                                               fps = 10))

############################### END: GIF #######################################