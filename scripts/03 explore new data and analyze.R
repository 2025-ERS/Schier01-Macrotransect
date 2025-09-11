# --------------------------HEADER ----------------
# title: "03 explore new data and analyze.R"
# author: "Group 1 - macrotransect"
# date: "2025-Sep-11"
# description: 
# input: "No input files, but the script reads data from an online Google Sheets database"
# output: "No output files, but the script produces some plots"


#-----------------------01 Setup the environment -----------------------------------
# Make sure that you have the renv package installed. If not, install it using install.packages("renv")
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
# Load the renv package 
library(renv)
# restore the library versions from the lock file
renv::restore() 

if (!requireNamespace("ggrepel", quietly = TRUE)) {install.packages("ggrepel")}
# Load the libraries that you will use in this script. Only load the libraries that you will actually use!
library(tidyverse)
library(ggrepel)# load the tidyverse libraries, including readr and ggplot2

#-----------------------02 Import Data Tables from Google Sheet -----------------------------------
# link to "01-Macortransect" = "https://docs.google.com/spreadsheets/d/1UmpZoNIjo5lXdcpGKuBHe7wFX1Z3AQioeHjzrFgYnxo/edit?gid=1550309563#gid=1550309563"

mettables_macrotrans_link <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=972536062&single=true&output=csv"

mettables_macrotrans <- read_csv(mettables_macrotrans_link)

elevdata <- mettables_macrotrans |>
  filter(`Column 1`=="FactElevation") |>
  pull(`gid_for_reading _in_R`) |>
  read_csv()

#-----------------------03 Basic Exploratory Plots -----------------------------------

#first look at only 2025 data
elevdata |>
  filter(year==2025) |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m)) +
  geom_line()

ggsave("figures/line_plot_2025_elevation_m.png", width=1920/300,height=1080/300,dpi=300, units="in")

#first look at all years elevation data
elevdata |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m,
                       color = as.factor(year))) +
  geom_line() +
  labs(title = "Elevations Along Transect Line by Year",
       x = "distance along transect (m)",
       y = "elevation (m)",
        color = "year") + 
  annotate(
    "rect",
    xmin = 150, xmax = 1000,
    ymin = -.5, ymax = 3.2,
    color = "red",
    fill = NA,
    size = 1
  )


#compare 2024 to 2025
elevdata |>
  filter(year>=2024) |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m,
                       color = as.factor(year))) +
  geom_line()

#compare transect point 2100 across all the years with labels
elevdata |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m,
                       color = as.factor(year))) +
  geom_line() +
  coord_cartesian(xlim = c(150,1000),
                  ylim = c(-.5,3.2)) +
  labs(title = "Elevations Along Transect Line by Year",
       x = "distance along transect (m)",
       y = "elevation (m)",
       color = "year")
  
  # ggrepel::geom_label_repel(data = subset(elevdata, 
  #                                         TransectPoint_ID == 2100),
  #                           mapping = aes(label = elevation_m,
  #                                         fill = as.factor(year)),
  #                           color = "white",
  #                           show.legend = FALSE) +
  # geom_vline(
  #   xintercept = 2100,
  #   linetype = "dotted",
  #   color = "black",
  #   alpha = .5
  # )


elevdata |>
  filter(year >= 2024) |> 
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m,
                       color = as.factor(year))) +
  geom_line() +
  coord_cartesian(xlim = c(2000,2400),
                  ylim = c(-1.5,1.5)) +
  ggrepel::geom_label_repel(data = subset(filter(elevdata, year>=2024),
                                          TransectPoint_ID == 2150),
                            mapping = aes(label = elevation_m,
                                          fill = as.factor(year)),
                            color = "white",
                            show.legend = FALSE) +
  geom_vline(xintercept = 2150,
             linetype = "dotted",
             color = "black",
             alpha = .5) +
  ggrepel::geom_label_repel(data = subset(filter(elevdata, year>=2024),
                                          TransectPoint_ID == 2250),
                            mapping = aes(label = elevation_m,
                                          fill = as.factor(year)),
                            color = "white",
                            show.legend = FALSE) +
  geom_vline(xintercept = 2250,
             linetype = "dotted",
             color = "black",
             alpha = .5) +
  labs(title = "Elevation Shift on Lower End of the Transect Between 2024 and 2025",
       x = "distance along transect (m)",
       y = "elevation (m)",
       color = "year")

#ggsave("figures/line_plot_2025_v_2024_elevation.png", width=1920/300,height=1080/300,dpi=300, units="in")






# #generate difference column between 2025 and previous year 
# elevdata |>
#   select(TransectPoint_ID, year, elevation_m) |>
#   drop_na() |>
#   pivot_wider(
#     names_from = year,
#     values_from = elevation_m
#   ) |>
#   drop_na() |>
#   mutate(diff25_24 = `2025` - `2024`) |>
#   ggplot(mapping = aes(x = TransectPoint_ID,
#                        y = diff25_24)) +
#   geom_line() +
#   geom_hline(yintercept = 0,
#              linetype = "dashed",
#              alpha = .5)


#generate difference column between 2025 and previous year 
# make more granular by binning distance_rtk_m
elevdata |>
  select(year, elevation_m, distance_rtk_m) |>
  mutate(distance_bin = cut(distance_rtk_m, 
                            breaks = seq(0, max(distance_rtk_m) + 5, by = 5),
                            right = FALSE,       
                            labels = seq(0, max(distance_rtk_m), by = 5))) |>
  select(-distance_rtk_m) |>
  pivot_wider(
    names_from = year,
    values_from = elevation_m,
    values_fn = mean
  ) |>
  select(distance_bin, `2024`, `2025`) |>
  drop_na() |>
  mutate(diff25_24 = `2025` - `2024`) |>
  ggplot(aes(x = as.numeric(as.character(distance_bin)),
             y = diff25_24,
             fill = diff25_24 > 0)) +
  geom_col(width = 4) +  
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), guide = "none") +
  labs(title = "Difference in Elevation From 2024 to 2025",
       x = "Distance along transect (m)",
       y = "Elevation in 2025 - Elevation in 2024 (m)")

#ggsave("figures/calc_diff_2025_2024_elevation.png", width=1920/300,height=1080/300,dpi=300, units="in")




#now 2025-2018
elevdata |>
  select(year, elevation_m, distance_rtk_m) |>
  mutate(distance_bin = cut(distance_rtk_m, 
                            breaks = seq(0, max(distance_rtk_m) + 5, by = 5),
                            right = FALSE,       
                            labels = seq(0, max(distance_rtk_m), by = 5))) |>
  select(-distance_rtk_m) |>
  pivot_wider(
    names_from = year,
    values_from = elevation_m,
    values_fn = mean
  ) |>
  select(distance_bin, `2018`, `2025`) |>
  drop_na() |>
  mutate(diff25_18 = `2025` - `2018`) |>
  ggplot(aes(x = as.numeric(as.character(distance_bin)),
             y = diff25_18,
             fill = diff25_18 > 0)) +
  geom_col(width = 4) +  
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), guide = "none") +
  labs(title = "Difference in Elevation From 2018 to 2025",
       x = "Distance along transect (m)",
       y = "Elevation in 2025 - Elevation in 2018 (m)")

#ggsave("figures/line_plot_2025_v_2018_elevation.png", width=1920/300,height=1080/300,dpi=300, units="in")



#now 2024-2018
elevdata |>
  select(year, elevation_m, distance_rtk_m) |>
  mutate(distance_rounded = round(distance_rtk_m, 0)) |>
  select(-distance_rtk_m) |>
  pivot_wider(
    names_from = year,
    values_from = elevation_m,
    values_fn = mean
  ) |>
  select(distance_rounded, `2018`, `2024`) |>
  drop_na() |>
  mutate(diff24_18 = `2024` - `2018`) |>
  ggplot(mapping = aes(x = distance_rounded,
                       y = diff24_18)) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             alpha = .5) +
  labs(title = "Calculated Difference in Elevation From 2018 to 2024",
       x = "distance along transect (m)",
       y = "elevation in 2024 - elevation in 2018 (m)")

#-----------------------04 Basic Exploratory Plots -----------------------------------



































































#-----------------------05 Basic Exploratory Plots -----------------------------------
# calculate the mean of the elevation data in 2025
elevdata |>
  mean.default(distance_rtk_m, year==2025)

# calculate the mean of the elevation data in 2025
elevdata |>
  mean(distance_rtk_m, year==2025)

# calculate the mean of the elevation data in 2025
elevdata(dplyr) |>
  filter(Year == 2025) %>%
  summarise(mean_elevation = mean(Elevation, na.rm = TRUE))

# Calculate mean elevation for 2025
mean_elev_2025 <- elevdata |>
  filter(year == 2025) |>
  summarise(mean_elevation = mean(elevation_m, na.rm = TRUE))

# Print result
print(mean_elev_2025)

# Calculate mean elevation for 2024
mean_elev_2024 <- elevdata |>
  filter(year == 2024) |>
  summarise(mean_elevation = mean(elevation_m, na.rm = TRUE))

# Calculate mean elevation for 2018
mean_elev_2018 <- elevdata |>
  filter(year == 2018) |>
  summarise(mean_elevation = mean(elevation_m, na.rm = TRUE))




























































































#-----------------------05 More Exploratory Plots -----------------------------------


# show a plot for 2025 data using a red color
ggplot(elev_2025,
       col = red_palette,
       main = "Elevation Map - 2025",
       xlab = "Longitude",
       ylab = "Latitude")

