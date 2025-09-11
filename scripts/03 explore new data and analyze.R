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
  coord_cartesian(xlim = c(1500,2400),
                  ylim = c(-1.5,1.5)) +
  ggrepel::geom_label_repel(data = subset(elevdata, 
                                         TransectPoint_ID == 2100),
                           mapping = aes(label = elevation_m,
                                         fill = as.factor(year)),
                           color = "white",
                           show.legend = FALSE) +
  geom_vline(
    xintercept = 2100,
    linetype = "dotted",
    color = "black"
  )





#generate difference column between 2025 and previous year 
# elevdata |>
#   filter(!is.na(TransectPoint_ID),
#          year >= 2024) |>
#   group_by(TransectPoint_ID) |> 
#   summarise(difference = year[])
#   View()



