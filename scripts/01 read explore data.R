# --------------------------HEADER ----------------
# title: "01-read and organize data.R"
# author: "Group 1 - macrotransect"
# date: "2025-8-22"
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
library(tidyverse) # load the tidyverse libraries, including readr and ggplot2


#-----------------------02 Read the tables from the database -----------------------
# import MetTables from 01-Macrotransect-Elevation
mettableslink <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=972536062&single=true&output=csv"

mettables <- readr::read_csv(mettableslink)

#import FactElevation
factelevlink <- mettables |>
  dplyr::filter(Data_Table=="FactElevation") |>
  pull(`gid_for_reading _in_R`)

elevdat <- readr::read_csv(factelevlink)

#-----------------------03 Exploratory Plots of Transect Elevation -----------------------
# basic plot of elevation down transect
elevdat |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m)) +
  geom_point() +
  facet_grid(rows=vars(year))

elevdat |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m,
                       color = as.factor(year))) +
  geom_line() +
  coord_cartesian(xlim=c(100,425))

elevdat |>
  filter(year==2024) |>
  ggplot(mapping = aes(x = distance_rtk_m,
                       y = elevation_m)) +
  #geom_point(alpha=.1) +
  geom_line() +
  coord_cartesian(xlim=c(100,425))





