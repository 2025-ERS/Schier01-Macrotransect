# --------------------------HEADER ----------------
# title: "02 calculate distance rtk.R"
# author: "Group 1 - macrotransect"
# date: "2025-Sep-22"
# description: adding distance_rtk_m column
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

#-----------------------02 Import Raw Elevation Data -----------------------------------

rawelevdatalink2025 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=320464475&single=true&output=csv"

edata <- read_csv(rawelevdatalink2025) 

#-----------------------03 Calculate distance_rtk_m -----------------------------------

y170 <- edata |> 
  filter(`transect point id`==170) |>
  pull(Y)
x170 <-edata |> 
  filter(`transect point id`==170) |>
  pull(X)

edata$distance_rtk_m<-sqrt((y170-edata$Y)^2+(edata$X-x170)^2)+170 # calculate actual distance covered with gps

with(edata,plot(distance_rtk_m,elevation,type="l")) # plot actual distance covered with transect to elevation difference

write_csv(edata, "distance_rtk_output_2025.csv")


