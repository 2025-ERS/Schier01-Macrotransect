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
