# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Preparing
# programmer:   Zhe Liu
# Date:         2022-04-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


options(java.parameters = "-Xmx2048m",
        stringsAsFactors = FALSE, 
        encoding = 'UTF-8')


##---- loading the required packages ----
suppressPackageStartupMessages({
  library(zip)
  library(openxlsx)
  library(readxl)
  library(writexl)
  library(RcppRoll)
  library(plyr)
  library(stringi)
  library(feather)
  library(RODBC)
  library(MASS)
  library(car)
  library(data.table)
  library(tidyverse)
  library(lubridate)
})


##---- setup the directories ----
system("cmd.exe", input = "mkdir 01_Background 02_Inputs 03_Outputs 04_Codes 05_Internal_Review 06_Deliveries")
