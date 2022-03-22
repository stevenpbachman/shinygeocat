# get the WCVP data


library(here)
library(dplyr)
library(magrittr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)

## set up working directory
#work_dir = here()

## 2. download World Checklist of Vascular Plants (WCVP) ----
#dest = paste0(work_dir, "/wcvp_v7_dec_2021.zip")
#url = "http://sftp.kew.org/pub/data-repositories/WCVP/wcvp_v7_dec_2021.zip"
#curl::curl_download(url, destfile = dest)

## unzip 
#exdir = work_dir
#unzip(dest, exdir = exdir)

work_dir = getwd()

# change this to the full version when ready - this is a reduced version
wcvp = read.csv(paste0(work_dir,"/R/WCVP_10K.csv"),
                encoding="UTF-8", 
                stringsAsFactors=FALSE,
                sep = ",")

# sort out the table
wcvp_data <- wcvp %>%
  dplyr::filter(rank == "SPECIES",
         taxonomic_status == "Accepted")

#data <- data[1:10000, ]

#library(readr)
# save down to processed data folder
#write_csv(data, here("R/WCVP_10K.csv"))
