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

# also here:
#T:\Conservation Assessments and Analysis\Steve\WCVP_special_data\wcvp_names_and_distribution_special_edition_2022

## unzip 
#exdir = work_dir
#unzip(dest, exdir = exdir)

work_dir = getwd()

# load in the template csv file
csv_temp = read.csv(paste0(work_dir,"/acacia_anegadensis.csv"),
                encoding="UTF-8", 
                stringsAsFactors=FALSE,
                sep = ",")

# load in the WCVP simplified table
wcvp_data = read.csv(here("R/wcvp_simp.csv"),
                sep=",",
                encoding="UTF-8")

issues = read.csv(here("R/issues.csv"),
                  encoding="UTF-8", 
                  stringsAsFactors=FALSE,
                  sep = ",")

#empty_row = c("77098516-1", "", "", "", "new species","","")
  
#wcvp_data = rbind(wcvp_data, empty_row)

#wcvp_data = sample_n(wcvp_data, 1000)

#wcvp_data = wcvp_data %>%
#  tidyr::unite("taxon_name_authors", c(taxon_name, authors), sep = " ", remove = FALSE)

# load in the WCVP simplified table
#wcvp_data = read.csv(here("R/wcvp_simp.txt"),
#                     sep=",")


# Extra nonsense
# library(vroom)
# wcvp_data <- vroom(here("R/wcvp_simp.csv"),
#                    delim=",",
#                    show_col_type=FALSE)
# 
# rm(wcvp_data)
# #tail(wcvp_data)
# 
# # change this to the full version when ready - this is a reduced version
# wcvp = read.csv(#paste0(work_dir,"/R/WCVP_10K.csv"),
#   "T:/Conservation Assessments and Analysis/Steve/WCVP_special_data/wcvp_names_and_distribution_special_edition_2022/wcvp_names.txt",
#                 encoding="UTF-8",
#                 stringsAsFactors=FALSE,
#                 sep = "|",
#                 quote = "",
#                 header = TRUE,
#                 fill = TRUE)



# # sort out the table
# wcvp_data <- wcvp %>%
#   dplyr::filter(taxon_rank  == "Species",
#                 taxon_status  == "Accepted") %>%
#   dplyr::select(c("ipni_id", "family", "genus", "species", "taxon_name", "taxon_authors")) %>%
#   tidyr::unite("taxon_name_authors", c(taxon_name, taxon_authors), sep = " ", remove = FALSE)
# 
# 
# 
# #library(readr)
# # save down to processed data folder
#write_csv(wcvp_data, here("R/wcvp_simp.csv"))




