
# old method until baz sorts it
library(jsonlite)
library(httr)
library(readr)

#load in the TDWG geometry
#TDWG_LEVEL3 <- read_rds(here("R/tdwg_level3.rds"))

# get native geom
native_geom = function(ID){
  
  #ID <- wcvp_data %>%
  #dplyr::filter(taxon_name_authors == long_name) %>%
  #dplyr::select(ipni_id)
  
  native_powo = get_native_range(ID)
  
  # merge with TDWG to get native geom
  nat_dis <- merge(TDWG_LEVEL3, native_powo, by = "LEVEL3_COD")
  
  return(nat_dis)
  
}


lookup_powo_old <- function(ID, distribution=FALSE) {
  lookup_url <- paste("http://plantsoftheworldonline.org/api/1/taxon/urn:lsid:ipni.org:names:", ID, sep="")
  if (distribution) {
    response <- httr::GET(lookup_url, query=list(fields="distribution"))
  } else {
    response <- httr::GET(lookup_url)
  }

  if (! httr::http_error(response)) {
    return(fromJSON(content(response, as="text")))
  }
  return(NULL)
}

# pull back native range codes
get_native_range = function(ID){
  results = tibble::tibble(
    LEVEL3_COD=NA_character_,
    featureId=NA_character_,
    tdwgLevel=NA_integer_,
    establishment=NA_character_,
    LEVEL3_NAM=NA_character_,
    POWO_ID=NA_character_
  )
  
  returned_data <- lookup_powo_old(ID, distribution=TRUE)
  distribution <- returned_data$distributions
  distribution <- distribution %>%
    filter(establishment == "Native")
  
  if (! is.null(distribution)) {
    results = dplyr::mutate(distribution, POWO_ID=ID)
    results = dplyr::rename(results, LEVEL3_NAM=name, LEVEL3_COD=tdwgCode)
    results = dplyr::mutate(results, LEVEL3_NAM=recode(LEVEL3_NAM, "á"="a"))
  }
  
  return(results)
}




###### get distributions - kewr method baz needs to fix

# library(kewr)
# library(dplyr)
# library(tidyr)
# 
# ipni_id <- "119003-2"
# 
# record <- lookup_powo(ipni_id, distribution=TRUE)
# 
# tidied <- tidy(record)
# 
# glimpse(tidied)
# 
# tidied %>%
#   select(fqId, distribution) %>%
#   unnest(cols=distribution) %>%
#   select(-introduced) %>%
#   unnest(cols=natives)
# 
# name <- lookup_ipni("271445-2", "taxon")
# tidy(name)


