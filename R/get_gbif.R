#' Search plant name against GBIF names backbone
#'
#' This function uses the \code{rgbif} package \url{https://docs.ropensci.org/rgbif/index.html}
#' to query a scientific plant name against the GBIF names backbone - see \url{https://www.gbif.org/}.
#' It restricts the result to species rank constrained to kingdom = 'Plantae'.
#' This function uses the \code{rgbif::name_backbone_verbose} function.
#'
#' @param name A scientific plant species name. Better results can be obtained
#'     when author is included e.g. Poa annua L.
#'
#' @return Returns a data frame with initial search term \code{searchName}, GBIF taxon key \code{usageKey}, GBIF scientific
#'     name \code{scientificName}, and a measure of \code{confidence} in the match. When there is no match it returns a value
#'      of "no_match" under the \code{confidence} field.
#'
#' @examples

#' # single name search
#' name_search_gbif("Poa annua L.")
#'
#' # Or, search multiple names using purrr::map_dfr
#' names <- c("Poa annua L.", "Welwitschia mirabilis Hook.f.")
#'
#' if (requireNamespace("purrr", quietly = TRUE)) {
#' names_out <- purrr::map_dfr(names, name_search_gbif)
#' }

#'
#' @keywords GBIF
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom rgbif name_backbone_verbose
#'
#' @export

# add this for testing: "Aragalus casteteri"

name_search_gbif = function (name) {
  
  # set up default results table
  default_tbl = gbif_name_tbl_(name)
  
  # search using verbose to get fuzzy alternatives
  matches = name_backbone_verbose(
    name = name,
    #rank = 'species', # restrict to species?
    kingdom = 'Plantae',
    strict = FALSE
  )
  
  # bind together in case there are missing data
  matches = bind_rows(matches$alternatives, matches$data)
  
  no_match = all(matches$matchType == "NONE")
  all_higher = all(matches$matchType == "HIGHERRANK")
  
  # catch when search term is too vague or non-plant
  if (no_match | all_higher) {
    results = default_tbl
  } else {
    
    # if we want to filter on species rank
    #results = filter(matches, .data$rank == "SPECIES")
    results = matches
    
    results$searchName = name
    
    results = select(results, colnames(default_tbl))
    results = arrange(results, desc(.data$confidence))
  }
  
  results
}

#' Generate the default table for GBIF name search results
#'
#' @importFrom tibble tibble
#'
#' @noRd
gbif_name_tbl_ = function(query) {
  tibble(
    searchName = query,
    usageKey = NA_integer_,
    scientificName = NA_character_,
    rank = NA_character_,
    confidence = NA_integer_,
    family = NA_character_
  )
}

#' Search plant name against GBIF, KNMS and POWO names backbone.
#'
#' Perform a fuzzy search against three names lists to get best
#' match and POWO taxonomic status.
#'
#' @param name A scientific plant species name. Better results can be obtained
#'     when author is included e.g. Poa annua L.
#'
#' @param homosyn_replace If matched name is classified as a homotypic synonym, the
#'  accepted name is returned
#'
#' @return Returns a data frame with ...
#'
#' @examples
#' # single name search
#' name_search("Poa annua L.")
#'
#' # Or, search multiple names using purrr::map_dfr
#' names <- c("Poa annua L.", "Welwitschia mirabilis Hook.f.", "Acacia torrei")
#'
#' if (requireNamespace("purrr", quietly = TRUE)) {
#' names_out <- purrr::map_dfr(names, name_search)
#' }
#'
#' # if your matched name is a homotypic synonym, you can replace it with the WCVP accepted name
#' # Results retain the original search name, and all other fields are replaced with the accepted name
#' name_search("Acacia torrei", homosyn_replace = TRUE)
#'
#' # and same for multiple species
#' if (requireNamespace("purrr", quietly = TRUE)) {
#' names_out <- purrr::map_dfr(names, name_search, homosyn_replace = TRUE)
#' }

#'
#' @keywords GBIF, KNMS, Plants of the World Online
#'
#' @import dplyr
#' @importFrom kewr match_knms lookup_wcvp tidy
#' @importFrom rlang .data
#'
#' @export

name_search = function(name, homosyn_replace = F){
  
  # set up default results table
  default_tbl = name_tbl_(name)
  
  # first search - fuzzy matching to GBIF names backbone
  gbif_result = name_search_gbif(name)
  
  # catch when search term is too vague or non-plant
  if (is.na(gbif_result$usageKey)[1]) {
    return(default_tbl)
  }
  
  # second search - plug search results into KNMS to get match against Kew names lists
  knms_check = match_knms(gbif_result$scientificName)
  knms_check = tidy(knms_check)
  
  # join up the results
  gbif_knms = left_join(gbif_result, knms_check, by=c("scientificName"="submitted"))
  
  # check if there were ANY matches in KNMS - if not return GBIF
  if (!any(gbif_knms$matched)) {
    
    # and filter on maximum confidence from GBIF search
    # ensures there is only one result
    gbif_knms = slice_max(gbif_knms, .data$confidence, n=1)
    
    # return only the GBIF results
    results = rename(gbif_knms,
                     GBIF_key = usageKey,
                     GBIF_name = scientificName,
                     GBIF_rank = rank,
                     GBIF_confidence = confidence,
                     GBIF_family = family,
                     WCVP_matched = matched,
                     WCVP_ipni_id = ipni_id,
                     WCVP_record = matched_record)
    results$WCVP_status = NA_character_
    results$WCVP_name = NA_character_
    
    return(results)
  }
  
  # filter only on those that matched KNMS
  gbif_knms = filter(gbif_knms, .data$matched == "TRUE")
  
  # remove duplicates if KNMS matched more than one
  gbif_knms = distinct(gbif_knms, .data$ipni_id, .keep_all = TRUE)
  
  # and filter on maximum confidence from GBIF search
  # ensures there is only one result
  gbif_knms = slice_max(gbif_knms, .data$confidence, n=1)
  
  # third search - check WCVP status
  wcvp_check = lookup_wcvp(gbif_knms$ipni_id)
  wcvp_check = tidy(wcvp_check)
  
  # get taxonomic status and names
  wcvp_check = select(wcvp_check, .data$id, .data$status, .data$name, .data$authors)
  
  # check if homotypic synonym and if user wants to replace with accepted
  if (homosyn_replace & wcvp_check$status == "homotypic synonym") {
    
    wcvp_check = lookup_wcvp(wcvp_check$id)
    acc = paste(wcvp_check$accepted$name, wcvp_check$accepted$author, sep = " ")
    
    results = name_search(acc)
    results = mutate(results, searchName = name)
    
    return(results)
    
    
  }
  
  # join up the binomial and author strings
  wcvp_check = tidyr::unite(wcvp_check, name, c(name, authors), sep = " ")
  
  # join up results again
  results = left_join(gbif_knms, wcvp_check, by=c("ipni_id"="id"))
  
  # rename the results for clarity
  results = rename(results, GBIF_key = usageKey,
                   GBIF_name = scientificName,
                   GBIF_rank = rank,
                   GBIF_confidence = confidence,
                   GBIF_family = family,
                   WCVP_matched = matched,
                   WCVP_ipni_id = ipni_id,
                   WCVP_record = matched_record,
                   WCVP_status = status,
                   WCVP_name = name)
  
  return(results)
}

#' Generate the default table for name search results
#'
#' @importFrom tibble tibble
#'
#' @noRd
name_tbl_ = function(query) {
  tibble(
    searchName = query,
    GBIF_key = NA_integer_,
    GBIF_name = NA_character_,
    GBIF_rank = NA_character_,
    GBIF_confidence = NA_integer_,
    GBIF_family = NA_character_,
    WCVP_matched = NA,
    WCVP_ipni_id = NA_character_,
    WCVP_record = NA_character_,
    WCVP_status = NA_character_,
    WCVP_name = NA_character_
  )
}

get_gbif_points = function(key, gbif_limit) {
  result_name_map <- c(BasisOfRec="basisOfRecord",
                       latitude="decimalLatitude",
                       longitude="decimalLongitude",
                       EVENT_YEAR="year",
                       #BINOMIAL="scientificName",
                       CATALOG_NO="catalogNumber")
  
  results = tibble(
    basisOfRecord = NA_character_,
    scientificName = NA_character_,
    decimalLatitude = -999,
    decimalLongitude = -999,
    year = -999L,
    catalogNumber = NA_character_,
    SPATIALREF = "WGS84",
    PRESENCE = "1",
    ORIGIN = "1",
    SEASONAL = "1",
    DATA_SENS = "No",
    SOURCE = NA_character_,
    YEAR = NA_character_,
    COMPILER = NA_character_,
    CITATION = NA_character_,
    recordedBy = NA_character_,
    recordNumber = NA_character_,
    issues = NA_character_,
    datasetKey = NA_character_,
    group = "GBIF"
  )
  
  if (key != "" & ! is.na(key)) {
    gbif_results <- occ_data(
      taxonKey = key,
      hasGeospatialIssue = FALSE,
      hasCoordinate = TRUE,
      limit = gbif_limit
    )
    
    results_count <- gbif_results$meta$count
  } else {
    results_count <- 0
  }
  
  if (results_count > 0){
    gbif_points <- gbif_results$data
  } else {
    gbif_points <- results
  }
  
  if (nrow(gbif_points) > 0) {
    
    columns_to_add = setdiff(colnames(results), colnames(gbif_points))
    default_data = as.list(results)
    gbif_points = tibble::add_column(gbif_points, !!! default_data[columns_to_add])
    
    gbif_points$YEAR = format(Sys.Date(), "%Y")
    gbif_points$SOURCE = paste0("https://www.gbif.org/dataset/", gbif_points$datasetKey, sep = "")
    
    # reformat to iucn standard
    gbif_points = mutate(gbif_points,
                         basisOfRecord=recode(basisOfRecord,
                                              "FOSSIL_SPECIMEN"="FossilSpecimen",
                                              "HUMAN_OBSERVATION"="HumanObservation",
                                              "LITERATURE"="",
                                              "LIVING_SPECIMEN"="LivingSpecimen",
                                              "MACHINE_OBSERVATION"="MachineObservation",
                                              "OBSERVATION"="",
                                              "PRESERVED_SPECIMEN"="PreservedSpecimen",
                                              "UNKNOWN"="Unknown"
                         ))
    
    results = select(gbif_points, colnames(results))
  }
  
  results <- rename(results, !!! result_name_map)
  return(results)
}




