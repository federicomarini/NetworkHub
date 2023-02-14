# utility functions -------------------------------------------------------

#' Title
#'
#' @param version 
#'
#' @return
#' @export
#'
#' @examples
#' species_df <- info_stringdb_species()
info_stringdb_species <- function(version = "11.5") {
  # see from https://string-db.org/cgi/download
  
  ## accessory data
  ## e.g. https://stringdb-static.org/download/species.v11.5.txt
  
  url_species <- sprintf(
    "https://stringdb-static.org/download/species.v%s.txt",
    version
  )
  
  df_species <- read.delim(url(url_species))
  
  # TODO: some fallback plan for when there is no internet connection?
  # have this available and cached? or inserted as a small data frame into the package?
  return(df_species)
}


#' Title
#'
#' @param type 
#' @param species 
#' @param version 
#'
#' @return
#' @export
#'
#' @examples
#' version <- "11.5"
#' species <- "Homo sapiens"
#' 
#' urlmaker_STRINGDB("PPI", "Homo sapiens", "11.5")
#' urlmaker_STRINGDB("PPI", "Mus musculus", "11.5")
#' urlmaker_STRINGDB("protein_info", "Homo sapiens", "11.5")
urlmaker_STRINGDB <- function(type = "PPI",   #could be also protein info
                              species = "Homo sapiens",
                              version = "11.5") {
  # "https://stringdb-static.org/download/protein.links.full.v11.5/9606.protein.links.full.v11.5.txt.gz"
  
  # do here the species matching
  
  info_species <- info_stringdb_species(version = version)
  species_id <- info_species$X.taxon_id[
    match(species, info_species$official_name_NCBI)]
  
  if (type == "PPI") {
    url <- sprintf(
      "https://stringdb-static.org/download/protein.links.full.v%s/%s.protein.links.full.v%s.txt.gz",
      version,
      species_id,
      version
    )
  } else if (type == "protein_info") {
    url <- sprintf(
      "https://stringdb-static.org/download/protein.aliases.v%s/%s.protein.aliases.v%s.txt.gz",
      version,
      species_id,
      version
    )
  } 
  
  return(url)
}

