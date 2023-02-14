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


# get data from STRINGDB --------------------------------------------------

#' Title
#'
#' @param species 
#' @param version 
#' @param cache 
#' @param remap_identifiers 
#' @param remap_to 
#'
#' @return
#' @export
#'
#' @examples
#' # main usage: -------------------------------------------------------------
#' ppi_human_v11.5 <- get_networkdata_STRINGDB(species = "Homo sapiens",
#'                                             version = "11.5")
#' dim(ppi_human_v11.5)
#' head(ppi_human_v11.5)
#' pryr::object_size(ppi_human_v11.5)
#' 
#' 
#' ppi_human_v11.0 <- get_networkdata_STRINGDB(species = "Homo sapiens",
#'                                             version = "11.0")
#' dim(ppi_human_v11.0)
#' head(ppi_human_v11.0)
#' pryr::object_size(ppi_human_v11.0)
#' 
#' ppi_mouse_v11.5 <- get_networkdata_STRINGDB(species = "Mus musculus",
#'                                             version = "11.5")
#' dim(ppi_mouse_v11.5)
#' head(ppi_mouse_v11.5)
#' pryr::object_size(ppi_mouse_v11.5)
get_networkdata_STRINGDB <- function(species,
                                     version,
                                     cache = TRUE,
                                     remap_identifiers = TRUE,
                                     remap_to = c("ENSEMBL", "gene_symbol"
                                                  #, "ENTREZ"
                                     )) {
  
  # matching for the species...
  species_id <- NULL
  
  info_species <- info_stringdb_species(version = version)
  species_id <- info_species$X.taxon_id[
    match(species, info_species$official_name_NCBI)]
  
  
  # if (is.null(species_id))
  #   stop("No match found!")
  
  # buildup of the resource location for the version and all
  ## elegantly done in another smaller utility function
  
  
  
  rname <- paste0(
    "STRINGDB_",
    species,
    "_v",
    version
  )
  
  if (cache) {
    # tries to fetch from the cache
    message("Trying to fetch from cache...")
    network_file <- fetch_NetworkHub(rname)
  }
  
  if (!cache | is.null(network_file)) {
    # retrieves the file for the first time
    message("Retrieving to cache...")
    # buildup from "base" STRINGDB url
    stringdb_url <- 
      urlmaker_STRINGDB(type = "PPI",
                        species = species,
                        version = version)
    
    
    network_file <- cache_NetworkHub(rname = rname,
                                     fpath = stringdb_url)
  }
  
  # read in the resource, whether cached or freshly downloaded
  sdb_ppis <- vroom::vroom(network_file)
  # sdb_ppis <- read.delim(network_file, sep = " ")
  
  # do anything one needs to do on the individual columns
  
  # do a remapping of the used identifiers?
  if (remap_identifiers) {
    accessory_info <- get_accessoryinfo_STRINGDB(species = species,
                                                 version = version)
    
    # reshape and process for the matching
    ## many info are not really soooo useful for our "usual" matching of identifiers
    
    accessory_info_df <- 
      create_annotation_from_STRINGDBaccessory(accessory_info)

    
    # maybe just do not rename but simply add that kind of info, based on the matching that is given
    # in accessory_info_df
    
    sdb_ppis$geneid_1 <- accessory_info_df[sdb_ppis$protein1, ][["ensembl_id"]]
    sdb_ppis$geneid_2 <- accessory_info_df[sdb_ppis$protein2, ][["ensembl_id"]] 
    sdb_ppis$gene_1 <- accessory_info_df[sdb_ppis$protein1, ][["gene_symbol"]] 
    sdb_ppis$gene_2 <- accessory_info_df[sdb_ppis$protein2, ][["gene_symbol"]] 
    
    # alternative: point towards a resource such as annotation hub or the orgDb packages
    ## TODO?
    ## Yet: this can also be done AFTERWARDS, in any manner desired
  }
  
  
  ## thinking out loud: best way to deal with a graph:
  ## have the nodes to be stuck as ensembl ids, and the gene symbols as a label
  ## if not available, go with a fallback solution which would be the ensembl id itself
  
  # rename columns to make it consistent with others (? will see how)
  
  
  # optional: make it "essential"? like, interactors AND a score if present
  
  return(sdb_ppis)
}




#' Title
#'
#' @param species 
#' @param version 
#' @param cache 
#'
#' @return
#' @export
#'
#' @examples
#' ppi_accinfo_human_v11.5 <- get_accessoryinfo_STRINGDB(species = "Homo sapiens",
#'                                               version = "11.5")
get_accessoryinfo_STRINGDB <- function(species,
                                       version,
                                       cache = TRUE) {
  
  # matching for the species...
  species_id <- NULL
  
  info_species <- info_stringdb_species(version = version)
  species_id <- info_species$X.taxon_id[
    match(species, info_species$official_name_NCBI)]
  
  rname <- paste0(
    "STRINGDB_accessoryInfo_",
    species,
    "_v",
    version
  )
  
  if (cache) {
    # tries to fetch from the cache
    message("Trying to fetch from cache...")
    proteininfo_file <- fetch_NetworkHub(rname)
  }
  
  if (!cache | is.null(proteininfo_file)) {
    # retrieves the file for the first time
    message("Retrieving to cache...")
    # buildup from "base" STRINGDB url
    stringdb_url <- 
      urlmaker_STRINGDB(type = "protein_info",
                        species = species,
                        version = version)
    
    proteininfo_file <- cache_NetworkHub(rname = rname,
                                         fpath = stringdb_url)
  }
  
  # read in the resource, whether cached or freshly downloaded
  sdb_proteininfo <- vroom::vroom(proteininfo_file)
  # sdb_proteininfo <- read.delim(proteininfo_file, sep = "\t")
  
  return(sdb_proteininfo)
}


create_annotation_from_STRINGDBaccessory <- function(accessory_info) {
  accessory_info_df <- data.frame(
    protein_id = unique(accessory_info$`#string_protein_id`),
    row.names = unique(accessory_info$`#string_protein_id`)
  )
  
  # prefill with NAs
  accessory_info_df$ensembl_id <- NA
  accessory_info_df$gene_symbol <- NA
  # accessory_info_df$entrez_id <- NA
  
  df_ensembl <- accessory_info[accessory_info$source == "Ensembl_gene", ]
  df_genesymbol <- accessory_info[accessory_info$source == "Ensembl_EntrezGene", ]
  # df_entrez <- accessory_info[accessory_info$source == "Ensembl_EntrezGene", ]
  ## entrez is not always the same col name!!!
  
  accessory_info_df$ensembl_id <- 
    df_ensembl$alias[match(accessory_info_df$protein_id, df_ensembl$`#string_protein_id`)]
  accessory_info_df$gene_symbol <- 
    df_genesymbol$alias[match(accessory_info_df$protein_id, df_genesymbol$`#string_protein_id`)]
  # accessory_info_df$entrez_id
  
  # maybe just do not rename but simply add that kind of info, based on the matching that is given
  # in accessory_info_df
  
  return(accessory_info_df)
}


