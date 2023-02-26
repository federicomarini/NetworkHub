# handling data retrieval from HIPPIE ------------------------------------------

# utility functions -------------------------------------------------------

#' Title
#'
#' @param version 
#'
#' @return
#' @export
#'
#' @examples
#' version <- "current"  # corresponds to 2.3
#' 
#' urlmaker_HIPPIE()
#' 
urlmaker_HIPPIE <- function(version = "current") {
  # http://cbdm-01.zdv.uni-mainz.de/~mschaefer/hippie/HIPPIE-current.mitab.txt
  # http://cbdm-01.zdv.uni-mainz.de/~mschaefer/hippie/HIPPIE-2.1.mitab.txt
  
  ## using the mitab format, to be processed later
  ## this seems to contain all the info that the hippie own format contains
  
    url <- sprintf(
      "http://cbdm-01.zdv.uni-mainz.de/~mschaefer/hippie/HIPPIE-%s.mitab.txt",
      version
    )
  
  return(url)
}



# get data from HIPPIE----- --------------------------------------------------

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
#' ppi_hippie_human <- get_networkdata_HIPPIE(version = "2.3")
#' dim(ppi_hippie_human)
#' head(ppi_hippie_human)
#' pryr::object_size(ppi_hippie_human)
#' 
get_networkdata_HIPPIE <- function(species = "human",
                                   version = "2.3",
                                   cache = TRUE,
                                   remap_identifiers = TRUE,
                                   remap_to = c("ENSEMBL", "gene_symbol"
                                                 #, "ENTREZ"
                                   ),
                                   verbose = TRUE) {
  
  ...
  
  return(hippie_ppis)
}
