# humannet


# https://staging2.inetbio.org/humannetv3/networks/HumanNet-GSP.tsv
# https://staging2.inetbio.org/humannetv3/networks/HumanNet-XC.tsv
# https://staging2.inetbio.org/humannetv3/networks/HumanNet-FN.tsv
# https://staging2.inetbio.org/humannetv3/networks/HS-PI.tsv

# https://www.inetbio.org/humannetv2/networks/HumanNet-XC.tsv
# https://www.inetbio.org/humannetv2/networks/HumanNet-PI.tsv
# https://www.inetbio.org/humannetv2/networks/HumanNet-FN.tsv

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
#' urlmaker_HumanNet()
urlmaker_HumanNet <- function(type = "PPI",   #could be also protein info
                              species = "Homo sapiens",
                              version = "3") {
  # versions covered: 2 or 3
  # species: only hs
  
  # type: one of PPI, XC, FN, or GSP
  
  baseurl_v2 <- "https://www.inetbio.org/humannetv2/networks/"
  baseurl_v3 <- "https://staging2.inetbio.org/humannetv3/networks/"
  
  if (version == "3") {
    if (type == "PPI") {
      url <- paste0(baseurl_v3, "HS-PI.tsv")
    } else if (type == "XC") {
      url <- paste0(baseurl_v3, "HumanNet-XC.tsv")
    } else if (type == "FN") {
      url <- paste0(baseurl_v3, "HumanNet-FN.tsv")
    } else if (type == "GSP") {
      url <- paste0(baseurl_v3, "HumanNet-GSP.tsv")
    } else {
      stop("Thrown error msg")
    }
  } else if (version == "2") {
    if (type == "PPI") {
      url <- paste0(baseurl_v2, "HumanNet-PI.tsv")
    } else if (type == "XC") {
      url <- paste0(baseurl_v2, "HumanNet-XC.tsv")
    } else if (type == "FN") {
      url <- paste0(baseurl_v2, "HumanNet-FN.tsv")
    } else {
      stop("Thrown error msg v2")
    }
  }
  
  return(url)
}


# get data from HumanNet -------------------------------------------------------

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
#' ppi_humannet_v3 <- get_networkdata_HumanNet("PPI", version = "3")
#' dim(ppi_humannet_v3)
#' head(ppi_humannet_v3)
#' pryr::object_size(ppi_humannet_v3)
#' 
#' get_networkdata_HumanNet("FN", version = "3")
#' get_networkdata_HumanNet("XC", version = "3")
#' get_networkdata_HumanNet("GSP", version = "3")
#' 
#' # get_networkdata_HumanNet("PPI", version = "2")
#' # get_networkdata_HumanNet("XC", version = "2")
#' get_networkdata_HumanNet("FN", version = "2")
get_networkdata_HumanNet <- function(type = "PPI",
                                     version = "3",
                                     cache = TRUE,
                                     remap_identifiers = TRUE,
                                     remap_to = c("ENSEMBL", "gene_symbol"
                                                  #, "ENTREZ"
                                     )) {
  
  rname <- paste0(
    "HumanNet",
    "_v",
    version,
    "_",
    type
  )
  
  # version can br GSP, XC, FN, or the PPI
  
  ...
  
  
  return(hn_network)
}
