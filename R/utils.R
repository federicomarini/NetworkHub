# initialization of the whole ---------------------------------------------

#' Title
#'
#' @param nh_cachedir 
#'
#' @return
#' @export
#'
#' @examples
#' bfc_nh <- initialize_NetworkHub()
#' bfccache(bfc_nh)
#' length(bfc_nh)
#' bfcinfo(bfc_nh)
initialize_NetworkHub <- function(nh_cachedir = "NetworkHub") {
  cache_dir <- tools::R_user_dir(nh_cachedir, which = "cache")
  bfc_nh <- BiocFileCache::BiocFileCache(cache_dir)
  
  return(bfc_nh)
} 
