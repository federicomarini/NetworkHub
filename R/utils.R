# initialization of the whole ---------------------------------------------

#' Title
#'
#' @param nh_cachedir
#'
#' @return
#' @export
#'
#' @examples
#' library("BiocFileCache")
#' bfc_nh <- initialize_NetworkHub()
#' bfccache(bfc_nh)
#' length(bfc_nh)
#' bfcinfo(bfc_nh)
initialize_NetworkHub <- function(nh_cachedir = "NetworkHub") {
  cache_dir <- tools::R_user_dir(nh_cachedir, which = "cache")
  bfc_nh <- BiocFileCache::BiocFileCache(cache_dir)

  return(bfc_nh)
}


# internal functions called by the more specific ones ---------------------

#' Title
#'
#' @param rname
#' @param fpath
#' @param nh_cachedir
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' # for example, retrieve something from stringDB
#' 
#' url_sdb <- urlmaker_STRINGDB("PPI", "Homo sapiens", "11.5")
#' url_sdb
#' 
#' cache_NetworkHub(
#'   rname = "STRINGDB_Homo sapiens_v11.5",
#'   fpath = url_sdb
#' )
cache_NetworkHub <- function(rname,
                             fpath,
                             nh_cachedir = "NetworkHub",
                             ...) {
  cache_dir <- tools::R_user_dir(nh_cachedir, which = "cache")
  bfc_nh <- BiocFileCache::BiocFileCache(cache_dir)

  # check if fpath is being tracked
  nh_query <- BiocFileCache::bfcquery(bfc_nh, fpath)
  if (BiocFileCache::bfccount(nh_query) == 0) {
    rpath <- BiocFileCache::bfcadd(bfc_nh, rname, fpath, ...)
  } else {
    rpath <- nh_query$rpath
  }
  return(rpath)
}



#' Title
#'
#' @param rname
#' @param update
#' @param nh_cachedir
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' fetch_NetworkHub(rname = "STRINGDB_Homo sapiens_v11.5")
#'
#' fetch_NetworkHub(rname = "STRINGDB_Homo sapiens_v11.0")
fetch_NetworkHub <- function(rname,
                             update = TRUE,
                             nh_cachedir = "NetworkHub",
                             ...) {
  cache_dir <- tools::R_user_dir(nh_cachedir, which = "cache")
  bfc_nh <- BiocFileCache::BiocFileCache(cache_dir)

  nh_query <- BiocFileCache::bfcquery(bfc_nh, rname, exact = TRUE)

  # is there already a cached version?
  res_nh <- NULL
  if (BiocFileCache::bfccount(nh_query)) {
    rid <- nh_query$rid

    # is the cached version outdated?
    nu <- FALSE
    if (update) {
      nu <- BiocFileCache::bfcneedsupdate(bfc_nh, rid)
    }
    if (!isFALSE(nu)) {
      BiocFileCache::bfcdownload(bfc_nh, rid, ask = FALSE, ...)
    }

    message("Using cached version from ", nh_query$create_time)
    res_nh <- BiocFileCache::bfcrpath(bfc_nh, rname)
  }
  if (is.null(res_nh)) {
    message("No record found in NetworkHub!")
  }
  return(res_nh)
}
