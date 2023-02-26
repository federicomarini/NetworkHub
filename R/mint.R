# handling data retrieval from MINT --------------------------------------------

# https://mint.bio.uniroma2.it/index.php/download/

# downloading from the "query"

# "http://www.ebi.ac.uk/Tools/webservices/psicquic/mint/webservices/current/search/query/*"

# parameters: 
# @species
# @version - if at all possible?!
# @cache


get_networkdata_MINT <- function() {
  rname <- paste0(
    "MINT_",
    "all",
    "_v",
    "Feb2023"
  )
  
  if (cache) {
    # tries to fetch from the cache
    message("Trying to fetch from cache...")
    ### Here: forcing update to FALSE
    network_file <- fetch_NetworkHub(rname, update = FALSE)
  }
  
  if (!cache | is.null(network_file)) {
    # retrieves the file for the first time
    message("Retrieving to cache...")
    # buildup from HIPPIE url
    mint_url <- "http://www.ebi.ac.uk/Tools/webservices/psicquic/mint/webservices/current/search/query/*"
    
    network_file <- cache_NetworkHub(rname = rname,
                                     fpath = mint_url)
  }
  
  # read in the resource, whether cached or freshly downloaded
  mint_ppis <- vroom::vroom(network_file, col_names = FALSE)
  
  
  
  # mint_df <- vroom::vroom("mint_all.txt", col_names = FALSE)
  
  # need to specify the column names, as these are not contained in the df itself
  ## see specifications...
  
  ...
  
  
  
  return(mint_df_subset)
}







