# handling data retrieval from BioGRID -----------------------------------------

# utility functions -------------------------------------------------------

# bgh_all <- vroom::vroom("dev/BIOGRID-ALL-4.4.207.tab3.zip")
# unique(c(bgh_all$`Organism Name Interactor A`, bgh_all$`Organism Name Interactor B`))
BIOGRID_species <- c(
  "Homo sapiens",
  "Drosophila melanogaster",
  "Caenorhabditis elegans",
  "Saccharomyces cerevisiae (S288c)",
  "Schizosaccharomyces pombe (972h)",
  "Mus musculus",
  "Rattus norvegicus",
  "Canis familiaris",
  "Arabidopsis thaliana (Columbia)",
  "Bos taurus",
  "Gallus gallus",
  "Xenopus laevis",
  "Bacillus subtilis (168)",
  "Danio rerio",
  "Escherichia coli (K12/MG1655)",
  "Human Herpesvirus 1",
  "Human Immunodeficiency Virus 1",
  "Human Herpesvirus 4",
  "Hepatitus C Virus",
  "Chlamydomonas reinhardtii",
  "Zea mays",
  "Candida albicans (SC5314)",
  "Plasmodium falciparum (3D7)",
  "Oryctolagus cuniculus",
  "Human Herpesvirus 8",
  "Human Herpesvirus 2",
  "Sus scrofa",
  "Dictyostelium discoideum (AX4)",
  "Human Herpesvirus 5",
  "Human Immunodeficiency Virus 2",
  "Pan troglodytes",
  "Oryza sativa (Japonica)",
  "Ustilago maydis (521)",
  "Emericella nidulans (FGSC A4)",
  "Simian Immunodeficiency Virus",
  "Human Herpesvirus 6A",
  "Cavia porcellus",
  "Macaca mulatta",
  "Human Herpesvirus 6B",
  "Human Herpesvirus 3",
  "Cricetulus griseus",
  "Ricinus communis",
  "Neurospora crassa (OR74A)",
  "Apis mellifera",
  "Meleagris gallopavo",
  "Vaccinia Virus",
  "Solanum lycopersicum",
  "Tobacco Mosaic Virus",
  "Chlorocebus sabaeus",
  "Escherichia coli (K12/W3110)",
  "Solanum tuberosum",
  "Glycine max",
  "Human papillomavirus (16)",
  "Pediculus humanus",
  "Vitis vinifera",
  "Nicotiana tomentosiformis",
  "Mycobacterium tuberculosis (H37Rv)",
  "Selaginella moellendorffii",
  "Simian Virus 40",
  "Escherichia coli (K12)",
  "Escherichia coli (K12/MC4100/BW2952)",
  "Ovis aries",
  "Human papillomavirus (6b)",
  "Human papillomavirus (10)",
  "Leishmania major (Friedlin)",
  "Streptococcus pneumoniae (ATCCBAA255)",
  "Severe acute respiratory syndrome coronavirus 2",
  "Severe acute respiratory syndrome-related coronavirus",
  "Middle-East Respiratory Syndrome-related Coronavirus",
  "Monodelphis domestica",
  "Human papillomavirus (5)",
  "Human papillomavirus (9)",
  "Human papillomavirus (32)",
  "Human papillomavirus (7)",
  "Sorghum bicolor",
  "Anas platyrhynchos",
  "Anopheles gambiae (PEST)",
  "Equus caballus",
  "Strongylocentrotus purpuratus",
  "Human Herpesvirus 7",
  "Felis Catus"
)



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
#' version <- "4.4.218"
#' species <- "Homo sapiens"
#'
#' urlmaker_BIOGRID()
#'
urlmaker_BIOGRID <- function(type = "PPI", # could be also protein info
                             # species = "Homo sapiens",
                             version = "4.4.218") {
  # https://downloads.thebiogrid.org/File/BioGRID/Release-Archive/BIOGRID-4.4.218/BIOGRID-ALL-4.4.218.tab3.zip
  # https://downloads.thebiogrid.org/Download/BioGRID/Release-Archive/BIOGRID-4.4.218/BIOGRID-ALL-4.4.218.tab3.zip

  ## using the tab3 zipped format, to be processed later
  ## there are no single organism files to retrieve

  if (type == "PPI") {
    url <- sprintf(
      "https://downloads.thebiogrid.org/Download/BioGRID/Release-Archive/BIOGRID-%s/BIOGRID-ALL-%s.tab3.zip",
      version,
      version
    )
  } else {
    url <- NULL
  }

  return(url)
}



# get data from Biogrid --------------------------------------------------

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
#' ppi_biogrid_human <- get_networkdata_BIOGRID(
#'   species = "Homo sapiens",
#'   version = "4.4.218"
#' )
#' dim(ppi_biogrid_human)
#' head(ppi_biogrid_human)
#' pryr::object_size(ppi_biogrid_human)
#'
#' ppi_biogrid_mouse <- get_networkdata_BIOGRID(
#'   species = "Mus musculus",
#'   version = "4.4.218"
#' )
#' dim(ppi_biogrid_mouse)
#' head(ppi_biogrid_mouse)
#' pryr::object_size(ppi_biogrid_mouse)
#'
get_networkdata_BIOGRID <- function(species,
                                    version = "4.4.218",
                                    cache = TRUE,
                                    remap_identifiers = TRUE,
                                    remap_to = c(
                                      "ENSEMBL", "gene_symbol"
                                      # , "ENTREZ"
                                    ),
                                    verbose = TRUE) {
  # checking for the species...
  if (!(species %in% BIOGRID_species)) {
    stop("Species not found")
  }

  rname <- paste0(
    "BIOGRID_",
    "all",
    "_v",
    version
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
    # buildup from BIOGRID url
    biogrid_url <-
      urlmaker_BIOGRID(
        type = "PPI",
        version = version
      )


    network_file <- cache_NetworkHub(
      rname = rname,
      fpath = biogrid_url
    )
  }

  # read in the resource, whether cached or freshly downloaded
  bga_ppis <- vroom::vroom(network_file)

  # here: filter on the organisms
  # two columns are to be checked, the one with the organism of the interactors
  # in AT LEAST one of the two, the species specified must be in it
  ## so it can be that some interactions are "mixed" - could lead to weird results in the graph then?
  ### maybe make it an option to include only interactions among proteins of the same species

  organism_entries <-
    bga_ppis$`Organism Name Interactor A` == species | bga_ppis$`Organism Name Interactor B` == species

  organism_only_entries <-
    bga_ppis$`Organism Name Interactor A` == species & bga_ppis$`Organism Name Interactor B` == species

  bgo_ppis <- bga_ppis[organism_entries, ]

  colnames(bgo_ppis)

  bgo_ppis$entrezid_1 <- bgo_ppis$`Entrez Gene Interactor A`
  bgo_ppis$entrezid_2 <- bgo_ppis$`Entrez Gene Interactor B`
  bgo_ppis$gene_1 <- bgo_ppis$`Official Symbol Interactor A`
  bgo_ppis$gene_2 <- bgo_ppis$`Official Symbol Interactor B`

  # double checking it is the approach biogrid also follows
  # cfr
  ### dim(bgo_ppis)
  ### dim(bga_ppis)
  ### dim(vroom::vroom("dev/BIOGRID-ORGANISM-4.4.218.tab3/BIOGRID-ORGANISM-Homo_sapiens-4.4.218.tab3.txt"))

  # do anything one needs to do on the individual columns

  #### # do a remapping of the used identifiers?
  #### if (remap_identifiers) {
  ####   accessory_info <- get_accessoryinfo_STRINGDB(species = species,
  ####                                                version = version)
  ####
  ####   # reshape and process for the matching
  ####   ## many info are not really soooo useful for our "usual" matching of identifiers
  ####   accessory_info_df <- data.frame(
  ####     protein_id = unique(accessory_info$`#string_protein_id`),
  ####     row.names = unique(accessory_info$`#string_protein_id`)
  ####   )
  ####
  ####   # prefill with NAs
  ####   accessory_info_df$ensembl_id <- NA
  ####   accessory_info_df$gene_symbol <- NA
  ####   # accessory_info_df$entrez_id <- NA
  ####
  ####   df_ensembl <- accessory_info[accessory_info$source == "Ensembl_gene", ]
  ####   df_genesymbol <- accessory_info[accessory_info$source == "Ensembl_EntrezGene", ]
  ####   # df_entrez <- accessory_info[accessory_info$source == "Ensembl_EntrezGene", ]
  ####   ## entrez is not always the same col name!!!
  ####
  ####   accessory_info_df$ensembl_id <-
  ####     df_ensembl$alias[match(accessory_info_df$protein_id, df_ensembl$`#string_protein_id`)]
  ####   accessory_info_df$gene_symbol <-
  ####     df_genesymbol$alias[match(accessory_info_df$protein_id, df_genesymbol$`#string_protein_id`)]
  ####   # accessory_info_df$entrez_id
  ####
  ####   # maybe just do not rename but simply add that kind of info, based on the matching that is given
  ####   # in accessory_info_df
  ####
  ####   sdb_ppis$geneid_1 <- accessory_info_df[sdb_ppis$protein1, ][["ensembl_id"]]
  ####   sdb_ppis$geneid_2 <- accessory_info_df[sdb_ppis$protein2, ][["ensembl_id"]]
  ####   sdb_ppis$gene_1 <- accessory_info_df[sdb_ppis$protein1, ][["gene_symbol"]]
  ####   sdb_ppis$gene_2 <- accessory_info_df[sdb_ppis$protein2, ][["gene_symbol"]]
  ####
  ####   # alternative: point towards a resource such as annotation hub or the orgDb packages
  ####   ## TODO?
  ####   ## Yet: this can also be done AFTERWARDS, in any manner desired
  #### }

  ## thinking out loud: best way to deal with a graph:
  ## have the nodes to be stuck as ensembl ids, and the gene symbols as a label
  ## if not available, go with a fallback solution which would be the ensembl id itself

  # rename columns to make it consistent with others (? will see how)

  # option: how to add the info from ENSEMBL if missing?

  # optional: make it "essential"? like, interactors AND a score (if present)

  return(bgo_ppis)
}
