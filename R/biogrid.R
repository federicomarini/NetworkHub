
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
urlmaker_BIOGRID <- function(type = "PPI",   #could be also protein info
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


