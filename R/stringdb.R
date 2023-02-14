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





# from graph data to real graphs ------------------------------------------

#' Title
#'
#' @param graph_data 
#' @param output_format 
#' @param min_score_threshold 
#' @param subset_nodes must be a vector of graph nodes
#'
#' @return
#' @export
#'
#' @examples
#' min_score_threshold <- 600
#' output_format <- "igraph"
#' 
#' graph_data <- get_networkdata_STRINGDB(species = "Homo sapiens",
#'                                        version = "11.5")
#' 
#' data(res_de_macrophage, package = "GeneTonic")
#' res_de <- res_macrophage_IFNg_vs_naive
#' top_de_genes <- na.omit(GeneTonic::deseqresult2df(res_de)[1:150, "SYMBOL"])
#' 
#' ## todo: change this one with the matching
#' 
#' library(STRINGdb)
#' string_db <- STRINGdb$new(version="11", species=9606,
#'   score_threshold=200, input_directory="sdb_human")
#' data(diff_exp_example1)
#' head(diff_exp_example1)
#' example1_mapped <- string_db$map(diff_exp_example1, "gene", 
#'                              removeUnmappedRows = TRUE)
#' hits <- example1_mapped$STRING_id[1:200]
#' hits
#' 
#' top_proteins <- hits
#' 
#' network_all <- build_graph_STRINGDB(graph_data = graph_data, 
#'                      output_format = "igraph",
#'                      min_score_threshold = 600)
#'                      
#' network_desubset <- build_graph_STRINGDB(graph_data = graph_data, 
#'                      output_format = "igraph",
#'                      min_score_threshold = 600,
#'                      subset_nodes = top_proteins)
#'                      
build_graph_STRINGDB <- function(graph_data,
                                 output_format = c("igraph", "graphnel", "sif"),
                                 min_score_threshold = NULL,
                                 ## alternatives? native for cytoscape?
                                 subset_nodes = NULL) {
  # graph data comes in from the reading-in functionality
  
  # some check to do on the...
  colnames(graph_data)
  
  # inspect histogram for the scores?
  hist(graph_data$combined_score, breaks = 50)
  score_threshold <- 200
  
  if (!is.null(min_score_threshold)) {
    graph_data_processed <- graph_data[graph_data$combined_score >= min_score_threshold,]
  } else {
    graph_data_processed <- graph_data
  }
  
  dim(graph_data)
  dim(graph_data_processed)
  
  whole_graph <- 
    igraph::graph.data.frame(d = graph_data_processed, directed = FALSE)
  
  if (length(subset_nodes) > 0) {
    my_graph <- igraph::induced.subgraph(whole_graph, 
                                         which(V(whole_graph)$name %in% subset_nodes))
  } else {
    my_graph <- whole_graph
  }
  
  # simplify by avoiding multiple entries?
  ## could make it smaller and easier to handle, without losing too much/at all in info
  my_graph <- igraph::simplify(my_graph)
  
  return(my_graph)
}

build_graph <- function(graph_data,
                        data_source = "STRINGDB",
                        output_format = c("igraph", "graphnel", "sif"),
                        min_score_threshold,
                        ## alternatives? native for cytoscape?
                        subset_nodes) {
  
  if (data_source == "STRINGDB") {
    
    my_graph <- build_graph_STRINGDB(graph_data = graph_data,
                                     output_format = output_format,
                                     min_score_threshold = min_score_threshold,
                                     subset_nodes = subset_nodes)
    
  } else {
    
    # TBD
    
  }
  
  return(my_graph)
}


# Enhancing graphs and interacting with them ------------------------------

# de object goes into the SE
# choose the graph
# annotation to map
# rowdata, choose what to plot upon (logFC, e.g.)
# expression data - choose assay? via assay_name
#
#' graph, se, map rowData ("DE results") and potentially gene-wise expression values
#' 
#' g <- de_graph # from the vignette
#' se <- se_macrophage
#' annotation_df <- anno_df_string
#' 
#' rowdata_to_map <- "macrores_log2FoldChange"
#' assay_name <- "counts"
#' gg <- map_se_onto_graph(g, se_macrophage, anno_df_string, assay_name = "counts")
map_se_onto_graph <- function(g,
                              se,
                              annotation_df,
                              rowdata_to_map = NULL,
                              assay_cols = NULL,
                              assay_name) {

  # 
  common_feats <- intersect(rownames(se), V(g)$name)
  if (!length(common_feats)) {
    stop("no features in common")
  }
  
  # TODO: checks on annotation df provided
  
  # if not specifying the rowdata or the samples, map them all
  if (is.null(rowdata_to_map)) {
    rowdata_to_map <- colnames(rowData(se))
  } else if (!all(rowdata_to_map %in% colnames(rowData(se)))) {
    stop("Invalid rowdata_to_map provided")
  }
  
  if (is.null(assay_cols)) {
    assay_cols <- colnames(se)
  } else if (!all(assay_cols %in% colnames(se))) {
    stop("Invalid assay_cols provided")
  }
  
  # TODO: checks here
  assay_to_use <- assays(se)[[assay_name]]
  
  # TODO: checks on anno object provided
  
  prefix <- ""
  
  node_id_type <- "ensembl_id"
  
  for (anno_col in colnames(annotation_df)) {
    attr_name <- paste0("anno_", anno_col)
    matched_anno_info <- annotation_df[match(common_feats, annotation_df[, node_id_type]), ]
    g <- set_vertex_attr(g, 
                         name = attr_name,
                         value = matched_anno_info[, anno_col])
  }
  
  
  
  # mapping the rowdata columns onto the nodes
  for (rd in rowdata_to_map) {  
    attr_name <- paste0(prefix, rd)
    g <- set_vertex_attr(g, 
                         name = attr_name,
                         value = rowData(se)[common_feats, rd])
  }
  
  
  # mapping the assay data onto nodes
  for (ad in assay_cols) {  
    attr_name <- paste0(prefix, ad)
    g <- set_vertex_attr(g, 
                         name = attr_name,
                         value = assay_to_use[common_feats, ad])
  }
  
  return(g)
}


# g in this case is the "well enriched of info" graph
# example:
# color_nodes_by <- "macrores_log2FoldChange"
# use like
#' # from above
#' iplot_graph(gg, color_nodes_by = "macrores_log2FoldChange") 
iplot_graph <- function(g,
                        color_nodes_by,
                        add_tooltip = TRUE) {
  
  # using the graph object with values mapped to the nodes avoid having to do matching here
  
  mypal <- rev(scales::alpha(
    colorRampPalette(RColorBrewer::brewer.pal(name = "RdBu", 11))(50), 0.4
  ))
  
  # TODO: check it is available - otherwise leave colors alone?
  attr_to_plot <- color_nodes_by
  
  attr_maxlimits <- max(abs(range(vertex_attr(g, attr_to_plot))))
  
  V(g)$color <- GeneTonic::map2color(
    vertex_attr(g, attr_to_plot), 
    mypal, 
    limits = c(-attr_maxlimits, attr_maxlimits)
  )
  
  # TODO: the edges should actually stay monochrome-grey...
  
  # using the info from the annotation "inside the graph"
  gene_ids_backup <- V(g)$name
  gene_names <- vertex_attr(g, "anno_gene_symbol")
  new_names <- gene_names
  new_names[is.na(new_names)] <- gene_ids_backup[is.na(new_names)]
  V(g)$name <- new_names
  
  
  rank_gs <- rank(V(g)$name)
  g <- permute.vertices(g, rank_gs)
  
  if (add_tooltip) {
    # title for tooltips
    V(g)$title <- NA
    
    V(g)$title <- paste0(
      "<h4>", V(g)$name, "</h4><br>",
      attr_to_plot,
      " = ", format(round(vertex_attr(g, attr_to_plot), 2), nsmall = 2)
    )
  }
  
  ig <- visNetwork::visIgraph(g) |> 
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                   degree = 1,
                                                   hover = TRUE),
                           nodesIdSelection = TRUE)
  
  return(ig)
}

