#devtools::install_github("dwbapst/paleotree", ref="developmentBranch")

#Get taxa
#' Return vector of taxa
#'
#' @return vector of names
#' @export
GetTaxa <- function() {
  #data(taxa_links, package="horseEvo")
  #return(taxa_links$Genus[which(taxa_links$Genus != "All")])
  return(c("hyracotherium", "mesohippus", "pliohippus", "equus"))
}


#Get Tree
#' Get tree
#'
#' @param taxa Vector of taxon names
#' @param rank What taxonomic rank to use
#' @return a phylo object
#' @export
GetTree <- function(taxa = GetTaxa(), rank="genus") {
  if(length(taxa)>1) {
    taxa <- unique(taxa)
    taxa <- paste(taxa, collapse=",")
  }
  data <- paleotree::getSpecificTaxaPBDB(taxa)

  tree <- paleotree::makePBDBtaxonTree(data, rank = rank)

#  tree <- amb(tree)
#plotPhylopicTreePBDB(tree = tree)
  timeTree <- paleotree::dateTaxonTreePBDB(tree)
  return(timeTree)
}

GetTree()
