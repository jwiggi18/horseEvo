

#Get taxa
#' Return vector of taxa
#'
#' @return vector of names
#' @export
GetTaxa <- function() {
  #data(taxa_links, package="horseEvo")
  #return(taxa_links$Genus[which(taxa_links$Genus != "All")])
  return(c("xenicohippus", "eohippus", "orohippus", "haplohippus", "epihippus", "miohippus","kalobatippus", "anchitherium", "hypohippus", "megahippus", "archaeohippus", "desmatippus", "parahippus", "merychippus", "nannippus", "cormohipparion", "pseudhipparion", "neohipparion", "hipparion", "protohippus", "calippus", "pliohippus", "astrohippus", "dinohippus", "equus"))
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
  plot(timeTree)
}

GetTree()
