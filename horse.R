

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

#' Get boundaries and names of geological periods
#'
#' @return a data.frame of Periods, min, max, and mid points
#' @export
GetAgeDF <- function() {
  Period <- c("Early Paleogene","Mid Paleogene","Late Paleogene","Early Neogene","Late Neogene", "Quaternary")

  MinMa <- c(51.68, 37.36, 23.03, 12.8, 2.58, 0)

  MaxMa <- c(66, 51.68, 37.36, 23.03, 12.8, 2.58)

  MidMa <- c(58.84, 44.52, 30.2, 15.87, 7.69, 1.25 )

  Color <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FDBF6F","#FF7F00")

  ## Map plotting
  age_df <- data.frame(Period, MinMa, MaxMa, MidMa, Color, stringsAsFactors=FALSE) #have to create this df because latlong_age pulls from the GetLatLong fn, which calls the paleobiodb API that doesn't use MidMa
  return(age_df)
}

#' Get information on specimens from pbdb
#'
#' @param taxa a vector of taxon names
#' @return a data.frame with information on all these taxa
#' @export
latlong_age <- function(taxa=GetTaxa(), age_df=GetAgeDF()){
  #store pbdb_data.accepted_name, pbdb_data.lng, pbdb_data.lat, taxon, pbdb_data.early_interval, pbdb_data.late_interval, pbdb_data.max_ma, pbdb_data.min_ma
  lat_long_df <- data.frame()

  for (taxon_index in seq_along(taxa)) {
            latlong.result <- GetLatLongAnytime(taxa[taxon_index])
            latlong.result$taxon=taxa[taxon_index]
            lat_long_df<- rbind(lat_long_df, latlong.result)
        }
  #create empty column called Period
  lat_long_df$Period <- NA

  for (period_index in seq_along(age_df$Period)){
          lat_long_df$Period[which(lat_long_df$pbdb_data.min_ma>=age_df$MinMa[period_index] & lat_long_df$pbdb_data.max_ma <= age_df$MaxMa[period_index])] <- age_df$Period[period_index]
        }
  return(lat_long_df)
}

#' Make a list of maps for all periods
#'
#' By default, uses, gws.gplates.org
#'
#' However, you can do gplatesr::launch_docker() if you are on a machine with docker running, then use base_url="http://localhost:8888/" to use an instance running locally
#'
#' @param age_df Output of GetAgeDF()
#' @param base_url What URL to use for gplates
#' @return list of maps, with names for periods
#' @export
CreateMapList <- function(age_df=GetAgeDF(), base_url='http://gws.gplates.org/') {
  #create map list
  for (age_index in seq_along(age_df$MidMa)){
    maplist <- gplatesr::land_sea(age_df$MidMa[age_index], base_url=base_url)
  }
  #name maplist according to period
  names(maplist) <- age_df$Period

  return(maplist)
}

#the for loop alone works
#for (age_index in seq_along(age_df$MidMa)){
#  maplist <- gplatesr::land_sea(age_df$MidMa[age_index], base_url='http://gws.gplates.org/')
#}

#' Make a list of maps for all times
#'
#' By default, uses, gws.gplates.org
#'
#' However, you can do gplatesr::launch_docker() if you are on a machine with docker running, then use base_url="http://localhost:8888/" to use an instance running locally
#'
#' @param start_age How many MYA to start making the map
#' @param stop_age How many MYA to stop making the map
#' @param step_size How many MY between maps
#' @param age_df Output of GetAgeDF()
#' @param base_url What URL to use for gplates
#' @return list of maps, with names for periods
#' @export
CreateMapListAllTimes <- function(start_age=0, stop_age=66, step_size=1, age_df=GetAgeDF(), base_url='http://gws.gplates.org/') {
  #create map list
  ages <- sort(unique(c(seq(from=start_age, to=stop_age, by=step_size), age_df$MinMa, age_df$MaxMa, age_df$MidMa, 0, 1, 2, 3, 4, 5)))
  ages <- ages[ages<=66]
  maplist <- vector("list", length(ages))
  for (i in seq_along(ages)) {
    maplist[[i]] <- gplatesr::land_sea(mya=ages[i], base_url=base_url)
  }

  #name maplist according to age
  names(maplist) <- ages

  return(maplist)
}


#things to do
#get images
#make maps (start ~60mya)
