

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
  maplist <- list()
  for (age_index in seq_along(age_df$MidMa)){
    maplist[[age_index]] <- gplatesr::land_sea(age_df$MidMa[age_index], base_url=base_url)
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

#' Create an animated gif of a map
#'
#' This can work with or without taxa.
#'
#' The age range to plot can be set by the periods, the taxa, or fixed ages (which by default go from 0 to 540 MY). If taxa are specified, it uses the times those are found. If periods are specified, it uses the start and stop of those periods. If both periods and taxa are specified, it defaults to using the periods.
#'
#' a <- ot(use_cached_maps_only=TRUE, step_size=1, taxa=GetTaxa())
#'
#' @param start_time The time of the first frame of the animation
#' @param stop_time The time of the last frame of the animation before it starts looping back
#' @param periods A vector of period names, capitalized properly (can be left blank)
#' @param taxa A vector of taxon names (can be left blank)
#' @param step_size How many million years to take in a single step
#' @param age_df Data.frame of ages, typically from GetAgeDF()
#' @param specimen_df Cached fossil localities and times
#' @param interval How many seconds per frame
#' @param use_cached_maps_only If TRUE, only uses the maps already in the package, rather than pulling from gplates
#' @param use_phylopics If TRUE, use phylopic images; otherwise, use dots
#' @param point_color If just plotting points, what color
#' @param gif_name Path to gif, including its name
#' @param paleomaps_allages Cache of maps
#' @param single_frame If TRUE, just does a single frame. It picks the best time.
#' @export
AnimatePlot <- function(start_time=NULL, stop_time=NULL, periods=NULL, taxa=NULL, step_size=1, age_df=GetAgeDF(), specimen_df=specimens, interval=0.5, use_cached_maps_only=FALSE, use_phylopics=FALSE, point_color="red", gif_name=NULL, paleomaps_allages = NULL, single_frame=FALSE) {
  plotlist <- list()
  if(!is.null(periods)) {
    if("All" %in% periods) {
      periods <- NULL # since it was probably passed in accidentally
    }
  }
  if(is.null(paleomaps_allages)) {
    paleomaps_allages <-CreateMapListAllTimes(start_age=ifelse(is.null(start_time),0,start_time), stop_age=ifelse(is.null(stop_time),0,stop_time), step_size=step_size)
  }
  paleomap_info <- as.numeric(gsub("Ma", "", gsub("Time = ", "", unlist(lapply(lapply(paleomaps_allages, "[[", "labels"), "[[", "title")))))
  #names(paleomap_info) <- names(paleomaps)
  if(!is.null(taxa)) {
    specimen_df <- specimen_df[specimen_df$searched_taxon %in% taxa,] # subset of the taxa we want
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.paleolng),]
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.paleolat),]
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.max_ma),]
    specimen_df <- specimen_df[!is.na(specimen_df$pbdb_data.min_ma),]
    if(nrow(specimen_df)>0) {
      possible_start_time <- min(specimen_df$pbdb_data.min_ma, na.rm=TRUE)
      possible_stop_time <- max(specimen_df$pbdb_data.max_ma, na.rm=TRUE)
      #if(possible_stop_time - possible_start_time > 40) { #otherwise, not enough intervals
      #  stop_time <- possible_stop_time
      #  start_time <- possible_start_time
      #}
    }
  }
  if(!is.null(periods)) {
    relevant_periods <- age_df[age_df$Period %in% periods,]
    start_time <- min(relevant_periods$MinMa, na.rm=TRUE)
    stop_time <- max(relevant_periods$MaxMa, na.rm=TRUE)
  }
  if(is.null(start_time)) {
    start_time <- 0
  }
  if(is.null(stop_time)) {
    stop_time <- 540
  }
  if(single_frame) {
    start_time <- 0
    stop_time <- 0
    specimen_df_local <- specimen_df
    if(!is.null(periods)) {
      relevant_period <- which(age_df$Period %in% periods)
      start_time <- min(age_df$MidMa[relevant_period], na.rm=TRUE)
      stop_time <- max(age_df$MidMa[relevant_period], na.rm=TRUE)
      if(nrow(specimen_df_local)>0) {
        specimen_df_local <- specimen_df_local[specimen_df_local$pbdb_data.max_ma<=age_df$MaxMa[relevant_period] & specimen_df_local$pbdb_data.min_ma>=age_df$MinMa[relevant_period],]
      }
    }
    if(nrow(specimen_df_local)>0) {

      median_start <- stats::median(specimen_df_local$pbdb_data.min_ma, na.rm=TRUE)
      median_stop <- stats::median(specimen_df_local$pbdb_data.max_ma, na.rm=TRUE)
      start_time <- stats::median(c(0, rep(stats::median(c(median_start, median_stop), na.rm=TRUE), 2)), na.rm=TRUE) # so that if there are no dates, it uses 0, otherwise, it uses the dates
      start_time <- round(start_time,0)
      stop_time <- start_time
      use_cached_maps_only <- FALSE #so we get plot of the right time
    }
  }
  ages<-seq(from=start_time, to=stop_time, by=step_size)
  for (i in seq_along(ages)) {
    period_color <- NULL
    period_color <- age_df[ ages[i]>=age_df$MinMa & ages[i]<age_df$MaxMa,]$Color
    if(!is.null(period_color)) {
      if(length(period_color)>0) {
        point_color <- plotrix::color.id(period_color)
        #point_color <- period_color
    #  print(point_color)
      }
    }
    my_plot <- NULL
    if(!use_cached_maps_only) {
      try(my_plot <- gplatesr::land_sea(ages[i]))
    }
    if(is.null(my_plot)) { #as a backup, go to the cache
      matching_map_index <- which(paleomap_info==ages[i])
      if(length(matching_map_index)>0) {
        my_plot <- paleomaps_allages[[matching_map_index]]
      }
    }
    if(!is.null(my_plot)) {
      if(!is.null(taxa)) {
        for(taxon_index in seq_along(taxa)) {
          img <- NULL
          if(use_phylopics) {
            try(img <- taxonimages[taxa[taxon_index]][[1]])
            if(is.null(img)) {
              img <- rphylopic::image_data("5d646d5a-b2dd-49cd-b450-4132827ef25e",size=128)[[1]]
            }
          }
          taxon_df <- specimen_df[specimen_df$searched_taxon==taxa[taxon_index],]
          taxon_df <- taxon_df[taxon_df$pbdb_data.max_ma>ages[i],]
          taxon_df <- taxon_df[taxon_df$pbdb_data.min_ma<ages[i],]
          if(use_phylopics) {

            for (taxon_to_add in sequence(nrow(taxon_df))) {
            #  my_plot <-  my_plot + rphylopic::add_phylopic(img, 1, taxon_df$pbdb_data.paleolng[taxon_to_add], taxon_df$pbdb_data.paleolat[taxon_to_add] , ysize = 0.2)
                my_plot <-  my_plot + add_phylopic_to_map(img, 1, taxon_df$pbdb_data.paleolng[taxon_to_add], taxon_df$pbdb_data.paleolat[taxon_to_add] , ysize = 0.2)
            }
          } else {
            if(nrow(taxon_df)>0) {
              taxon_df$Color <- point_color
              my_plot <- add_points(my_plot, taxon_df)
              my_plot <- my_plot + ggplot2::theme(legend.position="none")
            }
            if(ages[i]==0) {
              gbif_points <- GetGBIFPoints(taxa[taxon_index])
              if(nrow(gbif_points)>0) {
                gbif_points$Color <- '#B15928'
                my_plot <- add_points(my_plot, gbif_points)
                my_plot <- my_plot + ggplot2::theme(legend.position="none")
              }
            }
          }
        }
      }
      plotlist[[length(plotlist)+1]] <- my_plot
    }
  }
  if(length(plotlist)>0) {
    animation::ani.options(interval = interval, loop=TRUE, ani.height=315, ani.width=650, interval=1, autobrowse = FALSE)

    movie.name <- tempfile(pattern="animation", fileext="gif")
    if(!is.null(gif_name)) {
      movie.name <- gif_name
    }
    animation::saveGIF({
      for (i in seq_along(plotlist)) {
        anim <- plotlist[[i]]
        plot(anim)
      }
      if(length(plotlist)>1) {
        for (i in (length(plotlist)-1):1) {
          anim <- plotlist[[i]]
          plot(anim)
        }
      }
    }, movie.name=movie.name)
  #  return(list(gif=movie.name, plots=plotlist))
#  } else {
  #  return(list(gif=NA, plots=NA))
  }
}
