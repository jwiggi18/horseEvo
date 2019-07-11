#' Cache tree information
#'
#' @param taxa vector of taxa
#' @export
CacheTree <- function(taxa=GetTaxa()) {
  chronogram <- NULL
  try(chronogram <- GetTree(taxa=taxa))
  if(!is.null(chronogram)) {
    usethis::use_data(chronogram, overwrite=TRUE)
  }
}

#' Cache phylotree tree information
#'
#' @param taxa vector of taxa
#' @export
cache_pictree <- function(tree=GetTree()) {
  pictree <- NULL
  try(pictree <- ctree(tree = tree))
  if(!is.null(pictree)) {
    usethis::use_data(pictree, overwrite=TRUE)
  }
}


#' Cache map information
#'
#' @param age_df output of GetAgeDF()
#' @export
CacheMaps <- function(age_df=GetAgeDF()) {
  paleomaps <- NULL
  try(paleomaps <- CreateMapList(age_df))
  if(!is.null(paleomaps)) {
    usethis::use_data(paleomaps,   overwrite=TRUE)
  }
}

#' Create map information all ages
#'
#' It used to store inside the package, but it's too huge
#'
#' @param base_url What URL to use for gplates
#' @return paleomaps_allages
#' @export
CacheMapsAllAges <- function(base_url='http://gws.gplates.org/') {
  paleomaps_allages <- NULL
  try(paleomaps_allages <- CreateMapListAllTimes(base_url=base_url))
#  if(!is.null(paleomaps_allages)) {
#    usethis::use_data(paleomaps_allages,   overwrite=TRUE)
#  }
  return(paleomaps_allages)
}

#' Cache specimen information
#'
#' Localities of fossils from PBDB
#
#' @param taxa vector of taxa
#' @export
CacheSpecimenAges <- function(taxa=GetTaxa()) {
  specimens <- NULL
  try(specimens <- latlong_age(taxa))
  if(!is.null(specimens)) {
    usethis::use_data(specimens,   overwrite=TRUE)
  }
}


#' Cache animated maps
#'
#' Create an array of animated maps, where the rows are taxa and the columns periods. "all" and "none" are possible taxa, and "all" is a possible period
#' @inheritParams AnimatePlot
#' @param MaximumAnimate If TRUE, animate everything. If false, static maps for  taxa
#' @export
CacheAnimatedMaps <- function(start_time=NULL, stop_time=NULL, periods=NULL, taxa=GetTaxa(), step_size=1, age_df=GetAgeDF(), specimen_df=specimens, interval=0.5, use_cached_maps_only=TRUE, use_phylopics=FALSE, point_color="red", gif_name=NULL, single_frame=FALSE, MaximumAnimate=FALSE) {
  paleomaps_allages <-  CreateMapListAllTimes()
  all_taxa <- c("All", "None", taxa)
  all_periods <- c("All", age_df$Period)
  all_periods_liststub <- vector("list",length(all_periods))
  names(all_periods_liststub) <- all_periods
  animatedmaps <- vector("list", length(all_taxa))
  for (i in sequence(length(animatedmaps))) {
    animatedmaps[[i]] <- all_periods_liststub
  }
  names(animatedmaps) <- all_taxa
  #animatedmaps <- array(list(), c(2+length(taxa), 1+nrow(age_df)))

  #first do all taxa, all periods
  animatedmaps[["All"]][["All"]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa, gif_name=gsub(" ", "_", paste0("~/GitHubRepos/horseEvo/img/map_All_All.gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)


  #second do no taxa, all periods
  animatedmaps[["None"]][["All"]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=NULL, gif_name=gsub(" ", "_", paste0("~/GitHubRepos/horseEvo/img/map_None_All.gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)

  # Now for backup copy this everywhere in case they don't generate elsewhere
  for (period_index in sequence(length(age_df$Period))) {
      system(paste0("cp ~/GitHubRepos/horseEvo/img/map_none_All.gif ~/GitHubRepos/horseEvo/img/map_All_", age_df$Period[period_index], ".gif"))
      system(paste0("cp ~/GitHubRepos/horseEvo/img/map_none_All.gif ~/GitHubRepos/horseEvo/img/map_None_", age_df$Period[period_index], ".gif"))
      for (taxon_index in seq_along(taxa)) {
        system(paste0("cp ~/GitHubRepos/horseEvo/img/map_None_All.gif ~/GitHubRepos/horseEvo/img/map_", taxa[taxon_index], "_", age_df$Period[period_index], ".gif"))
        system(paste0("cp ~/GitHubRepos/horseEvo/img/map_None_All.gif ~/GitHubRepos/horseEvo/img/map_", taxa[taxon_index], "_All.gif"))
      }
  }


  # now loop over periods, all taxa
  for (period_index in sequence(length(age_df$Period))) {
    print(paste("Making map for all taxa, ", age_df$Period[period_index]))
    animatedmaps[["All"]][[period_index+1]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa, periods=age_df$Period[period_index], gif_name=gsub(" ", "_", paste0("~/GitHubRepos/horseEvo/img/map_All_", age_df$Period[period_index], ".gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)
  }


  # now loop over periods, all taxa
  for (period_index in sequence(length(age_df$Period))) {
    print(paste("Making map for no taxa, ", age_df$Period[period_index]))
    animatedmaps[["None"]][[period_index+1]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=NULL, periods=age_df$Period[period_index], gif_name=gsub(" ", "_", paste0("~/GitHubRepos/horseEvo/img/map_All_", age_df$Period[period_index], ".gif")), paleomaps_allages=paleomaps_allages, single_frame=single_frame)
  }

  #third do single taxa, all periods

  for (taxon_index in seq_along(taxa)) {
    print(paste("Making map for taxon ",  taxa[taxon_index], " all periods"))
    animatedmaps[[taxon_index+2]][["All"]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa[taxon_index], periods=NULL, gif_name=gsub(" ", "_", paste0("~/GitHubRepos/horseEvo/img/map_",taxa[taxon_index], "_All.gif")), paleomaps_allages=paleomaps_allages, single_frame=!MaximumAnimate)
    # now loop over periods, all taxa
    for (period_index in sequence(length(age_df$Period)-1)) {
      print(paste("Making map for taxon ",  taxa[taxon_index], ", period ", age_df$Period[period_index]))
      animatedmaps[[taxon_index+2]][[period_index+1]] <- AnimatePlot(use_phylopics=use_phylopics, interval=interval, point_color=point_color, step_size=step_size, age_df=age_df, use_cached_maps_only=use_cached_maps_only, taxa=taxa[taxon_index], periods=age_df$Period[period_index], gif_name=gsub(" ", "_", paste0("~/GitHubRepos/horseEvo/img/map_",taxa[taxon_index], "_", age_df$Period[period_index], ".gif")), paleomaps_allages=paleomaps_allages, single_frame=!MaximumAnimate)
    }
  }


  if(!is.null(animatedmaps)) {
    for (t_index in seq_along(all_taxa)) {
      for (p_index in seq_along(all_periods)) {
        #try(magick::image_write(animatedmaps[[all_taxa[t_index]]][[all_periods[p_index]]], gsub(" ", "_", paste0("/Users/bomeara/Documents/MyDocuments/GitClones/HistoryOfEarth/docs/img/map_",all_taxa[t_index], "_", all_periods[p_index], ".gif"))))
      }
    }
    #usethis::use_data(animatedmaps, overwrite=TRUE)
  }
}




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


#' Put points on a map
#' @param taxa vector of taxon names, default is GetTaxa()
#' @param specimen_df data.frame of specimen info
#' @param age_df dataframe with Period, MinMa, MaxMa, MidMa, default is GetAgeDF()
#' @param maps paleomaps list of maps (stored internally)
#' @return a list of period maps with points for fossils from that period
#' @export
PutPointsOnMap <- function(taxa = GetTaxa(), specimen_df=specimens, age_df=GetAgeDF(), maps=paleomaps) {
  specimen_df <- specimen_df[specimen_df$searched_taxon %in% taxa,] # subset of the taxa we want
  points_maplist <- maps
  for (map_index in seq_along(points_maplist)) {
    chosen_period <- names(points_maplist)[map_index]
    local_df <- specimen_df[specimen_df$Period==chosen_period,]
    local_df$Color <- age_df$Color[age_df$Period==chosen_period]
    if (nrow(local_df)>0) {
      points_maplist[[map_index]] <- add_points(map=points_maplist[[map_index]], df=local_df)
    }
    points_maplist[[map_index]] <- points_maplist[[map_index]] + ggplot2::theme(legend.position="none")
  }
  return(points_maplist)
}

#' Get locations for modern species using GBIF
#'
#' Note that to make this work with the add_points function, needs odd output column names
#'
#' @param taxa vector of taxon names, default is GetTaxa()
#' @param the rank to search at for each taxon
#' @param fossils If TRUE, include locations of fossils
#' @return data frame with pbdb_data.paleolng and pbdb_data.paleolat columns reflecting points from gbif
#' @export
GetGBIFPoints <- function(taxa = GetTaxa(), rank="genus", fossils=FALSE) {
  location_points <- data.frame()
  for (taxon_index in seq_along(taxa)) {
    key <- rgbif::name_suggest(q=taxa[taxon_index], rank=rank)$key[1]
    location_points <- plyr::rbind.fill(location_points, rgbif::occ_search(taxonKey=key, hasCoordinate=TRUE)$data)
  }
  if(!fossils) {
    location_points <- location_points[location_points$basisOfRecord != "FOSSIL_SPECIMEN",]
  }
  final_points <- data.frame(pbdb_data.paleolng=location_points$decimalLongitude, pbdb_data.paleolat=location_points$decimalLatitude)
  return(final_points)
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
