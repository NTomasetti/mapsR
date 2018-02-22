library(tidyverse)
library(rlang)
#' Plot Australian Map Dataframes
#'
#' Wrapper to plot Australia, with functionality to subset by state, fill by a column and add interactivity through plotly
#'
#' @param data a dataframe containing long, lat and group columns provided from ggplot2::fortify.
#'     Additional columns can be added for filtering and plot aesthetics.
#'     If NULL, a dataframe containing Australia split by States is provided
#' @param state Either NULL or a character vector containing only elements from
#'     "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC" or "WA". Only the selected states
#'     or terrictories will be plotted. If NULL all of Australia is plotted.
#' @param fill Either NULL or a column of the dataframe to fill the plot by.
#'     If NULL then fill defaults to 'grey90'
#' @param interactive logical - if FALSE (default) a ggplot2 object is returned. If TRUE a plotly::ggplotly object is returned
#' @param label Either NULL or a column in the data to include in the plotly tooltips
#' @param long Either Null or a vector c(longMin, longMax) passed to xlim to view a particular longitude range
#' @param lat Either NULL or a vector c(latMin, latMax) passed to ylim to view a particular lattitude range
#'
#' @return Either a ggplot2 or ggplotly object
#' @export
#'
#' @examples
#'
#' plotOz()
#' # A plot of Australia in the default colours
#'
#' plotOz(state = c('ACT', 'NSW', 'VIC', 'TAS', 'QLD'), fill = pop)
#' # A plot of the eastern states filled according to population
#'
#' plotOz(state = 'VIC', interactive = TRUE, label = pop)
#' An interactive plot of Victoria with population as a tooltip
#'
#' plotOz(fill = pop, interactive = TRUE, label = State)
#' An interactive plot of Australia with colour filled by population and the State names added to the tooltip

plotOz <- function(data = NULL, state = NULL, fill = NULL, interactive = FALSE, label = NULL, long = NULL, lat = NULL){

  #If no shapefile is supplied in data, default to the basic map of Australia with state lines
  if(is.null(data)){
    data <- aus_map
  }

  # Check if a state argument is supplied, if null then do not filter the data
  if(!is.null(state)){
    # If a state argument is supplied, but it is not a string, or does not match the valid state codes return an error
    if(!(is.character(state) & all(state %in% c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')))){
      stop('state must be NULL or a character vector that contains only "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC" or "WA"')
    }
    # If a valid state code is supplied filter the data file to display that state
    data <- filter(data, State %in% state)
  }

  # Quote the fill argument and convert to text
  fill <- quo_text(enquo(fill))
  # Create plot objects, either filled by the quoted input (first case, fill != NULL), or by default colours (second case)
  if(fill != "NULL"){
    p <- ggplot(data) +
      aes_string('long', 'lat', group = 'group', fill = fill) +
      geom_polygon() +
      coord_cartesian(xlim = long, ylim = lat) +
      theme_bw()
  } else {
    p <- ggplot(data) +
      aes(long, lat, group = group) +
      geom_polygon(fill = 'grey90', colour = 'black') +
      coord_cartesian(xlim = long, ylim = lat) +
      theme_bw()
  }

  # Optionally return a plotly object
  if(interactive){
    label <- quo_text(enquo(label))
    if(label != 'NULL'){
      p <- p + aes_string(label = label)
    }
    return(plotly::ggplotly(p))
  }
  return(p)
}

