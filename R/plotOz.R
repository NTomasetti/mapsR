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
#' @param fillVariable Either NULL or a variable in the dataframe to fill each region in the plot by.
#'     If NULL defaults to `fill = 'grey90`.
#' @param interactive logical - if FALSE (default) a ggplot2 object is returned. If TRUE a plotly::ggplotly object is returned
#' @param label Either NULL or a column in the data to include in the plotly tooltips
#' @param long Either Null or a vector c(longMin, longMax) passed to xlim to view a particular longitude range
#' @param lat Either NULL or a vector c(latMin, latMax) passed to ylim to view a particular lattitude range
#' @param ... Extra arguments passed to geom_polygon. Defaults to `fill = 'grey90` and `colour = 'black'`
#'
#' @return Either a ggplot2 or ggplotly object
#' @export
#'
#' @examples
#'
#' plotOz(fill = 'grey90', colour = 'black')
#' # A plot of Australia
#'
#' plotOz(state = c('ACT', 'NSW', 'VIC', 'TAS', 'QLD'), fillVariable = pop)
#' # A plot of the eastern states filled according to population
#'
#' plotOz(state = 'VIC', interactive = TRUE, label = pop, fill = 'grey90', colour = 'black')
#' An interactive plot of Victoria with population as a tooltip
#'
#' plotOz(fillVariable = pop, interactive = TRUE, label = State)
#' An interactive plot of Australia with colour filled by population and the State names added to the tooltip

plotOz <- function(data = NULL, state = NULL, fillVariable = NULL, interactive = FALSE, label = NULL, long = NULL, lat = NULL, ...){

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

  argnames <- names(sys.call()[-1])
  print(argnames)
  # Quote the fill argument and convert to text
  fillVariable <- quo_text(enquo(fillVariable))
  # Create the plot object
  p <-  ggplot(data, aes(long, lat, group = group)) +
    coord_cartesian(xlim = long, ylim = lat) +
    theme_bw()
  # Add fill and colour depending on their presence in the arguments. Default to'grey90' and 'black' respectively.

  #First Case: A variable is provided to fill by. Colour/color is used if provided, else defaults to black
  if('fillVariable' %in% argnames){
    if('colour' %in% argnames | 'color' %in% argnames){
      p <- p + aes_string(fill = fillVariable) +
        geom_polygon(...)
    } else {
      p <- p + aes_string(fill = fillVariable) +
        geom_polygon(colour = 'black', ...)
    }
    #Second Case: fill is provided in ..., same situation for colour/color
  } else if('fill' %in% argnames){
    print('')
    if('colour' %in% argnames | 'color' %in% argnames){
      p <- p + geom_polygon(...)
    } else {
      print('b')
      p <- p + geom_polygon(...)
    }
    # Final Case: Fill and Fill variable are both excluded
  } else {
    if('colour' %in% argnames | 'color' %in% argnames){
      p <- p + geom_polygon(fill = 'grey90', ...)
    } else {
      p <- p + geom_polygon(fill = 'grey90', colour = 'black', ...)
    }
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

