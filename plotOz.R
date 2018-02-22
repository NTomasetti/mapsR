library(tidyverse)
library(rlang)
# Inputs:
#data: Either Null or a Data frame of polygons and group as a result from ggplot2::fortify
#state: Either Null or an Australian state code
#interactive: FALSE: Return ggplot2 object, TRUE: Return ggplotly object
# fill = User supplied column to fill by
# label = User supplied column to add to plotly tooltips

# Output:
# alternatively a ggplot2 or ggplotly object of the mapped shapefile, possibly filtered by state

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

  fill <- quo_text(enquo(fill))
  if(fill != "NULL"){
    p <- ggplot(data) + aes_string('long', 'lat', group = 'group', fill = fill) + geom_polygon() +  theme_bw()
  } else {
    p <- ggplot(data) + aes(long, lat, group = group) + geom_polygon(fill = 'grey90', colour = 'black') + theme_bw()
  }
  
  
  # Show only certain long
  if(!is.null(long)){
    p <- p + coord_cartesian(xlim = (long))
  }
  # Show only certain lat
  if(!is.null(lat)){
    p <- p + coord_cartesian(ylim = (lat))
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
plotOz(data= sa4_map, state = c('NSW', 'VIC', 'TAS', 'QLD'), fill = pop, long = c(145, 156))
plotOz(fill = pop, interactive = TRUE, label = State)
plotOz()
plotOz(state = 'VIC', interactive = TRUE, label = paste(pop, State))



