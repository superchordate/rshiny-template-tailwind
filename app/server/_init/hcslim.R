# helpful function for performing tasks that are expected to always be necessary.
hc_helper = function(options){

  is_map = !is.null(options$chart$type) && grepl('map', options$chart$type) && options$chart$type != 'heatmap'

  # if there is only one series, don't show series name in tooltip.
  if(length(options$series) == 1 && is.null(options$tooltip)) if(eq(options$chart$type, 'boxplot')){
    options$tooltip$format = '
      <table>
        <tr style="border-bottom: 2px solid #ddd"><th colspan="2">{point.category}</th></tr>
        <tr><td>Max</td><td>&nbsp;{point.high}<td></tr>
        <tr><td>95th-%ile</td><td>&nbsp;{point.q3}<td></tr>
        <tr><td>Median</td><td>&nbsp;{point.median}<td></tr>
        <tr><td>5th-%ile</td><td>&nbsp;{point.q1}<td></tr>
        <tr><td>Min</td><td>&nbsp;{point.low}<td></tr>
      </table>
    '
    options$tooltip$useHTML = TRUE
  } else if(is_map){
    options$tooltip$format = '{point.name}: <strong>{point.value}</strong>'
  } else if(eq(options$chart$type, 'sankey')){
    options$tooltip$headerFormat = ''
    options$tooltip$pointFormatter = hc_markjs("function(point){ return this.from + ' -> ' + this.to + ': <strong>' + this.weight + '</strong>'; }")
    # return point.from + " -> " + point.to + ": <strong>" + point.value + "</strong>"; 
  } else if(eq(options$chart$type, 'scatter')){
    options$tooltip$format = '{point.x}, {point.y}'
  } else {
    options$tooltip$format = '{point.label}: <strong>{point.y}</strong>'
  }

  # if series data is a data frame, we need to convert it to a list.
  for(i in seq_along(options$series)) if(!is.vector(options$series[[i]]$data)){
    
    if('data' %ni% names(options$series[[i]])) if(!is.null(options$chart$type) && grepl('map', options$chart$type)){
      next
    } else {
      stop('hcslim: series passed without data.')
    }

    # if the x axis is a character, convert it to a number.
    if('x' %in% colnames(options$series[[i]]$data) && is.character(options$series[[i]]$data$x) || is.factor(options$series[[i]]$data$x)){
      if(is.character(options$series[[i]]$data$x)) stop(glue('An x axis was passed as a character for [{id}]. Please use a factor or a number.'))
      if('xAxis' %ni% names(options)) options$xAxis = list()
      options$xAxis$type = 'categorical'
      # preserve sorting of the incoming data. 
      options$xAxis$categories = unique(as.character(options$series[[i]]$data$x))
      options$series[[i]]$data$x = as.numeric(factor(as.character(options$series[[i]]$data$x), levels = options$xAxis$categories)) - 1 # -1 for 0-indexing.
      if(eq(options$chart$type, 'bar') && (is.null(options$xAxis$reversed) || !options$xAxis$reversed)) options$xAxis$reversed = TRUE # this chart sorts in reverse order.
    }

    # if the data has datalabels column, use it. 
    if('datalabel' %in% colnames(options$series[[i]]$data)){
      if('dataLabels' %ni% names(options$series[[i]])) options$series[[i]]$dataLabels = list()
      if('format' %ni% names(options$series[[i]]$dataLabels)) options$series[[i]]$dataLabels$format = '{point.datalabel}'
    }    

    # special handling for sankey. 
    if(eq(options$chart$type, 'sankey')){
    
        # create nodes and data.
        if('color_from' %ni% names(options$series[[i]]$data)) options$series[[i]]$data %<>% mutate(color_from = NA_character_, color_to = NA_character_)
        if('tooltip_label_from' %ni% names(options$series[[i]]$data)) options$series[[i]]$data %<>% mutate(tooltip_label_from = NA_character_, tooltip_label_to = NA_character_)
        if('dataLabel_label_from' %ni% names(options$series[[i]]$data)) options$series[[i]]$data %<>% mutate(dataLabel_label_from = NA_character_, dataLabel_label_to = NA_character_)
        nodes = rbindlist(list(
          options$series[[i]]$data %>% select(id = from, color = color_from, name = tooltip_label_from, dataLabel_label = dataLabel_label_from), 
          options$series[[i]]$data %>% select(id = to, color = color_to, name = tooltip_label_to, dataLabel_label = dataLabel_label_to)
        ))
        nodes %<>% distinct()
        
        nodes = setNames(lapply(split(nodes, 1:nrow(nodes)), function(x) list(id = x$id, color = x$color, name = x$name)), NULL)
        data = setNames(lapply(split(options$series[[i]]$data, 1:nrow(options$series[[i]]$data)), function(x) list(x$from, x$to, x$weight)), NULL)

        options$series[[i]]$data = data
        options$series[[i]]$nodes = nodes
        options$series[[i]]$keys = c('from', 'to', 'weight')
        # if('dataLabels' %ni% names(options$series[[i]])) options$series[[i]]$dataLabels = list()
        # options$series[[i]]$dataLabels$pointFormatter = hc_markjs("function(){ console.log(this); return 'test'; }")

    } else {

      # convert the dataframe to a list.
      options$series[[i]]$data <- hc_dataframe_to_list(options$series[[i]]$data)
      options$series[[i]]$turboThreshold = length(options$series[[i]]$data) + 1 # to avoid error about turbo mode requiring vectors.

    }

  }

  # special handling for maps. 
  if(is_map){

    # add the baseline map if it hasn't been added already.
    if(all(sapply(options$series, function(x) 'data' %in% names(x)))){
      newseries = list(list(
        borderWidth = 1,
        nullColor = 'rgba(200, 200, 200, 0.3)', 
        showInLegend = FALSE, 
        dataLabels = list(enabled = FALSE)
      ))
      for(series in options$series) newseries[[length(newseries) + 1]] <- series
      options$series = newseries
      rm(newseries)
    }
    
  }

  return(options)

}

hc_html = function(
  id, 
  options, 
  class = c('chart', 'mapChart', 'stockChart', 'ganttChart'),  
  loadmapfromurl = NULL,
  printjs = FALSE, 
  pretty = printjs,
  use_helper = TRUE
){
  
  # validate inputs.
  class = match.arg(class)
  hc_checkid(id)

  # if there is no data, return a paragraph with empty data.
  if(all(sapply(options$series, function(x) is.null(x$data) || eq(nrow(x$data), 0)))) return(htmltools::HTML('<p style="font-style: italic;" class="alert no-data">No data.</p>'))
  
  if('chart' %ni% names(options)) options$chart = list()
  if(use_helper) options %<>% hc_helper()

  # initial conversion to JSON.
  json = jsonlite::toJSON(options, auto_unbox = TRUE, pretty = pretty, force = TRUE)
  
  # format markjs code as raw JS.
  json = gsub('"JS!([^!]+)!"', '\\1', json)

  # replace bad values with null.
  json = gsub('"(NA|-Inf|Inf)"', 'null', json)

  # Highcharts needs vectors, change single numbers to vectors.
  json = gsub('categories": ?([^[,} ]+)', 'categories": [\\1]', json)
  json = gsub('data": ?([^[,} ]+)', 'data": [\\1]', json)

  # compile final Highcharts JS call.
  # option to print completed JS to console for troubleshooting or pasting into jsFiddle.
  # add map download if necessary https://www.highcharts.com/docs/maps/map-collection
  if(!is.null(loadmapfromurl)){
    js = glue::glue('
      const topology = await fetch("{loadmapfromurl}").then(response => response.json()); 
      Highcharts.{class}("{id}", {json});
    ')
    html = glue::glue('<script>(async () => {{{js}}})();</script>')
  } else {
    js = glue::glue("Highcharts.{class}('{id}', {json});")
    html = glue::glue('<script>{js}</script>')
  }
  if(printjs) print(js)
  
  return(htmltools::HTML(as.character(html)))

}

#' Mark JS
#' 
#' Marks Javascript code so hchtml knows how to handle it.
#'
#' @param string 
#'
#' @return string with Javascript marked.
#' @export
#'
hc_markjs = function(string){
  return(as.character(glue::glue('JS!{string}!')))
}

#' Add grouped series. 
#' 
#' Seperating data into series is a very common operation. This function takes your grouped data and adds the series'.
#'
#' @param options Highcharts options for the chart. Includes data, chart type, etc.
#' @param data data.frame-like object containing the underlying data. It must already be grouped and summarized. 
#' @param groupcol Name of the column that will be used to split the data into series. These groups will show in the legend. 
#' @param xcol Name of the column to be used as the X value.
#' @param ycol Name of the column to be used as the Y value.
#'
#' @return options list with series' added. 
#' @export
#'
hc_addgroupedseries = function(options, data, groupcol, xcol, ycol, zcol = NULL){

    # validation.

      if('state' %in% names(data)) warning('
        [state] has a special usage in Highcharts. If you plan to use the column [state] you may experience issues.
        Suggest renaming to [state_abbr] or similar. 
        hcslim::addgroupedseries Warning W513.
      ')

      for(icol in c(groupcol, xcol, ycol)) if(!(icol %in% names(data))) stop(glue('
        Column [{icol}] was not found in the data. 
        hcslim::addgroupedseries Error 514.
      '))

    # select columns.
    data$x = data[[xcol]]
    data$y = data[[ycol]]
    if(!is.null(zcol)) data$z = data[[zcol]] 
    data$group = data[[groupcol]]

    # this only works if using factors so we'll convert to factors.
    data$x = factor(data$x, levels = unique(data$x)) # set levels to preserve sorting.
    data$group = factor(data$group, levels = unique(data$group)) # set levels to preserve sorting.
    data %<>% droplevels() # unused levels will create chaos later.

    # we need a complete mapping so the series' line up.
    # fill missing segments.
    data %<>% right_join( # right join to keep sorting from data. 
      expand.grid(
          x = levels(data$x),
          group = levels(data$group)
      ),
      by = c('x', 'group')
    )

    # extract categories for x axis.
    categories = levels(data$x)

    # create each series.
    if('series' %ni% names(options)) options$series = list()
    for(jdt in split(data, data$group)) options$series[[length(options$series) + 1]] <- list(
        name = as.character(jdt$group[1]),
        data = jdt %>% 
            mutate(x = as.numeric(x) - 1) %>% 
            select(-group) %>%
            hc_dataframe_to_list()
    )

    if('xAxis' %ni% names(options)) options$xAxis = list()
    options$xAxis$type = 'categorical'
    options$xAxis$categories = categories

    # enable the legend. 
    if('legend' %ni% names(options)) options$legend = list()
    options$legend$enabled = TRUE

    return(options)

}

hc_checkid = function(id) if(grepl('[ ]', id)) stop(glue::glue('
  hcslim: Invalid id [{id}]. Must be HTML-compatible. See https://stackoverflow.com/a/79022/4089266.
'))

#.hcslimvars = new.env()
#assign('.loadedpaths', c(), envir=.hcslimvars)

#' Update Highcharts Options
#' 
#' Preserves current options while adding new ones. This isn't as exact as manually working the list, but may be convenient.
#'
#' @param options Highcharts options list.
#' @param option first-level (chart, xAxis, etc.) option to be modified.
#' @param ... Named second-level options (width, margin, etc.) to be set.
#'
#' @return List that can be passed to other functions.
#' @export
#'
#' @examples
hc_updateoption = function(options, option, ...){
  
  # add the first-level option if it doesn't exist yet.
  if(!(option %in% names(options))) options[[option]] = list()
  
  # get the new options.
  datalist = list(...)
  
  if(option=='colors'){
    options[[option]] = unlist(datalist)
  } else {
    for(i in names(datalist)) options[[option]][[i]] = datalist[[i]]
  }
  
  return(options)

}

# enabling the tooltip is not intuitive so we have this function to add it. 
hc_enabletooltips = function(options){
  if('plotOptions' %ni% names(options)) options$plotOptions = list()
  if('series' %ni% names(options$plotOptions)) options$plotOptions$series = list()
  options$plotOptions$series$enableMouseTracking = TRUE
  return(options)
}

hc_dataframe_to_list = function(x){
  if(nrow(x)==0) return(list())
  dt = lapply(split(x, 1:nrow(x)), as.list)
  names(dt) = NULL
  return(dt)
}

# Adding easyr functions here so the whole pacakage doesn't need to be installed. 
# https://github.com/superchordate/easyr/
#' NA-Friendly Equality Comparison
#'
#' Vectorized flexible equality comparison which considers NAs as a value. Returns TRUE if both values are NA, and FALSE when only one is NA. 
#' The standard == comparison returns NA in both of these cases and sometimes this is interpreted unexpectedly.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @param x First vector/value for comparison.
#' @param y Second vector/value for comparison. 
#' @param do.nanull.equal Return TRUE if both inputs are NA or NULL (tested via nanull).
#'
#' @return Boolean vector/value of comparisons.
#' @export
#'
#' @examples
#' c(NA,'NA',1,2,'c') == c(NA,NA,1,2,'a') # regular equality check.
#' eq(c(NA,'NA',1,2,'c'),c(NA,NA,1,2,'a')) # check with eq.
eq <- function( x, y, do.nanull.equal = TRUE ) {

  if( is.null(x) && is.null(y) ) return( TRUE )
  # if vectors are length > 1 and different length, return FALSE.  
  
  if(length(x) != 1 && length(y) != 1 && length(y) != length(x)) stop('Not a valid comparison.')
  
  # if vectors differ in length, modify them. 
  # this allows comparison of a single value to a vector. 
  if( length(x) == 1 && length(y) > 1 ) x = rep( x = x, times = length(y) )
  if( length(y) ==1 && length(x) > 1 ) y = rep( x = y, times = length(x) )
  
  if( do.nanull.equal ) if( all( nanull( x, do.test.each = TRUE ) & nanull( y, do.test.each = TRUE ) ) ) return( TRUE )
  
  nanull.x = nanull( x, do.test.each = TRUE )
  nanull.y = nanull( y, do.test.each = TRUE )
  
  out = rep( FALSE, length( nanull.x ) )
  out[ nanull.x & nanull.y ] <- TRUE
  out[ !nanull.x & !nanull.y & x==y ] <- TRUE
  
  return(out)
  
}

#' NA / NULL Check
#' 
#' Facilitates checking for missing values which may cause errors later in code. 
#' NULL values can cause errors on is.na checks, and is.na can cause warnings if it is inside if() and is passed multiple values.
#' This function makes it easier to check for missing values before trying to operate on a variable. 
#' It will NOT check for strings like ""  or "NA". Only NULL and NA values will return TRUE.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @param x Vector to check. In the case of a data frame or vector, it will check the first (non-NULL) value.
#' @param na_strings (Optional) Set the strings you want to consider NA. These will be applied after stringr::str_trim on x.
#' @param do.test.each Return a vector of results to check each element instead of checking the entire object.
#'
#' @return True/false indicating if the argument is NA, NULL, or an empty/NA string/vector. For speect, only the first value is checked.
#' @export
#'
#' @examples
#' nanull( NULL )
#' nanull( NA )
#' nanull( c( NA , NULL ) )
#' nanull( c( 1, 2, 3 ) )
#' nanull( c( NA, 2, 3 ) )
#' nanull( c( 1, 2, NA ) ) # only the first values is checked, so this will come back FALSE.
#' nanull( c( NULL, 2, 3 ) ) # NULL values get skipped in a vector.
#' nanull( data.frame() )
#' nanull( dplyr::group_by( dplyr::select( cars, speed, dist ), speed ) ) # test a tibble.
nanull <- function( x, na_strings = c('NA', '', 'NULL'), do.test.each = FALSE ) !isval(x, na_strings, do.test.each = do.test.each )

#' Is Valid / Is a Value / NA NULL Check
#' 
#' Facilitates checking for missing values which may cause errors later in code. 
#' NULL values can cause errors on is.na checks, and is.na can cause warnings if it is inside if() and is passed multiple values.
#' This function makes it easier to check for missing values before trying to operate on a variable. 
#' It will NOT check for strings like ""  or "NA". Only NULL and NA values will return TRUE.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @param x Object to check. In the case of a data frame or vector, it will check the first (non-NULL) value.
#' @param na_strings (Optional) Set the strings you want to consider NA. These will be applied after stringr::str_trim on x.
#' @param do.test.each Return a vector of results to check each element instead of checking the entire object.
#'
#' @return True/false indicating if the argument is NA, NULL, or an empty/NA string/vector. For speect, only the first value is checked.
#' 
#' @export
#'
#' @examples
#' isval( NULL )
#' isval( NA )
#' isval( c( NA , NULL ) )
#' isval( c( 1, 2, 3 ) )
#' isval( c( NA, 2, 3 ) )
#' isval( c( 1, 2, NA ) ) # only the first values is checked, so this will come back FALSE.
#' isval( c( NULL, 2, 3 ) ) # NULL values get skipped in a vector.
#' isval( data.frame() )
#' isval( dplyr::group_by( dplyr::select( cars, speed, dist ), speed ) ) # test a tibble.
#' isval( "#VALUE!" ) # test an excel error code.
isval <- function( x, na_strings = c('NA', '', 'NULL'), do.test.each = FALSE ){

  # Empty vectors/lists, or data frames with no columns.
  # Length of NULL is 0 so this also checks for null. Length of NA is 1.
  if( length(x) == 0 ) return(FALSE)
  
  # Default functionality - check the object, not the elements.
  if( !do.test.each ){
    
    # If a data frame made it this far, it is valid and can be worked on.
    if( is.data.frame(x) && ! do.test.each ) return( TRUE )
    
    # Get a single value to test, in case a data frame, tibble, or vector is passed.
    # Otherwise these checks can be very slow. In the future, I'd like to add an option to check all but for now we'll leave this as-is.
    icheck <- x[[1]][1]
  
    if( all( is.na( icheck ) ) ) return(FALSE) # it is NA; "any()" is necessary when checking tibbles (dplyr).
  
    # if( is.character(icheck) ) if( str_trim_fixed(icheck) %in% na_strings ) return( FALSE ) # check for na strings, including the blank.
  
    return(TRUE) # If we made it this far, it passed all checks and will not be considered nanull.
   
  # Check the elements. 
  } else { 
    
    # Vector.
    return( !is.na(x) )
    
  }

}

#' Not-In
#'
#' Opposite of %in% operator. From https://stackoverflow.com/a/46867726/4089266.
#' Author: Bryce Chamberlain.
#'
#' @param needle Vector to search for.
#' @param haystack Vector to search in.
#'
#' @return Boolean vector/value of comparisons.
#' @export
#'
#' @examples
#' c(1,3,11) %ni% 1:10
'%ni%' = function( needle, haystack ) ! needle %in% haystack