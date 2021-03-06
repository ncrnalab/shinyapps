#' Create quick and beautiful maps of Denmark at different levels of geographic detail
#'
#' @title Maps of Denmark
#'
#' @name mapDK
#'
#' @author Sebastian Barfort (\email{sebastianbarfort@@gmail.com})
#'
#' @return A ggplot class object
#'
#' @param values,id String variables specifying names of value and id columns in the dataset
#' @param data A data frame of values and ids
#' @param detail A string specifying the detail level of the map
#' @param show_missing A logical scalar. Should missing values (including NaN) be showed?
#' @param sub A vector of strings specifying subregions to be plotted
#' @param guide.label A string with custom label name
#' @param map.title A string with map title
#'
#' @seealso \url{https://github.com/sebastianbarfort/mapDK}
#' @examples
#'
#' mapDK(detail = "polling")
#' mapDK(detail = "zip")
#' mapDK(values = "indbrud", id = "kommune", data = crime)
#'
#' @export
#'
#'
#'
#'

# remove DK characters function
remove_dk <- function(x){
  x <- gsub("\\u00e6", "ae", x)
  x <- gsub("\\u00f8", "oe", x)
  x <- gsub("\\u00e5", "aa", x)
  return(x)
}
# remove non-alphanumeric characters and transform to lowercase
onlyChar <- function(string) {
  string <- tolower(gsub(" ", "", gsub("[^[:alnum:]]", " ", string)))
  string <- stringi::stri_escape_unicode(string)
  string <- remove_dk(string)
  string <- gsub("\\\\", "", string)
  return(string)
}


mapDK <- function(shapedata, values = NULL, id = NULL, data,
  show_missing = TRUE, sub = NULL,
  sub.plot = NULL,
  guide.label = NULL, map.title = NULL,
  map.fill = "gray92", map.colour = "black"){
  
  my.sub = sub
  
  
  if (!missing(data)){
    # remove tbl_df from data frame
    if(sum(class(data) == "tbl_df") > 0){
      data = data.frame(data)
    }
    if (is.null(guide.label)){
      guide.label = values
    }
    # make sure both values and id is provided
    if (is.null(values) | is.null(id)){
      stop(paste("You must provide value and id columns as strings"))
    }
    
    values = data[, values]
    
    id.text = data[, id]  # original text
    id = data[, id]
    
    id = onlyChar(id)
    # id in shapedata
    id.shape <- unique(shapedata$id)
    # transform values to factor
    if(is.numeric(values)) {
      discrete <- FALSE
    } else {
      discrete <- TRUE
      values <- as.factor(values)
    }
    if(is.numeric(id)) {
      id_input <- id
      id <- id.shape[!is.na(match(onlyChar(id.shape), onlyChar(id)))]
    }
    # NA if not all region are provided
    match.all <- match(onlyChar(id.shape), onlyChar(id))
    # NA if some region is not recognized
    match.missing <- match(onlyChar(id), onlyChar(id.shape))
    # do all ids match?
    if(sum(is.na(match.missing)) > 0) {
      warning(paste("Some id not recognized:",
        paste(id[is.na(match.missing)], collapse = ", ")))
    }
    # any regions without data?
    if(sum(is.na(match.all)) > 0) {
      warning(paste("You provided no data for the following ids:",
        paste(id.shape[is.na(match.all)], collapse = ", ")))
    }
    pos <- match(onlyChar(shapedata$id), onlyChar(id))
    # show missing?
    if (show_missing == FALSE) {
      sub_fromData <- id.shape[!is.na(match(onlyChar(id.shape), onlyChar(id)))]
      if (is.null(sub)) {
        sub <- sub_fromData
      }
      else {
        sub <- sub[onlyChar(sub) %in% onlyChar(sub_fromData)]
      }
    }
    # select sub ids?
    if (!is.null(sub)) {
      # Match sub and region
      sub_match_all <- match(onlyChar(shapedata$id), onlyChar(sub))
      sub_match_missing <- match(onlyChar(sub), onlyChar(shapedata$id))
      # Remove shapedata not in sub
      shapedata <- shapedata[onlyChar(shapedata$id) %in% onlyChar(sub), ]
      # Remove values not in sub
      values <- values[onlyChar(id) %in% onlyChar(sub)]
      # Remove pos not in sub
      pos <- sub_match_all[which(!is.na(sub_match_all))]
      # Check if some region sub is not matched
      if(sum(is.na(sub_match_missing)) > 0) {
        warning(paste("Some sub not recognized:",
          paste(id[is.na(sub_match_missing)], collapse = ", ")))
      }
    }
    # add values to shapedata
    
    shapedata[, "values"] <- values[pos]
    shapedata[, "municipality"] <- id.text[pos]
    
  }
  else {
    if (!is.null(sub)) {
      # Match sub and region
      sub_match_missing <- match(onlyChar(sub), onlyChar(shapedata$id))
      # Remove shapedata not in sub
      shapedata <- shapedata[onlyChar(shapedata$id) %in% onlyChar(sub), ]
      # Remove values not in sub
      if(sum(is.na(sub_match_missing)) > 0) {
        warning(paste("Some sub not recognized:", paste(sub[is.na(sub_match_missing)], collapse = ", ")))
      }
    }
    test <- ""
  }
  # plot
 
  
  full_rect <- data.frame (xmin=min (shapedata$long), xmax=max (shapedata$long), 
                           ymin=min (shapedata$lat), ymax=max (shapedata$lat),
                           municipality="Denmark", group="Denmark")

                           
  gp <- ggplot()
    
  fr <- geom_rect (data=full_rect, aes (xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, key=municipality, group=group), fill=NA, color=NA)
    
  thm <- theme(axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position = "top",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    plot.title = element_text(face='bold'),
    strip.text.x = element_text(size = rel(1), face='bold'),
    strip.background = element_rect(colour="white", fill="white"))
  
  map <- geom_polygon(data=shapedata, aes_string(x = "long", y = "lat", group = "group", key="municipality"), fill = map.fill, colour = map.colour, size=0.5)
  
  if (!is.null(values)){
    if(length(unique(sub)) == 1){
      map <- geom_polygon(data=shapedata, aes_string(x = "long", y = "lat", group = "group", key="municipality"), colour = map.colour, size=0.5)
    }
    else {
      map <- geom_polygon(data=shapedata, aes_string(x = "long", y = "lat", group = "group", key="municipality", fill = "values"), colour = map.colour, size=0.5) 
    }
    # if (discrete == TRUE) {
    #   scf <- scale_fill_discrete(name = guide.label)
    # }
    # else {
    #   scf <- scale_fill_continuous(name = guide.label)
    # }
    plt <- gp + fr +  map + thm # + scf
  }
  else {
    plt <- gp +  fr + map + thm
  }
  
  if(!is.null(map.title)){
    lab <- labs(x = "", y = "", title = map.title)
    plot <- plt + lab
  }
  else {
    plot <- plt
  }
  
  # facet if more than one sub.plot municipality provided
  if(length(sub.plot) > 1 ){
    plot <- plot + facet_wrap(~ KommuneNav, scales = "free")
  }
  else if(length(my.sub) > 1){
    plot <- plot + facet_wrap(~ id, scales = "free")
  }
  
  #plot = plot + coord_map()
  return(plot + labs (fill = "Fraction\npositive"))
}
