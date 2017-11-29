#' Dissimilarity Indices for Clustering
#' @param dat A data.frame or data.frame coercible object
#' @param method the method to be used
#' @param key the key of the observations
#' @param weights an optional vector containing weights
#' @param simm should a similarity index be returned?
#' @export
catDist <- function(dat, method = "goodall1", key = NULL,  weights = NULL, simm = FALSE) {
  dat2 <- droplevels(dat)
  #check for errors
  if(!is.data.frame(dat2)) {

    stop("Need to input data as a data frame")

  } else if(any(is.na(dat2))){

    stop("cannot have NA values")

  }



  if (method == "goodall1") {
    dist <- goodall1Index(dat, key, weights, simm)
  } else if (method == "goodall2") {

  } else if (method == "goodall3") {

  } else if (method == "goodall4") {

  } else if (method == "eskin") {
    if (!is.null(weights)) {
      warning("weights not yet implemented or not possible for eskin index.  continuing without weights")
    }
    dist <- eskinIndex(dat,key,simm)

  } else {
    stop("need to be one of possible dissimilarity indices.  See ?catDist for more help.")
  }


  return(dist)

}
