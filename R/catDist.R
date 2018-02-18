#' Dissimilarity Indices for Clustering
#'
#' This function calculates the dissimilarity index for categorical data.  In addition,
#' it provides a wrapper if numerical data are provided.
#' 
#' In common data science applications, many (if not all) variables may be categorical.  However, the default index used is typically Gower's
#' dissimilarity index, which assigns 1's to matches and 0's to mismatches.  Although Gower's index may be acceptable when most variables are 
#'continuous, it fails to include all the information provided when variables are all or almost all categorical. Hence, it may be beneficial
#'to use other dissimilarity metrics better suited to take in all the information provided by categorical variables.  For example, Goodall's index
#'assigns higher weighting to variables that match on an infrequent attribute, such as the presence of a disease.  
#'
#'This R package implements categorical dissimilarity indices from the paper "Similarity Measures for Categorical Data: A Comparative Evaluation," by Boriah,
#'Chandela, and Kumar (2008).  Although not all indices are implemented, the following are: Goodall1, Goodall2, Goodall3, Goodall4, Occurence Frequency,
#'Inverse Occurence Frequency, and the Eskin Index.  Currently, the plan is to implement most of the metrics from the paper.  
#'
#'Indices are called from the wrapper function catDist() by specifying the method parameter.  In addition, continuous variables may be passed into the function to 
#'create an analogue of gower's distance from the daisy() function from the cluster package.  Continuous variables are defaulted to manhattan distance, and
#'a weighted average of variables is taken from the continuous and categorical distances (after normalization).  
#'
#'The package is intended to be used with clustering applications in mind; in particular, with the pam() algorithm by Kaufman and Rousseeuw (1990).  In R, 
#'the pam() algorithm accepts dissimilarity matrices, and this package is intended to leverage that functionality.
#'
#' @param dat A data.frame or data.frame coercible object
#' @param method the method to be used
#' @param key the key of the observations
#' @param weights an optional vector containing weights
#' @param simm should a similarity index be returned?
#' @param metric the metric to be used when continuous variables are given (see @details)
#' @param ... additional arguments to be passed to daisy for continuous variables
#' @importFrom cluster daisy
#' @export
catDist <- function(dat, method = "goodall1", key = NULL,  weights = NULL, simm = FALSE,metric = 'manhattan',...) {
  dat2 <- droplevels(dat)
  #check for errors
  if(!is.data.frame(dat2)) {

    stop("Need to input data as a data frame")

  } else if(any(is.na(dat2))){

    stop("cannot have NA values")

  }

  numericVars <- sapply(names(dat2),function(x) {
    return(is.numeric(dat2[,x]) & !(x %in% c(key,weights)))
  })

  if (any(numericVars)) {
    if (simm) {
      simm <- FALSE
      warning('Treating simm as false as continuous variables are included')
    }
    dat2Numeric <- dat2[,numericVars]
    dist1 <- as.matrix(cluster::daisy(data.frame(dat2Numeric),metric = metric,...))
    dist1 <- dist1/nrow(dist1)
    if(!is.null(key)) {
      rownames(dist1) <- dat2[,key]
      colnames(dist1) <- dat2[,key]
    }
    dat2 <- dat2[,!numericVars]
  }


  if (method == "eskin") {
    if (!is.null(weights)) {
      warning("weights not yet implemented or not possible for eskin index.  continuing without weights")
    }
    dist <- eskinIndex(dat2,key,simm)

  } else if (method == "inverse occurence") {
    dist <- occurenceIndex(dat2,key,weights,simm,inverse = TRUE)
  } else if (method == "occurence") {

    dist <- occurenceIndex(dat2,key,weights,simm, inverse = FALSE)

  } else if (method == "goodall1") {
    dist <- goodallIndex(dat2, key, weights, simm, type = 1)
  } else if (method == "goodall2") {

    dist <- goodallIndex(dat2,key,weights,simm, type = 2)

  } else if (method == "goodall3") {

    dist <- goodallIndex(dat2,key,weights,simm, type = 3)

  } else if (method == "goodall4") {

    dist <- goodallIndex(dat2,key,weights,simm, type = 4)

  # } else if (method == "smirnov") {
  #   #TODO
  #   dist <- smirnovIndex(dat2,key,weights,simm)
  #
  # } else if (method == "gambaryan") {
  #   #TODO
  #   dist <- gambaryanIndex(dat2,key,weights,simm)
  #
  # } else if (method == "burnaby") {
  #   #TODO
  #   dist <- burnabyIndex(dat2,key,weights,simm)

  } else {

    stop("need to be one of possible dissimilarity (or similarity) indices.  See ?catDist for more help.")
  }


  if (any(numericVars)) {
    dist <- (dist*(nrow(dist)) + dist1*(nrow(dist1)))/(nrow(dist) + nrow(dist1))
  }

  return(dist)

}
