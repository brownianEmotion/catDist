#' Eskin Index for categorical data
#' @description This function calculates the dissimilarity index based on Eskin
#'
#' @param dat the data
#' @param key a character representing the unique key name for observations.  For example, this could contain the name of patients in a health dataset.
#' @param weights a character variable representing the name of the variable associated with the survey weights
#' @param simm logical indicating whether a similarity should be returned
#'
#' @return A matrix of pairwise dissimilarities
#' @author Joshua Agterberg
#' @details
#' This calculates the pairwise dissimilarities for a dataset using the goodall dissimilarity
#' index first proposed by Eskin.  Access through catDist function
#'
#' @useDynLib catDist
#' @importFrom Rcpp sourceCpp
eskinIndex <- function(dat2, key = NULL, simm = FALSE) {

  dist <- matrix(0, nrow =nrow(dat2), ncol = nrow(dat2))   #matrix to be returned

  #if key is provided, name the columns of dist:
  if(!is.null(key)) {
    rownames(dist) <- dat2[,key]
    colnames(dist) <- dat2[,key]
    #eliminate the key so we don't accidently factor it in:
    dat2[,key] <- NULL
  }

  for (var in names(dat2)) {

    freqs <- rep(0,length(levels(dat2[,var]))) #gather the frequencies of each category and name it
    names(freqs) <- levels(dat2[,var])


    freqs <- sapply(names(freqs), function(freqName) {
      return(nrow(dat2[which(dat2[,var] == freqName),])^2/(nrow(dat2[which(dat2[,var] == freqName),])^2 + 2))
    })


    dist <- dist + eskinIndexGather(freqs = freqs
                                   , namesFreqs = names(freqs)
                                   , datVar = as.character(dat2[,var]))



  }


  #manually assign diagonals to 0 and divide by total number of values
  numVars <- ncol(dat2)
  dist <- dist/numVars
  if (!simm) {
    dist <- 1 - dist
    diag(dist) <- 0
  } else {
    diag(dist) <- 1
  }

  return(dist)



}
