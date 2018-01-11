#' Eskin Index for categorical data
#' @description This function calculates the dissimilarity index based on Eskin
#'
#' @param dat the data
#' @param key a character representing the unique key name for observations.  For example, this could contain the name of patients in a health dataset.
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
eskinIndex <- function(dat, key = NULL, simm = FALSE) {

  dist <- matrix(0, nrow =nrow(dat), ncol = nrow(dat))   #matrix to be returned

  #if key is provided, name the columns of dist:
  if(!is.null(key)) {
    rownames(dist) <- dat[,key]
    colnames(dist) <- dat[,key]
    #eliminate the key so we don't accidently factor it in:
    dat[,key] <- NULL
  }

  for (var in names(dat)) {

    freqs <- rep(0,length(levels(dat[,var]))) #gather the frequencies of each category and name it
    names(freqs) <- levels(dat[,var])


    freqs <- sapply(names(freqs), function(freqName) {
      return(nrow(dat[which(dat[,var] == freqName),])^2/(nrow(dat[which(dat[,var] == freqName),])^2 + 2))
    })


    dist <- dist + eskinIndexGather(freqs = freqs
                                   , namesFreqs = names(freqs)
                                   , datVar = as.character(dat[,var]))



  }


  #manually assign diagonals to 0 and divide by total number of values
  numVars <- ncol(dat)
  dist <- dist/numVars
  if (!simm) {
    dist <- 1 - dist
    diag(dist) <- 0
  } else {
    diag(dist) <- 1
  }

  return(dist)



}
