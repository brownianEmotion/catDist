#' Occurence (or inverse occurence) index for categorical data
#'
#' This function calculates the dissimilarity index based on the (inverse) occurence index
#'
#' @param dat the data
#' @param key a character representing the unique key name for observations.
#' For example, this could contain the name of patients in a health dataset.
#' @param weights a character variable representing the name of the variable
#' associated with the survey weights
#' @param simm logical indicating whether a similarity should be returned
#' @param inverse parameter indicating whether to return the inverse occurence frequency or the occurence frequency
#' @return A matrix of pairwise dissimilarities
#' @author Joshua Agterberg
#' @details
#' This calculates the pairwise dissimilarities for a dataset using the (inverse) occurence dissimilarity
#' index
#' @useDynLib catDist
#' @importFrom Rcpp sourceCpp
occurenceIndex <- function(dat,key = NULL, weights, simm = FALSE, inverse = TRUE) {
  dist <- matrix(0, nrow =nrow(dat), ncol = nrow(dat))   #matrix to be returned

  #if key is provided, name the columns of dist:
  if(!is.null(key)) {
    rownames(dist) <- dat[,key]
    colnames(dist) <- dat[,key]
    #eliminate the key so we don't accidently factor it in:
    dat[,key] <- NULL
  }

  #iterate through each variable
  for (var in names(dat)[!names(dat) %in% weights]) {

    freqs <- rep(0,length(levels(dat[,var]))) #gather the frequencies of each category and name it
    names(freqs) <- levels(dat[,var])

    if (!is.null(weights)) { #if we are including survey weights
      freqs <- sapply(names(freqs), function(freqName) {
        return(sum(dat[which(dat[,var] == freqName),weights]))
      })
    } else {
      freqs <- sapply(names(freqs), function(freqName) {
        return(length(dat[which(dat[,var] == freqName),var]))
      })
    }

    distLookup <- matrix(0, length(freqs),length(freqs))
    rownames(distLookup) <- names(freqs)
    colnames(distLookup) <- names(freqs)

    distLookup <- sapply(names(freqs), function(x) {
      freq1 <- freqs[x]
      freq2 <- sapply(names(freqs), function(y){
        return(freqs[y])
      })
      if (!inverse) {
        toReturn <- 1+log(freq1)*log(freq2)
      } else {
        N <- length(freqs)
        toReturn <- 1+log(N/freq1)*log(N/freq2)
      }

      return(1/toReturn)
    })




    #gather the goodall index for this variable
    distAdd <- inverseOccurenceGather(distLookup = distLookup
                               , namesDistLookup = rownames(distLookup)
                                  , datVar = as.character(dat[,var]))


    dist = dist + distAdd


  } #iterating through each variable

  if (!is.null(weights)) {
    numVars <- length(names(dat)) - 1
  } else {
    numVars <- length(names(dat))
  }


  #manually assign diagonals to 0 and divide by total number of values
  dist <- dist/numVars
  if (!simm) {
    dist <- 1 - dist
    diag(dist) <- 0
  } else {
    diag(dist) <- 1
  }

  return(dist)

}
