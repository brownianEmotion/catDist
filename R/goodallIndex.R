#' Dissimilarity Indices With or Without Survey Weights
#'
#' This function calculates the dissimilarity index based on Goodall
#'
#' @param dat the data
#' @param key a character representing the unique key name for observations.
#' For example, this could contain the name of patients in a health dataset.
#' @param weights a character variable representing the name of the variable
#' associated with the survey weights
#' @param simm logical indicating whether a similarity should be returned
#'
#' @return A matrix of pairwise dissimilarities
#' @author Joshua Agterberg
#' @details
#' This calculates the pairwise dissimilarities for a dataset using the goodall dissimilarity
#' index first proposed by Goodall.
#' @useDynLib catDist
#' @importFrom Rcpp sourceCpp
goodall1Index <- function(dat, key = NULL, weights = NULL, simm = FALSE) {
  dist <- matrix(0, nrow =nrow(dat2), ncol = nrow(dat2))   #matrix to be returned

  #if key is provided, name the columns of dist:
  if(!is.null(key)) {
    rownames(dist) <- dat2[,key]
    colnames(dist) <- dat2[,key]
    #eliminate the key so we don't accidently factor it in:
    dat2[,key] <- NULL
  }

  #iterate through each variable
  for (var in names(dat2)[!names(dat2) %in% weights]) {

    freqs <- rep(0,length(levels(dat2[,var]))) #gather the frequencies of each category and name it
    names(freqs) <- levels(dat2[,var])

    if (!is.null(weights)) { #if we are including survey weights
      freqs <- sapply(names(freqs), function(freqName) {
        return(sum(dat2[which(dat2[,var] == freqName),weights]))
      })
    } else {
      freqs <- sapply(names(freqs), function(freqName) {
        return(length(dat2[which(dat2[,var] == freqName),var]))
      })
    }

    distLookup <- rep(0, length(freqs))
    names(distLookup) <- names(freqs)

    distLookup <- sapply(names(distLookup), function(p) {
      if(!is.null(weights)) {
        m <- sum(dat2[,weights])
      } else {
        m <- length(dat2[,var])
      }

      return(sum(ifelse(freqs <= freqs[p],freqs*(freqs - 1),0))/(m*(m-1)))
    })


    #gather the goodall index for this variable
    distAdd <- goodallIndexGather(distLookup = distLookup
                                  , namesDistLookup = names(distLookup)
                                  , datVar = as.character(dat2[,var]))


    dist = dist + distAdd


  } #iterating through each variable

  if (!is.null(weights)) {
    numVars <- length(names(dat2)) - 1
  } else {
    numVars <- length(names(dat2))
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


