#' Goodall Index (Versions 1,2,3, and 4) for categorical data
#'
#' This function calculates the dissimilarity index based on Goodall
#'
#' @param dat the data
#' @param key a character representing the unique key name for observations.
#' For example, this could contain the name of patients in a health dataset.
#' @param weights a character variable representing the name of the variable
#' associated with the survey weights
#' @param simm logical indicating whether a similarity should be returned
#' @param type a number in 1,2,3, or 4 indicating the the type of goodall index
#' @return A matrix of pairwise dissimilarities
#' @author Joshua Agterberg
#' @details
#' This calculates the pairwise dissimilarities for a dataset using the goodall dissimilarity
#' index first proposed by Goodall.
#' @useDynLib catDist
#' @importFrom Rcpp sourceCpp
goodallIndex <- function(dat, key = NULL, weights = NULL, simm = FALSE,type = 1,diag=FALSE) {
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

    distLookup <- rep(0, length(freqs))
    

    distLookup <- sapply(names(distLookup), function(p) {
      if(!is.null(weights)) {
        m <- sum(dat[,weights])
      } else {
        m <- length(dat[,var])
      }

      if (type == 1) {
        #shouldn't be 1 -?
        return(1 - sum(ifelse(freqs <= freqs[p],freqs*(freqs - 1),0))/(m*(m-1)))
      } else if (type == 2) {
        return(1 - sum(ifelse(freqs >= freqs[p],freqs*(freqs - 1),0))/(m*(m-1)))
      } else if (type == 3) {
        return(1 - freqs[p]*(freqs[p] - 1)/(m*(m-1)))
      } else if (type == 4) {
        return( freqs[p]*(freqs[p] - 1)/(m*(m-1)))
      } else {
        stop("Goodall Index Type must be in types 1,2,3, or 4.  See documentation for help")
      }

    })
    
    names(distLookup) <- names(freqs)


    #gather the goodall index for this variable
    distAdd <- goodallIndexGather(distLookup = distLookup
                                  , namesDistLookup = names(distLookup)
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

  if (diag) {
    diag(dist) <- 1
  }

  if (!simm) {
    dist <- 1-dist
  }

  return(dist)
}

