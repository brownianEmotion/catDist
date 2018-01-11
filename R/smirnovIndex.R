#' Dissimilarity Indices With or Without Survey Weights
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
smirnovIndex <- function(dat,key = NULL, weights, simm = FALSE) {
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


    distLookupVector <- rep(0, length(freqs))
    names(distLookup) <- names(freqs)
    distLookupVector <- sapply(names(distLookup), function(p) {
      if(!is.null(weights)) {
        m <- sum(dat[,weights])
      } else {
        m <- length(dat[,var])
      }

      return(2*log(freqs[var]/m))
    })


    #the matrix to use if they do not match
    distLookupMatrix <- matrix(0, length(freqs),length(freqs))
    rownames(distLookupMatrix) <- names(freqs)
    colnames(distLookupMatrix) <- names(freqs)
    distLookupMatrix <- sapply(names(freqs), function(x) {
      freq1 <- freqs[x]
      toReturn <- sapply(freqs, function(freq2) {
        N <- length(freqs)
        toReturn1 <- 2*log(freq1/N + freq2/N)
        return(toReturn1)
      })

      return(toReturn)
    })



    #gather the linIndex for this variable
    distAdd <- linIndexGather(distLookupVector = distLookupVector
                              ,distLookupMatrix = distLookupMatrix
                              , namesDistLookup = names(distLookup)
                              , datVar = as.character(dat[,var]))

    variableWeight <- 0
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
