#include <Rcpp.h>
using namespace Rcpp;


//[[Rcpp::export]]
NumericMatrix eskinIndexGather( NumericVector freqs, StringVector namesFreqs, StringVector datVar) {
  int n = datVar.size();
  NumericMatrix toReturn(n,n);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < i; j++) {
      if (datVar[i] == datVar[j]) {
        toReturn(i,j) = 1;
      } else {
        int index = 0;
        for (int k = 0; k < namesFreqs.size(); k ++) {
          if( namesFreqs[k] == datVar[j]) {
            index = k;
          }
        }
        toReturn(i,j) = freqs[index];
      }

      toReturn(j,i) = toReturn(i,j);
    }
  }

  return toReturn;
}

// [[Rcpp::export]]
NumericMatrix inverseOccurenceGather(NumericMatrix distLookup, StringVector namesDistLookup,StringVector datVar) {
  int n = datVar.size();
  NumericMatrix distAdd(n,n);


  for (int i = 0; i < n; i++) {
    for (int j = 0; j < i ; j++) {

      if (datVar[i] == datVar[j]) {
        distAdd(i,j) = 1;

      } else {
        int index1 = 0;
        int index2 = 0;
        for (int k = 0; k < namesDistLookup.size(); k ++) {
          if( namesDistLookup[k] == datVar[j]) {
            index1 = k;
          }
        }
        for (int l = 0; l < namesDistLookup.size(); l++) {
          if( namesDistLookup[l] == datVar[i]) {
            index2 = l;
          }
        }


        distAdd(i,j) = distLookup(index1,index2);
      }

      distAdd(j,i) = distAdd(i,j);
    }
  }

  return distAdd;

}



//[[Rcpp::export]]
NumericMatrix goodallIndexGather(NumericVector distLookup, StringVector namesDistLookup, StringVector datVar) {
  int n = datVar.size();
  NumericMatrix distAdd(n,n);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j <= i ; j++) {


      if (datVar[i] == datVar[j]) {
        int index = 0;
        for (int k = 0; k < namesDistLookup.size(); k ++) {
          if( namesDistLookup[k] == datVar[j]) {
            index = k;
          }
        }
        distAdd(i,j) = distLookup[index];

      } else {
        distAdd(i,j) = 0;
      }

      distAdd(j,i) = distAdd(i,j);
    }
  }

  return distAdd;
}

