#include "RSSCppMatrix.h"

SEXP RSSCppMatrix(SEXP xadj, SEXP radius) {
  using namespace Rcpp ;
    
  // NumericMatrix x( xadj );
  // int r = as<int>( radius );
  
  return wrap( xadj );
}