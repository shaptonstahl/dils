#include "RelationStrengthSimilarity.h"

std::vector<int> GetNeighborIds(Rcpp::NumericMatrix x, int v, Rcpp::NumericVector excludeids) {
  using namespace Rcpp ;
    
  // v, exclude are C++-based indices
  std::vector<int> neighbors;
  NumericVector adj_row = x( v, _);
  adj_row[v] = 0.0;
  for(int i = 0; i < excludeids.length(); i++) {
    adj_row[excludeids[i]] = 0.0;
  }
  for(int i = 0; i < x.ncol(); i++) {
    if( adj_row[i] != 0.0 ) {
      neighbors.push_back( i );  // store C++-based indices
    }
  }
  return( neighbors );
}

SEXP RelationStrengthSimilarity(SEXP xadj, SEXP v1, SEXP v2, SEXP radius) {
  using namespace Rcpp ;
    
  NumericMatrix x( xadj );
  int vfrom = as<int>( v1 ) - 1;
  int vto = as<int>( v1 ) - 1;
  NumericVector r( radius );
  
  return wrap( GetNeighborIds(x, vfrom, NumericVector::create(3.0)) );
}
