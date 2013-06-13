#include "relation_strength_similarity.h"

Rcpp::NumericVector NumericVectorConcatenate(Rcpp::NumericVector a, Rcpp::NumericVector b) {
  // I'm sure there is a command for this, but I can't figure it out, so....
  using namespace Rcpp;
  
  Rcpp::NumericVector c(a.size() + b.size());
  for(int i = 0; i < a.size(); i++) { 
    c[i] = a[i]; 
  }
  for(int i = a.size(); i < a.size() + b.size(); i++) { 
    c[i] = b[i]; 
  }
  return(c);
}

Rcpp::List ListConcatenate(Rcpp::List a, Rcpp::List b) {
  // I'm sure there is a command for this, but I can't figure it out, so....
  using namespace Rcpp;
  
  Rcpp::List c(a.size() + b.size());
  for(int i = 0; i < a.size(); i++) { 
    c[i] = a[i]; 
  }
  for(int i = a.size(); i < a.size() + b.size(); i++) { 
    c[i] = b[i]; 
  }
  return(c);
}

std::vector<int> GetNeighborIds(Rcpp::NumericMatrix x, int v, Rcpp::NumericVector excludeids) {
  // v, excludeids are C++-based indices
  using namespace Rcpp;
  
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

Rcpp::List GetPathsLength1(Rcpp::NumericMatrix x, int v, Rcpp::NumericVector excludeids) {
  // v, excludeids are C++-based indices
  using namespace Rcpp ;
  
  std::vector<int> neighbors = GetNeighborIds(x, v, excludeids);
  
  List out(neighbors.size());
  for(unsigned int i = 0; i < out.size(); i++) {
    out[i] = NumericVector::create(v, neighbors[i]);  // store C++-based indices
  }
  return( out );
}

Rcpp::List GetMinPathsGivenRadius(Rcpp::NumericMatrix x, int v, int r, Rcpp::NumericVector excludeids) {
  // v, excludeids are C++-based indices
  using namespace Rcpp ;
  
  std::vector<int> neighbors = GetNeighborIds(x, v, excludeids);
  
  if ( 1 == r ) {
    List out = GetPathsLength1(x, v, excludeids);
  } else {
    List out;
    List next_paths;
    std::vector<int> neighbors = GetNeighborIds(x, v, excludeids);
    for(int i = 0; i < neighbors.size(), i++) {
      List next_paths;
      next_paths = GetPathsLength1(x, neighbors[i], NumericVectorConcatenate(excludeids, NumericVector::create(v)));
      for(int j = 0; j < next_paths.size(); j++) {
        out = ListConcatenate(out, List::create(NumericVectorConcatenate(NumericVector::create(v), next_paths[j])));
      }
    }
  }
  return( out );
}

SEXP relation_strength_similarity(SEXP xadj, SEXP v1, SEXP v2, SEXP radius) {
  using namespace Rcpp ;
    
  NumericMatrix x( xadj );
  int vfrom = as<int>( v1 ) - 1;
  int vto = as<int>( v1 ) - 1;
  NumericVector r( radius );
  
  return wrap( GetMinPathsGivenRadius(x, vfrom, 2, NumericVector::create(3.0)) );
}
