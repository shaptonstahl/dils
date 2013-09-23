#include "rss_cpp_matrix.h"

double DotProduct(Rcpp::NumericVector a, Rcpp::NumericVector b) {
  using namespace Rcpp;
  double out = 0.0;
  
  for(int i = 0; i < a.size(); i++) {
    out += a(i) * b(i);
  }
  
  return(out);
}

Rcpp::NumericMatrix PrepMatrix(Rcpp::NumericMatrix x) {
  // Given a matrix divides each row by the rowsum
  using namespace Rcpp;
  double rowsum = 0.0;
  
  for(int i = 0; i < x.ncol(); i++) {
    x(i,i) = 0.0;
    rowsum = 0.0;
    for(int j = 0; j < x.ncol(); j++) {
      rowsum += x(i,j);
    }
    for(int j = 0; j < x.ncol(); j++) {
      x(i,j) /= rowsum;
    }
  }
  return(x);
}

double RssThisRadius(Rcpp::NumericMatrix x, int v1, int v2, int r) {
  // Given prepped adjacency matrix, from node and to node, and radius
  // returns RSS for that one link
  using namespace Rcpp;
  double out = 0.0;
  
  int n = x.nrow();
  
  if (v1 == v2) {
    out = 0.0;
  } else if (1 == r) {
    out = x(v1, v2);
  } else if (2 == r) {
    out = DotProduct(x(v1,_), x(_,v2));
  } else if (3 == r) {
    NumericVector y(n);
    for(int i = 0; i < n; i++) {
      y[i] = RssThisRadius(x, v1, i, 2) - x(v1, v2) * x(v2, i);
    }
    out = DotProduct(x(_,v2), y) + x(v1, v2) * x(v2, v1) * x(v1, v2);
  } else if (4 == r) {
    NumericVector y(n);
    for(int i = 0; i < n; i++) {
      y[i] = RssThisRadius(x, v1, i, 3) - 
        x(v2, i) * DotProduct(x(v1,_), x(_,v2)) +
        x(v2, i) * x(v1, i) * x(i, v2) +
        x(v1, v2) * x(v2, v1) * x(v1, i) -
        x(v1, v2) * DotProduct(x(v2,_), x(_,i));
    }
    out = DotProduct(x(_,v2), y) + 
      x(v1,v2) * x(v2,v1) * DotProduct(x(v1,_), x(_,v2)) +
      x(v1,v2) * x(v1,v2) * DotProduct(x(v2,_), x(_,v1));
  } else {
    out = -1000.0;
  }
  
  return(out);
}

double RssCell(Rcpp::NumericMatrix x, int v1, int v2, int r) {
  using namespace Rcpp;
  double out = 0.0;
  
  for(int i = 0; i < r; i++) {
    out += RssThisRadius(x, v1, v2, i+1);
  }
  
  return(out);
}

SEXP rss_cpp_matrix(SEXP xadj, SEXP radius, SEXP width) {
  using namespace Rcpp;
    
  NumericMatrix x( xadj );
  int r = as<int>( radius );
  int w = as<int>( width );
  
  NumericMatrix out(x.nrow(), x.ncol());
  
  x = PrepMatrix(x);
  
  Rcout << "test" << std::endl;
  
  for(int i = 0; i < x.nrow(); i++) {
    for(int j = 0; j < x.nrow(); j++) {
      out(i,j) = RssCell(x, i, j, r);
    }
  }
  
  return wrap( out );
}