if( !require(inline) ) {
  install.packages("inline")
  if( !require(inline) ) stop("Must install 'inline' package")
}
if( !require(RcppArmadillo) ) {
  install.packages("RcppArmadillo")
  if( !require(RcppArmadillo) ) stop("Must install 'RcppArmadillo' package")
}

cpp.body <- '
  int dim = as<int>( x ) ;
  arma::mat z = as<double>(y) * arma::eye<arma::mat>( dim, dim ) ;
  return wrap( arma::accu(z) ) ;
'
fx <- cxxfunction( sig=signature(x = "integer", y = "numeric" ), body=cpp.body, plugin="RcppArmadillo" )
fx( 2L, 5 )


cpp.body.mod <- '
  int dim = as<int>( x ) ;
  arma::mat z = as<double>(y) * arma::eye<arma::mat>( dim, dim ) ;
  return wrap( z ) ;
'
fx <- cxxfunction( sig=signature(x = "integer", y = "numeric" ), body=cpp.body.mod, plugin="RcppArmadillo" )
fx( 2L, 5 )


sig.matrix <- signature(x = "numeric")
cpp.body.matrix <- '
  arma::mat xx = as<arma::mat>( x );
  return(wrap(xx));
'
f.matrix <- cxxfunction(sig=sig.matrix, body=cpp.body.matrix, plugin="RcppArmadillo")
f.matrix( matrix(1:12, nrow=3) )


sig.GetNeighborIds <- signature(xadj="numeric", vi="integer", exclude="integer")
src.GetNeighborIds <- '
  NumericMatrix x(xadj);
  int v = as<int>( vi ) - 1;
  NumericVector excludeids( exclude );
  excludeids = excludeids - 1;
  std::vector<int> neighbors;
  NumericVector adj_row = x( v, _);
  
  adj_row[v] = 0.0;
  for(int i = 0; i < excludeids.length(); i++) {
    adj_row[excludeids[i]] = 0.0;
  }

  for(int i = 0; i < x.ncol(); i++) {
    if( adj_row[i] != 0.0 ) {
      neighbors.push_back( i + 1 );
    }
  }
  
  return wrap( neighbors );
'
GetNeighborIds <- cxxfunction(sig=sig.GetNeighborIds, body=src.GetNeighborIds, plugin="RcppArmadillo")
GetNeighborIds( matrix(1:12, nrow=3), 1, 3 )
GetNeighborIds( matrix(0:11, nrow=3), 1, numeric(0))
GetNeighborIds( matrix(0:11, nrow=3), 1, c(2:4))