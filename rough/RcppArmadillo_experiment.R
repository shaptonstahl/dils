if( !require(inline) ) {
  install.packages("inline")
  if( !require(inline) ) stop("Must install 'inline' package")
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
  arma::mat x = as<arma::mat>( xadj );
  int v = as<int>( vi );
  arma::ivec excludeids = as<arma::ivec>( exclude );
  double neighbors = 0.0;
  
  return wrap( neighbors );
'
GetNeighborIds <- cxxfunction(sig=sig.GetNeighborIds, body=src.GetNeighborIds, plugin="RcppArmadillo")