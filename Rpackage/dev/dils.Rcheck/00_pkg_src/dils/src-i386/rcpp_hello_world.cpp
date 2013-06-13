#include "rcpp_hello_world.h"

SEXP inside_function(){
  using namespace Rcpp ;
  
  return NumericVector::create( 0.0, 2.0 ) ;
}

SEXP rcpp_hello_world(){
    using namespace Rcpp ;
    
    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = inside_function();
    List z            = List::create( x, y ) ;
    
    return z ;
}
