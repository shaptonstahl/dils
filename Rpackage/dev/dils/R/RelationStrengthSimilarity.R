
RelationStrengthSimilarity <- function(xadj, v1, v2, radius){
  #' Add guardians here
  stopifnot( is.matrix(xadj) )
  stopifnot( ncol(xadj) == nrow(xadj) )

  stopifnot(v1 %% 1 == 0)
  stopifnot(v1 >= 0)
  stopifnot( v1 <= nrow(xadj) )

  stopifnot(v2 %% 1 == 0)
  stopifnot(v2 >= 0)
  stopifnot( v2 <= nrow(xadj) )
  
  stopifnot(radius %% 1 == 0)
  stopifnot(radius > 0)
  
  if( v1 == v2 ) {
    return( 1 )
  } else {
    return( .Call("relation_strength_similarity",
                  xadj, 
                  v1, 
                  v2, 
                  radius,
                  PACKAGE="dils" ) )
  }
}

