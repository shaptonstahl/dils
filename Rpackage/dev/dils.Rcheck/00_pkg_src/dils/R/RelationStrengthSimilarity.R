
RelationStrengthSimilarity <- function(xadj, v1, v2, radius){
  #' Add guardians here
  #' 
  #' 
  
	.Call("relation_strength_similarity",
	      xadj, 
        v1, 
        v2, 
        radius,
        PACKAGE="dils" )
}

