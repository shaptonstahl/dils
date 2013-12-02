me <- function(g, 
               FUN=function(x) evcent(x)$vector, 
               score.range=c(0,1)) {
  scores <- FUN(g)
  
  if( max(scores) == min(scores) ) return(0)
  
  scores.out.of.range <- sum(scores < score.range[1]) + sum(scores > score.range[2])
  if( scores.out.of.range > 0 ) {
    warning(round(scores.out.of.range/length(scores)*1000)/10, "% of scores outside of range indicated. Using observed range of values.")
    score.range <- range(scores)
  }
  freqs <- discretize(scores, 
                      numBins=max(c(10, round(sqrt(length(scores))))),
                      r=score.range)
  return( entropy(freqs) )
}

mes <- sapply(1:30, function(p) {
  reps <- replicate(1e2, me(random.graph.game(100, p/100)))
  return( mean(reps) )
})
plot(1:30 / 100, mes, type="l", 
     main="Measured Entropy for Random Graphs",
     xlab="Probability of drawing an edge",
     ylab="me")
abline(v=which.max(mes)/100)
