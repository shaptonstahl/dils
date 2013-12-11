
x.unif <- runif(10)
plot(0:1000/1000, ecdf(x.unif)(0:1000/1000), type="l")


QuantilePiecewiseLinear <- function(x, .data) {
  n.less <- sum(.data < x)
  if(0 == n.less) return(0)
  
  n <- length(.data)
  if(n == n.less) return(1)
  
  sorted.data <- sort(.data)
  lower <- sorted.data[n.less]
  upper <- sorted.data[n.less + 1]
  q.lower <- n.less / n
  q.upper <- (n.less+1) / n
  return(q.lower + (q.upper-q.lower)/(upper-lower) * (x-lower))
}
y <- sapply(0:1000/1000, function(this.x) QuantilePiecewiseLinear(this.x, x.unif))
lines(0:1000/1000, y)

QuantileDensityPiecewiseLinear <- function(x, .data) {
  n.less <- sum(.data < x)
  if(0 == n.less) return(0)
  
  n <- length(.data)
  if(n == n.less) return(0)
  
  sorted.data <- sort(.data)
  lower <- sorted.data[n.less]
  upper <- sorted.data[n.less + 1]
  q.lower <- n.less / n
  q.upper <- (n.less+1) / n
  return((q.upper-q.lower)/(upper-lower))
}
x.norm <- rnorm(10)
y <- sapply(-400:400/100, function(this.x) QuantileDensityPiecewiseLinear(this.x, x.norm))
plot(-400:400/100, y, type="l")

x.norm <- rnorm(100)
y <- sapply(-400:400/100, function(this.x) QuantileDensityPiecewiseLinear(this.x, x.norm))
plot(-400:400/100, y, type="l")

