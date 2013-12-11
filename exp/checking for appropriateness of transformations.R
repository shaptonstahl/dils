#' Should a log or exponential transformation be recommended?

x.norm <- rnorm(1000) + 10
x.log.norm <- log(x.norm)
x.exp.norm <- exp(x.norm)

hist(x.norm)
hist(x.log.norm)
hist(x.exp.norm)

#' Test for normality of each transformation of the variable
shapiro.test(x.norm)
shapiro.test(log(x.norm))
shapiro.test(exp(x.norm))
#' p-value above 0.1 implies normal

#' What about uniformity?  That's fine
x.unif <- runif(1000)
x.log.unif <- log(x.unif)
x.exp.unif <- exp(x.unif)

shapiro.test(x.unif)
shapiro.test(log(x.unif))
shapiro.test(exp(x.unif))

unif.test <- function(x) {
  x <- x - min(x)
  u <- x / max(x)
  n <- qnorm(u)
  n <- n[is.finite(n)]
  return( shapiro.test(n) )
}

unif.test(x.unif)
unif.test(x.norm)
unif.test(x.log)
unif.test(x.exp)

RecommendTransformation <- function(x, min.p=.05) {
  if(shapiro.test(x)$p.value > min.p) return("no recommendation (is normal)")
  if(unif.test(x)$p.value > min.p) return("no recommendation (is uniform)")
  if(shapiro.test(log(x))$p.value > min.p) return("recommend log transform (result is normal)")
  if(unif.test(log(x))$p.value > min.p) return("recommend log transform (result is uniform)")
  if(shapiro.test(exp(x))$p.value > min.p) return("recommend exp transform (result is normal)")
  if(unif.test(exp(x))$p.value > min.p) return("recommend exp transform (result is uniform)")
  return("no recommendation")
}

RecommendTransformation(x.norm)
RecommendTransformation(x.log.norm)
RecommendTransformation(x.exp.norm)
RecommendTransformation(x.unif)
RecommendTransformation(x.log.unif)
RecommendTransformation(x.exp.unif)
RecommendTransformation(rbeta(1000, .25, .25))

