TestUndirectedNetwork <- function(n) {
  M <- matrix(runif(n*n), nrow=n)
  M <- (M + t(M)) / 2
  diag(M) <- 0
  return(M)
}

M <- TestNetwork(6)
system.time(Rss.slow <- RelationStrengthSimilarity(M, radius=4, method="BetterR"))
Rss.fast <- RelationStrengthSimilarity(M, radius=4)
Rss.slow; Rss.fast

M <- TestNetwork(10)
system.time(Rss.slow <- RelationStrengthSimilarity(M, radius=4, method="BetterR"))
system.time(Rss.fast <- RelationStrengthSimilarity(M, radius=4))

M <- TestNetwork(20)
system.time(Rss.slow <- RelationStrengthSimilarity(M, radius=4, method="BetterR"))
system.time(Rss.fast <- RelationStrengthSimilarity(M, radius=4))

M <- TestNetwork(30)
system.time(Rss.slow <- RelationStrengthSimilarity(M, radius=4, method="BetterR"))  # 15.7 seconds
system.time(Rss.fast <- RelationStrengthSimilarity(M, radius=4))  # .21 seconds

M <- TestNetwork(40)
system.time(Rss.fast <- RelationStrengthSimilarity(M, radius=4))  # .79 seconds

M <- TestNetwork(75)
Rss.fast <- RelationStrengthSimilarity(M, radius=4)  # 13 seconds
system.time(Rss.slow <- RelationStrengthSimilarity(M, radius=3, directed=FALSE, method="BetterR"))  # 8.67 seconds
system.time(Rss.fast <- RelationStrengthSimilarity(M, radius=3, directed=FALSE))  # .18 seconds

M <- TestNetwork(100)
Rss.fast <- RelationStrengthSimilarity(M, radius=4)  # 53 seconds
Rss.fast <- RelationStrengthSimilarity(M, radius=3)  # 1 seconds

M <- TestNetwork(150)
Rss.fast <- RelationStrengthSimilarity(M, radius=3)  # 3 seconds

M <- TestNetwork(200)
Rss.fast <- RelationStrengthSimilarity(M, radius=3)  # 8 seconds

M <- TestNetwork(250)
Rss.fast <- RelationStrengthSimilarity(M, radius=3)  # 17 seconds

M <- TestNetwork(300)
Rss.fast <- RelationStrengthSimilarity(M, radius=3)  # 35 seconds

M <- TestNetwork(350)
Rss.fast <- RelationStrengthSimilarity(M, radius=3)  # 63 seconds

M <- TestNetwork(450)
Rss.fast <- RSSCppMatrix(M, radius=3)  # 172 seconds


x <- c(150, 200, 250, 300, 450)
y <- c(3, 7, 18, 35, 172)
res <- lm(log(y) ~ x)
summary(res)
plot(log(y) ~ x)
abline(res)