data(iris)
library(ggplot2)


Z = iris[,1:4]
pc1 = prcomp(Z, scale=TRUE)

pc1
summary(pc1)
plot(pc1)
biplot(pc1)

# A more informative biplot
loadings = pc1$rotation
scores = pc1$x
qplot(scores[,1], scores[,2], color=iris$Species, xlab='Component 1', ylab='Component 2')
