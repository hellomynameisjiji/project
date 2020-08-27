# 1

x <- seq(0, 20, length.out = 101)
shape <- 1/2
scale <- 0.00001
y <- x^(shape-1)*exp(-x/scale)/(scale^shape *gamma(shape))
dev.new()
png("BayesHW3-12.png")
plot(x, y)
dev.off()
