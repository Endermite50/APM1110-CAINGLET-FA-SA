
# Section 1 - Item 3

# Input the probability
p_input <- readline("Enter probability p (default is 0.6): ")
p <- if (p_input == "") 0.6 else as.numeric(p_input)

if (is.na(p) || p <= 0 || p > 1) {
  stop("p_input value must be greater than 0 and at most 1.")
}

# Number of simulations
n <- 10000

# Simulate the number of searches
X <- rgeom(n, prob = p) + 1

# Probability distribution
dist <- prop.table(table(X))
dist

# Plot the probability distribution
barplot(dist, main = "Simulated Probability Distribution",
        xlab = "Number of Searches", ylab = "Probability",
        col = "pink")

# Mean and variance
mean(X)
var(X)

# Conditional distribution after 3 unsuccessful searches
Y <- X[X > 3] - 3

cdist <- prop.table(table(Y))
cdist

# Conditional mean and variance
mean(Y)
var(Y)

# Compare the distributions
maxX<- max(c(X, Y))

orig <- prop.table(table(factor(X, levels = 1:maxX)))
cond <- prop.table(table(factor(Y, levels = 1:maxX)))

barplot(rbind(orig, cond),
        beside = TRUE, main = "Original vs Conditional Distribution",
        xlab = "Number of Searches", ylab = "Probability",
        col = c("pink", "lightblue"),
        legend.text = c("Original", "After 3 Failures"))

# Memoryless property estimate
p4 <- sum(X == 4) / sum(X > 3)
p1 <- mean(X == 1)

p5 <- sum(X == 5) / sum(X > 3)
p2 <- mean(X == 2)

p4
p1
p5
p2