##################################
# ENGR 421 Fall 2020 Homework 6  #
# Harun Sasmaz                   #
# 59900                          #
##################################

set.seed(421)

# given means
class_means <- matrix(c(+2.5, +2.5,
                        -2.5, +2.5,
                        -2.5, -2.5,
                        +2.5, -2.5,
                         0.0,  0.0), 2, 5)
# given covariances
class_covariances <- array(c(+0.8, -0.6, -0.6, +0.8,
                             +0.8, +0.6, +0.6, +0.8,
                             +0.8, -0.6, -0.6, +0.8,
                             +0.8, +0.6, +0.6, +0.8,
                             +1.6,  0.0,  0.0, +1.6), c(2, 2, 5))
# given class sizes
class_sizes <- c(50,50,50,50,100)

# generate random points for each class
points1 <- mvrnorm(n = class_sizes[1], mu = class_means[,1], Sigma = class_covariances[,,1])
points2 <- mvrnorm(n = class_sizes[2], mu = class_means[,2], Sigma = class_covariances[,,2])
points3 <- mvrnorm(n = class_sizes[3], mu = class_means[,3], Sigma = class_covariances[,,3])
points4 <- mvrnorm(n = class_sizes[4], mu = class_means[,4], Sigma = class_covariances[,,4])
points5 <- mvrnorm(n = class_sizes[5], mu = class_means[,5], Sigma = class_covariances[,,5])
X <- rbind(points1, points2, points3, points4, points5)

H <- ncol(X)
N <- sum(class_sizes)

# plot data points generated
plot(X[,1], X[,2], type = "p", pch = 19, col = "black", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")

# Run k-means clustering for 2 iterations
K <- 5
centroids <- X[sample(1:N, K),]
Dist <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
Dist <- Dist[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
assignments <<- sapply(1:ncol(Dist), function(i) {which.min(Dist[,i])})
for (k in 1:K) {
  centroids[k,] <- colMeans(X[assignments == k,])
} 
Dist <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
Dist <- Dist[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
assignments <<- sapply(1:ncol(Dist), function(i) {which.min(Dist[,i])})
for (k in 1:K) {
  centroids[k,] <- colMeans(X[assignments == k,])
}

# Calculate initial covariances and prior probabilities
prior <- sapply(1:K, function (c){
  sum(assignments == c)
}) / N
covariances <- t(sapply(1:K, function (c) {
  cov(X[assignments == c,])
}))
means <- centroids

# Apply EM Clustering for 100 iterations
get_covariance <- function (c) {
  return(matrix(covariances[c,],2,2))
}

for (it in 1:100) {
  G_i_c <- function (i, c) {
    prior[c] * ((det(get_covariance(c)))^(-0.5)) *
      exp((-0.5)* matrix((X[i,]- means[c,]), ncol=2) %*% t((X[i,]- means[c,]) %*% (solve(get_covariance(c)))))
  }
  
  Gic <- matrix(nrow=N, ncol=5)
  for (i in 1:N) {
    for (c in 1:K) {
      Gic[i,c] <- G_i_c(i,c)
    }
  }
  
  G_sum <- rep(0, N)
  for (i in 1:N) {
    G_sum[i] <-sum(sapply(1:K, function (c) {
      Gic[i,c]
    }))
  }
  
  h_i_c <- function (i,c) {
    Gic[i,c] / G_sum[i]
  }
  
  # update priors
  for (c in 1:K) {
    sum <- 0
    for (i in 1:N) {
      sum <- sum + h_i_c(i,c)
    }
    prior[c] <- sum / length(X)
  }
  
  # update means
  for (c in 1:K) {
    f_sum <- c(0,0)
    h_sum <- 0
    for (i in 1:N) {
      h <- h_i_c(i,c)
      f_sum <- f_sum + h * X[i,]
      h_sum <- h_sum + h
    }
    means[c, ] <- f_sum / h_sum
  }
  
  # update covs
  for (c in 1:K) {
    h_sum <- 0
    s_sum <- matrix(c(0,0,0,0),2,2)
    for (i in 1:N) {
      h <- h_i_c(i,c)
      s_sum <- s_sum + (X[i, ] - means[c,]) %*% t((X[i, ] - means[c,])) * h
      h_sum <- h_sum + h
    }
    covariances[c, ] <- s_sum / h_sum
  }
}

# Print resulting means
print(means)

# Plot clustering bounds for each class
Dist <- as.matrix(dist(rbind(means, X), method = "euclidean"))
Dist <- Dist[1:nrow(means), (nrow(means) + 1):(nrow(means) + nrow(X))]
assignments <<- sapply(1:ncol(Dist), function(i) {which.min(Dist[,i])})

# plot data points generated
plot(X[assignments == 1, 1], X[assignments == 1, 2], type = "p", pch = 19, col = "blue", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")
points(X[assignments == 2, 1], X[assignments == 2, 2], type = "p", pch = 19, col = "orange")
points(X[assignments == 3, 1], X[assignments == 3, 2], type = "p", pch = 19, col = "red")
points(X[assignments == 4, 1], X[assignments == 4, 2], type = "p", pch = 19, col = "purple")
points(X[assignments == 5, 1], X[assignments == 5, 2], type = "p", pch = 19, col = "green")

for(c in 1:K){
  # I used alpha = 0.25 instead of 0.05 because ellipse becomes really big for 0.05 
  ellipse(class_means[,c], class_covariances[,,c], alpha = .25, npoints = class_sizes[c], newplot = FALSE, draw = TRUE, lty=2, lwd=2)
  ellipse(means[c,], matrix(covariances[c,], 2,2), alpha = .25, npoints = class_sizes[c], newplot = FALSE, draw = TRUE, lwd=2)
}
