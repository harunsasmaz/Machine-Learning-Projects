####################################
# ENGR 421 - Homework 5            #
# Decision Tree Regression         #
#                                  #
# Harun Sasmaz                     #
# 0059900                          #
####################################

### PART 2 : Read and split input data
data_set <- read.csv("hw04_data_set.csv")

x <- data_set$x
y <- data_set$y

x_train <- x[1:100]
y_train <- y[1:100]

x_test <- x[101:133]
y_test <- y[101:133]

minimum_value <- floor(min(x)) - 2 
maximum_value <- ceiling(max(x)) + 2
N_train <- length(x_train)
N_test <- length(x_test)
K <- max(y)

### PART 3 : Implement Decision Tree Algorithm
DecisionTree <- function(P) 
{
  node_splits <- c()
  node_means <- c()
  node_indices <- list(1:100)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  
  while (1) 
  {
    split_nodes <- which(need_split)
    
    if (length(split_nodes) == 0) 
    {
      break
    }
    for (split_node in split_nodes) 
    {
      need_split[split_node] <- FALSE
      data_indices <- node_indices[[split_node]]
      node_mean <- mean(y_train[data_indices])
      
      if (length(x_train[data_indices]) <= P) 
      {
        is_terminal[split_node] <- TRUE
        node_means[split_node] <- node_mean
      } 
      else 
      {
        is_terminal[split_node] <- FALSE
        unique_values <- sort(unique(x_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        
        for (s in 1:length(split_positions)) 
        {
          error <- 0
          left_indices <- data_indices[which(x_train[data_indices] <= split_positions[s])]
          right_indices <- data_indices[which(x_train[data_indices] > split_positions[s])]
          
          if (length(left_indices) > 0) 
          {
            mean <- mean(y_train[left_indices])
            error <- error + sum((y_train[left_indices] - mean) ^ 2)
          }
          
          if (length(right_indices) > 0) 
          {
            mean <- mean(y_train[right_indices])
            error <- error + sum((y_train[right_indices] - mean) ^ 2)
          }
          
          split_scores[s] <- error / (length(left_indices) + length(right_indices))
        }
        if (length(unique_values) == 1) 
        {
          is_terminal[split_node] <- TRUE
          node_means[split_node] <- node_mean
          next 
        }
        
        best_split <- split_positions[which.min(split_scores)]
        node_splits[split_node] <- best_split
  
        left_indices <- data_indices[which(x_train[data_indices] < best_split)]
        need_split[2 * split_node] <- TRUE
        is_terminal[2 * split_node] <- FALSE
        node_indices[[2 * split_node]] <- left_indices

        right_indices <- data_indices[which(x_train[data_indices] >= best_split)]
        need_split[2 * split_node + 1] <- TRUE
        is_terminal[2 * split_node + 1] <- FALSE
        node_indices[[2 * split_node + 1]] <- right_indices

      }
    }
  }
  return(list("node_splits"= node_splits, "node_means"= node_means, "is_terminal"= is_terminal))
}

### PART 4 : Learn a decision tree for P = 15
prediction <- function(data_point, is_terminal, node_splits, node_means){
  n <- 1
  while (1) 
  {
    if (is_terminal[n] == TRUE) 
    {
      return(node_means[n])
    } 
    else 
    {
      if (data_point <= node_splits[n]) {
        n <- n * 2
      } else {
        n <- n * 2 + 1
      }
    }
  }
}

P <- 15
decision_tree <- DecisionTree(P)
node_splits <- decision_tree$node_splits
node_means <- decision_tree$node_means
is_terminal <- decision_tree$is_terminal

data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", main = "P = 15")
points(x_test, y_test, type = "p", pch = 20, col= "red")
legend("topleft", legend=c("training", "test"),
       col=c("blue", "red"), pch = 20, cex = 1)

for (n in 1:length(data_interval)) 
{
  x_left <- data_interval[n]
  x_right <- data_interval[n+1]
  lines(c(x_left, x_right), c(prediction(x_left, is_terminal, node_splits, node_means), prediction(x_left, is_terminal, node_splits, node_means)), lwd = 1.5, col = "black")
  if (n < length(data_interval)) {
    lines(c(x_right, x_right), c(prediction(x_left, is_terminal, node_splits, node_means), prediction(x_right, is_terminal, node_splits, node_means)), lwd = 1.5, col = "black") 
  }
}

### PART 5 : Calculate RMSE for P = 15
y_hat <- sapply(X=1:N_test, FUN = function(n) prediction(x_test[n], is_terminal, node_splits, node_means))
RMSE <- sqrt(sum((y_test - y_hat) ^ 2) / length(y_test))
print(sprintf("RMSE is %g when P is %s", RMSE, P))


### PART 6 : Learn Decision Tree for P = 5 10 .. 50 and plot RMSE graph
P_values <- seq(5, 50, by = 5)

RMSE_for_P <- sapply(X=P_values, FUN = function(p) {
  d_tree <- DecisionTree(p)
  node_splits <- d_tree$node_splits 
  node_means <- d_tree$node_means 
  is_terminal <- d_tree$is_terminal
  y_test_predicted <- sapply(X=1:N_test, FUN = function(n) prediction(x_test[n], is_terminal, node_splits, node_means))
  RMSE <- sqrt(sum((y_test - y_test_predicted) ^ 2) / length(y_test))
})

plot(P_values, RMSE_for_P,
     type = "o", lwd = 1, las = 1.5, pch = 20, lty = "solid", 
     xlim = range(5, 50),xlab = "Pre-pruning size (P)", ylab = "RMSE")

