# ENGR 421 Homework 3
# Harun Sasmaz
# 59900
library(MASS)

data_set <- read.csv("hw02_data_set_images.csv",header = FALSE)

safelog <- function(x) {
  return (log(x + 1e-100))
}

### PART 3
A_train <- data_set[1:25,]
B_train <- data_set[40:64,]
C_train <- data_set[79:103,]
D_train <- data_set[118:142,]
E_train <- data_set[157:181,]

A_test <- data_set[26:39,]
B_test <- data_set[65:78,]
C_test <- data_set[104:117,]
D_test <- data_set[143:156,]
E_test <- data_set[182:195,]

train_data <- rbind(A_train,B_train,C_train,D_train,E_train)
test_data <- rbind(A_test,B_test,C_test,D_test,E_test)

train_data <- data.matrix(train_data)
N <- NROW(train_data)
K <- 5

### PART 4
row_for_each <- NROW(A_train)
prior_prob_each <- row_for_each / N

priors <- c(rep(prior_prob_each, K))

#print(priors)

sum_a <- colSums(A_train)/row_for_each
sum_b <- colSums(B_train)/row_for_each
sum_c <- colSums(C_train)/row_for_each
sum_d <- colSums(D_train)/row_for_each
sum_e <- colSums(E_train)/row_for_each

pcd <- data.matrix(cbind(sum_a,sum_b,sum_c,sum_d,sum_e))

#print(pcd[,1])
#print(pcd[,2])
#print(pcd[,3])
#print(pcd[,4])
#print(pcd[,5])


### PART 5
height <- seq(0, 16, by = 1)
width <- seq(0, 20, by = 1)
A <- array(c(pcd[,1]), dim=c(20,16))
A <- t(apply(A, 2, rev))
image(height, width, A, col=gray.colors(n = 100, start = 0, end = 1,alpha=1, gamma = 1, rev = TRUE), useRaster = TRUE)

B <- array(c(pcd[,2]), dim=c(20,16))
B <- t(apply(B, 2, rev))
image(height, width, B, col=gray.colors(n = 100, start = 0, end = 1,alpha=1, gamma = 1, rev = TRUE), useRaster = TRUE)

C <- array(c(pcd[,3]), dim=c(20,16))
C <- t(apply(C, 2, rev))
image(height, width, C, col=gray.colors(n = 100, start = 0, end = 1,alpha=1, gamma = 1, rev = TRUE), useRaster = TRUE)

D <- array(c(pcd[,4]), dim=c(20,16))
D <- t(apply(D, 2, rev))
image(height, width, D, col=gray.colors(n = 100, start = 0, end = 1,alpha=1, gamma = 1, rev = TRUE), useRaster = TRUE)

E <- array(c(pcd[,5]), dim=c(20,16))
E <- t(apply(E, 2, rev))
image(height, width, E, col=gray.colors(n = 100, start = 0, end = 1,alpha=1, gamma = 1, rev = TRUE), useRaster = TRUE)

### PART 6
score_A <- sapply(X = 1:N, FUN = function(c) {train_data[c,]%*%safelog(pcd[,1])+(1-train_data[c,])%*%safelog(1-pcd[,1])+safelog(priors[1])})
score_B <- sapply(X = 1:N, FUN = function(c) {train_data[c,]%*%safelog(pcd[,2])+(1-train_data[c,])%*%safelog(1-pcd[,2])+safelog(priors[2])})
score_C <- sapply(X = 1:N, FUN = function(c) {train_data[c,]%*%safelog(pcd[,3])+(1-train_data[c,])%*%safelog(1-pcd[,3])+safelog(priors[3])})
score_D <- sapply(X = 1:N, FUN = function(c) {train_data[c,]%*%safelog(pcd[,4])+(1-train_data[c,])%*%safelog(1-pcd[,4])+safelog(priors[4])})
score_E <- sapply(X = 1:N, FUN = function(c) {train_data[c,]%*%safelog(pcd[,5])+(1-train_data[c,])%*%safelog(1-pcd[,5])+safelog(priors[5])})
score_all <- rbind(score_A, score_B, score_C, score_D, score_E)

y_pred <- sapply(X=1:N, FUN = function(c) {match(max(score_all[,c]),score_all)})
y_pred <- (y_pred - 1) %% K + 1

conf_A <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred[1:25],c)))})
conf_B <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred[26:50],c)))})
conf_C <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred[51:75],c)))})
conf_D <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred[76:100],c)))})
conf_E <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred[101:125],c)))})
confusion_matrix_train <- t(rbind(conf_A,conf_B,conf_C,conf_D,conf_E))
print(confusion_matrix_train)


### PART 7
test_data <- data.matrix(test_data)
N <- NROW(test_data)

score_A_test <- sapply(X = 1:N, FUN = function(c) {test_data[c,]%*%safelog(pcd[,1])+(1-test_data[c,])%*%safelog(1-pcd[,1])+safelog(priors[1])})
score_B_test <- sapply(X = 1:N, FUN = function(c) {test_data[c,]%*%safelog(pcd[,2])+(1-test_data[c,])%*%safelog(1-pcd[,2])+safelog(priors[2])})
score_C_test <- sapply(X = 1:N, FUN = function(c) {test_data[c,]%*%safelog(pcd[,3])+(1-test_data[c,])%*%safelog(1-pcd[,3])+safelog(priors[3])})
score_D_test <- sapply(X = 1:N, FUN = function(c) {test_data[c,]%*%safelog(pcd[,4])+(1-test_data[c,])%*%safelog(1-pcd[,4])+safelog(priors[4])})
score_E_test <- sapply(X = 1:N, FUN = function(c) {test_data[c,]%*%safelog(pcd[,5])+(1-test_data[c,])%*%safelog(1-pcd[,5])+safelog(priors[5])})
score_all_test <- rbind(score_A_test, score_B_test, score_C_test, score_D_test, score_E_test)

y_pred_test <- sapply(X=1:N, FUN = function(c) {match(max(score_all_test[,c]),score_all_test)})
y_pred_test <- (y_pred_test - 1) %% K + 1

conf_A_test <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred_test[1:14],c)))})
conf_B_test <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred_test[15:28],c)))})
conf_C_test <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred_test[29:42],c)))})
conf_D_test <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred_test[43:56],c)))})
conf_E_test <- sapply(X=1:K, FUN=function(c) {sum(!is.na(match(y_pred_test[57:70],c)))})
confusion_matrix_test <- t(rbind(conf_A_test,conf_B_test,conf_C_test,conf_D_test,conf_E_test))
print(confusion_matrix_test)

