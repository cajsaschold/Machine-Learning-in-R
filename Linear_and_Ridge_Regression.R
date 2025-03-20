setwd("~/Desktop/TDDE01/Lab1")

#1

#import data
data = read.csv("parkinsons.csv")

#devide into training and test data (60/40) and scale it
set.seed(12345)
n=nrow(data)
id=sample(1:n, floor(n*0.6))
train=data[id,]
test=data[-id,]
library(caret)
scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test)
#########################################

#2

#compute a linear regression model from the training data
#response: motor_UPDRS (Parkinson's disease symptom score)
#predictors: Jitter%, Shimmer%, NHR, HNR, RPDE, DFA, PPE
fit=lm(motor_UPDRS ~ Jitter... + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE, trainS)
output_lm = summary(fit)
output_lm

#estimate training MSE (mean square error)
predicted_train = predict(fit, trainS)
mse_train = sum((trainS$motor_UPDRS - predicted_train)^2)/nrow(trainS)

#estimate test MSE (mean square error)
predicted_test = predict(fit, testS)
mse_test = sum((testS$motor_UPDRS - predicted_test)^2)/nrow(testS)
##########################################

#3

#implement loglikelihood function and compute the log-likelihood function 
#for the stated model and the training data

theta = output_lm$coefficients[2:17, 1]#a parameter vector
sigma = output_lm$sigma #the standard deviation

loglikelihood_func <- function(input) {
  y = as.matrix(trainS$motor_UPDRS) #dependent variable
  X = as.matrix(trainS[,7:22]) #independent variables
  
  theta = input[1:16]
  sigma = input[17]
  n = nrow(trainS)
  
  #calculate the predicted values (ŷ) based on the parameter 
  #vector (θ) and the predictor variables (X) 
  y_hat = X %*% theta
  
  #calculate the log-likelihood based on the observed y and the predicted
  #y_hat and the fixed dispersion (sigma^2) for each data point:
  log_likelihood = -n / 2 * log(2 * pi * sigma^2) - sum((y - X %*% theta)^2) / (2 * sigma^2)
  
  return(log_likelihood)
}

#a ridge function where lambda is the ridge parameter and the ridge penalty is𝜆‖𝜽‖^2 
ridge_func <- function(par, lambda) {
  theta = par[1:16]
  ridge_penalty = lambda*sum(as.matrix(theta)^2)
  
  ridge = sum(ridge_penalty - loglikelihood_func(par))#sum because optim needs a scalar
  return(ridge)
  }

#a ridgeOpt function that depends on scalar lambda
ridgeOpt_func <- function(lambda) {
  
  result = optim(
    par=rep(1,17), 
    fn = ridge_func, 
    lambda = lambda,
    method="BFGS")
  
  return(result$par[1:16]) #par = the best set of parameters found
  
  }

#a df_function that computes the degrees of freedom
#df for linear regression is the sum of the diagonal in the hat matrix
df_func <- function(lambda) {
  
  p = ncol(X) #X is an n by p matrix
  Ip = diag(p) #the identity matrix of size pxp
  
  XtransX = t(X)%*%X
  inv_XtransX = solve(XtransX + lambda*Ip) #gets the inverse
  hat_matrix = X %*% inv_XtransX %*% t(X)
    
  df = sum(diag(hat_matrix))
  return(df)
}
##########################################################

#4.

#compute optimal theta for lambda=1, lambda=100 & lambda=1000 using RidgeOpt

theta1 = ridgeOpt_func(1)
theta100 = ridgeOpt_func(100)
theta1000 = ridgeOpt_func(1000)

#train mse
y = as.matrix(trainS$motor_UPDRS) #dependent variable
X = as.matrix(trainS[,7:22]) #independent variables

y_hat1_train = X %*% theta1
y_hat100_train = X %*% theta100
y_hat1000_train = X %*% theta1000

train_mse1 = sum((y - y_hat1_train)^2)/nrow(trainS)
train_mse100 = sum((y - y_hat100_train)^2)/nrow(trainS)
train_mse1000 = sum((y - y_hat1000_train)^2)/nrow(trainS)

#test mse
y_test = as.matrix(testS$motor_UPDRS) #dependent variable
X_test = as.matrix(testS[,7:22]) #independent variables

y_hat1_test = X_test %*% theta1
y_hat100_test = X_test %*% theta100
y_hat1000_test = X_test %*% theta1000

test_mse1 = sum((y_test - y_hat1_test)^2)/nrow(testS)
test_mse100 = sum((y_test - y_hat100_test)^2)/nrow(testS) 
test_mse1000 = sum((y_test - y_hat1000_test)^2)/nrow(testS)

#lambda=100 is the best penalty parameter

#Compute and compare the degrees of freedom of these models 
df1 = df_func(1)
df100 = df_func(100)
df1000 = df_func(1000)



#printing as a table
train_mse = c(train_mse1, train_mse100, train_mse1000)
test_mse = c(test_mse1, test_mse100, test_mse1000)
degrees_of_freedom = c(df1, df100, df1000)

result_table = data.frame(
  Lambda = c(1, 100, 1000),
  TrainMSE = train_mse,
  TestMSE = test_mse,
  df = degrees_of_freedom)

print(result_table)
