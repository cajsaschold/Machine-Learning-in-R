setwd("~/Desktop/TDDE01/Lab2")

data = read.csv("communities.csv")
library(caret)
set.seed(12345)

#1.
#Scale all variables except of ViolentCrimesPerPop
data_without_target = data[,-101]
scaler=preProcess(data_without_target)
dataS=predict(scaler, data_without_target)

#implement PCA by using function eigen()
cor_matrix = cor(dataS) #the linear relationships between pairs of variables, gives a 100x100 matrix with 1 in the diagonal. High absolute values (close to 1 or -1) indicate a strong linear relationship, while values close to 0 indicate a weak or no linear relationship.
eigen = eigen(cor_matrix)
lambda = eigen$values #eigenvalues represent the amount of variance captured by each principal component. Larger eigenvalues correspond to principal components that capture more variance in the data.

#how many components needed to obtain at least 95% of variance in the data

#proportion of variance explained by each principal component is calculated by 
#dividing each eigenvalue by the sum of all eigenvalues.

total = sum(lambda)
variance = 0

for (i in 1:length(lambda)) {
  variance = lambda[i]/total + variance #FATTA DENNA
  if(variance>0.95) {
    nr_comp=i
    break
  }
}    
sprintf("Number of components needed to obtain 95%% of the variance in the data: %d", nr_comp)

#the proportion of variation explained by each of the first two principal components
PC1_var = lambda[1]/total
PC2_var = lambda[2]/total
#The fraction of variance explained by a principal component is the ratio between the variance of that principal component and the total variance.
sprintf("The proportion of variation explained by PC1: %f", PC1_var)
sprintf("The proportion of variation explained by PC2: %f", PC2_var)
################

#2. 

pca = princomp(dataS)
first = pca$loadings[,1]#PCA loadings are the coefficients of the linear combination of the original variables from which the principal components (PCs) are constructed
plot(first, main = "Trace plot", xlab="Index", ylab = "PC1")

#Report which 5 feastures contribute mostly
max_values = c()
for (i in 1:5) {
  max = which.max(abs(first))
  max_values = append(max_values, first[max])
  first[max] = NA
  i = i + 1
}
names(max_values)

#plot of the PC scores in the coordinates (PC1, PC2), color of the points by ViolentCrimesPerPop
pca_scores = data.frame(pca$scores)
library(ggplot2)

df = data.frame(x=pca_scores[,1],y=pca_scores[,2], target=data$ViolentCrimesPerPop) 

ggplot(df,aes(x=x,y=y))+
  geom_point(aes(color=target))+
  labs(color="crimes", x="Principle component 1",y="Principle component 2",title="PC Scores") 
##################

#3.

#Split original data into training and test (50/50), scale both features and response appropriately
set.seed(12345)
n=nrow(data)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]
library(caret)
scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test) 

#estimate a linear regression model from training data in which 
#ViolentCrimesPerPop is target and all other data columns are features

linear_model = lm(ViolentCrimesPerPop ~., trainS)

predicted_train = predict(linear_model, trainS)
mse_train = sum((trainS$ViolentCrimesPerPop - predicted_train)^2)/nrow(trainS)

predicted_test = predict(linear_model, testS)
mse_test = sum((testS$ViolentCrimesPerPop - predicted_test)^2)/nrow(testS)
#########################

#4. 

Fs_train=list()
Fs_test=list()

k=0
costfunc<- function(theta, train, test){
  ytrain = as.matrix(trainS$ViolentCrimesPerPop)
  Xtrain = as.matrix(trainS[,-101])
  
  ytest = as.matrix(testS$ViolentCrimesPerPop)
  Xtest = as.matrix(testS[,-101])
  
  #predicted values
  y_hat_train = Xtrain %*% theta
  cost_train = mean((ytrain - y_hat_train)^2)
  
  y_hat_test = Xtest %*% theta
  cost_test = mean((ytest - y_hat_test)^2)
  
  .GlobalEnv$k= .GlobalEnv$k+1 #+1 to keep track of nr of iterations
  .GlobalEnv$Fs_train[[k]]=cost_train #store f (function value) for training in Fs list
  .GlobalEnv$Fs_test[[k]]=cost_test #store f (function value) for test in Fs list
  
  return(cost_train)
}

#except intercept
n = ncol(trainS)-1 #Nr of columns except intercept
train_new = as.matrix(trainS[,1:n])
test_new = as.matrix(testS[,1:n])

res<-optim(rep(0,n), fn=costfunc, train = train_new, test = test_new, method="BFGS")

#train and test mse for the optimal model
opt_train_mse = res$value
opt_test_pred = as.matrix(testS[,-101]) %*% res$par
opt_test_mse = mean((as.matrix(testS$ViolentCrimesPerPop) - opt_test_pred)^2)

#remove initial 500 iterations
Fs_train[1:500] = c()
Fs_test[1:500] = c()

#training and test error in the optimal model
min_train = which.min(Fs_train)
min_test = which.min(Fs_test)

sprintf("Optimal number of iterations: %d", min_test)

plot(as.numeric(Fs_train), ylim = c(0,2), main="Optmization plot", xlab = "Iterations", ylab = "Errors", col="blue")
points(as.numeric(Fs_test), col = "red")
points(min_train, Fs_train[min_train], pch = 15, col = "green", cex = 1)
points(min_test, Fs_test[min_test], pch = 17, col = "green", cex = 1)
legend("topright", legend = c("Training error", "Test error", "Optimal train", "Optimal test"), col = c("blue", "red", "green", "green"), pch = c(1, 1, 15, 17))


