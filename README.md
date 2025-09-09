# Machine Learning and Statistical Modeling in R
This repository contains implementations of various machine learning and statistical modeling techniques using R. The scripts explore principal component analysis (PCA), linear and ridge regression, and Gaussian kernel smoothing for temperature prediction.

## Contents
### 1. Principal Component Analysis (PCA)
**File:** `PCA.R`  
**Description:** Implements Principal Component Analysis (PCA) to analyze a dataset related to crime statistics.  
**Key Methods:**  
- Eigen decomposition to determine principal components.
- Variance explained analysis to determine the number of components needed for 95% variance retention.
- Trace plots to visualize feature importance.
- Scatter plots of PC1 and PC2 colored by crime rates.
- Linear regression on transformed principal components.
### 2. Linear and Ridge Regression
**File:** `Linear_and_Ridge_Regression.R`
**Description:** Implements linear regression and ridge regression to analyze Parkinson’s disease symptom scores.  
**Key Methods:**  
- Splitting data into training and test sets.
- Linear regression for predicting motor_UPDRS from voice parameters.
- Log-likelihood function computation for parameter estimation.
- Ridge regression with different lambda values (1, 100, 1000) to observe regularization effects.
- Comparison of model performance using MSE and degrees of freedom.
### 3. Gaussian Kernel Smoothing for Temperature Prediction
**File:** `Gaussian_Kernel.R`  
**Description:** Implements Gaussian kernel smoothing to predict temperature based on geospatial and temporal factors.  
**Key Methods:**  
- Merging weather station data with temperature readings.
- Defining Gaussian kernels for distance, date, and time.
- Combining kernels (sum & multiplicative approaches) for prediction.
- Visualizing kernel smoothing effects on distance, time, and date.
- Plotting predicted temperatures over different times of the day.
### 4. Hidden Hidden Markov

### 5. Reinforcement Learning

### 6. Bayesian methods
