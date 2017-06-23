# Statistical-Analysis-1
#Q1. Create a function that takes in a numeric vector. The output should be a vector with running mean values. The ith element of the output vector should be the mean of the values in the input vector from 1 to i.

#Q2. Create a function that takes in a numeric vector. The objective is to forecast using exponential smoothing. The formula is (𝑌𝑡 is the actual value and 𝑌̂𝑡 is the forecasted value) -
𝑌̂𝑡+1=𝑌̂𝑡+𝛼(𝑌𝑡− 𝑌̂𝑡)
The output of the function should be a dataframe with two columns – actual and predicted values. Set a default value of 0.8 for 𝛼.
Note: for the first row, the predicted and the actual values are same.

#Q3. There is a package with a function that enables you to check if a number is prime. install.packages("schoolmath") library(schoolmath) is.prim(3)
Create a function that takes in two integers (set default values of 1 to both). The function should calculate the number of prime numbers between the two values.

#Q4.Simulate a function to roll a dice. Note that a dice turns up with numbers 1, 2, 3, 4, 5 or 6. The function should do the following: you roll the dice twice, and if both the numbers are the same then return ‘You Win’ otherwise return ‘You Lose’ 

#Q5. Create a function called Missing that takes in a data frame as the input and outputs another data frame with column names, number of missing values in each column, percentage of missing values in each column, and the number of unique values in each column.

