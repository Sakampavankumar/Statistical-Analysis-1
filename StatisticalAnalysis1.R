#Mohammad Yasar Arshad

#Q1. Create a function that takes in a numeric vector. 
#The output should be a vector with running mean values. 
#The ith element of the output vector should be the 
#mean of the values in the input vector from 1 to i.

f = function(vec)
{
avg = vector("numeric", length = length(vec)) # Assigning Empty Vector
# Running a for loop to create running mean. There would be two loop. 
#Loop 1 will run from 1 to last element
#Loop 2 will keep on incrementing the average till next element
for (i in 1: length(vec))
{
  sum =0
  for (j in 1:i)
  {
    sum = sum + vec[j]
  }
  avg[i] = sum/i
}
return(avg)
}

#function 1 - Test Case
vec1 <- c(2,2,2,2,1)
f(vec1)

#function 2 - Test Case
vec2 <-c(3,2,4,6.6,7.7,1,4)
f(vec2)

 
#Q2Create a function that takes in a numeric vector. 
#The objective is to forecast using exponential smoothing. 
f = function(a)
{
alpha = 0.8 #setting the parameter
predicted <- vector("numeric", length = length(a)+1) #Empty vector
predicted[1] = a[1]#First predicted value is same as actual value
  for (i in 2:(length(a)+1))
  {
  predicted[i] = predicted[i-1] + alpha*(a[i-1]-predicted[i-1]) #Equation of forecast
  }
a[length(a)+1] = NA #Last Elemenet is Empty
data = cbind(a,predicted)
colnames(data) = c("Actual","Predicted")
return(data)
}

#Test Case 1
a1 <- c(1,3,4) 
f(a1)

#Test Case 2
a2 <- c(1,4,7,9)
f(a2)



# Q3 Create a function that takes in two integers (set default values of 1 to both). 
#The function should calculate the number of prime numbers between the two values. 

install.packages("schoolmath") # To calculate is.prime
library(schoolmath) 
f = function(a,b)
{
  count =0
  for(i in a:b) #loop from a to b
  {
    if (is.prim(i) == TRUE)
      {
      count = count +1
      #print(i)
      }
  }
  return(count)
}

#Test Case 1
f(1,20)
#Test Case 2
f(3,5)


#Q4 Simulate a function to roll a dice. Note that a dice 
#turns up with numbers 1, 2, 3, 4, 5 or 6. The function should do the following: 
#you roll the dice twice, and if both the numbers are the same 
#then return 'You Win' otherwise return 'You Lose'

f = function()
  {
  a = sample(c(1:6),2, replace = T) #Rolling dice 2 times - Simulation and assigning result to "a"
  ifelse(a[1]==a[2],"You Win", "You Lose")
  }
f()

#Q5 Create a function called Missing that takes in a data frame as 
#the input and outputs another data frame with column names, number 
#of missing values in each column, percentage of missing values in each 
#column, and the number of unique values in each column


Missing = function(a)
{
df <- data.frame(matrix(ncol = 4, nrow = ncol(a))) #Create and empty data frame
x <- c("Column_Name", "Missing_Values", "Percnt_Missing", "Unique_Values")
colnames(df) <- x #Assigning the colnames
df$Column_Name <- colnames(a) # Creating first Column of the Data Frame (Column_Name) with the column names of vector
for (i in 1:ncol(a))
{
df$Missing_Values[i] <- sum(is.na(a[,i])) #Calculate Missing Values
df$Percnt_Missing[i] <- mean(is.na(a[,i]))*100 # Calculate % Missing Values
df$Unique_Values[i] <- length(levels(as.factor(a[,i]))) #Calculate Unique Values
}
return(df)
}

#Test Case 1
library(MASS)
Missing(survey)

#Test Case 2

a <- data.frame (c1 = 1:8, c2 = factor (c("a", "b", "a", "c", "b", "c", "a", "b")))
a[4,1] <- a[6,2] <- a[5,1] <- NA 
Missing(a)
data("survey")


library(ggplot2)
str(survey)


length(unique(survey$Sex))
