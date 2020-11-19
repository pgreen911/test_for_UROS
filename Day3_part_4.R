# ****************************************
#                Day 3 part 4
# ****************************************


# R packages: practical applications for daily production (2)

## Functions (3)

# * Functions with empty input are possible but rare
# * Usually: element-wise application of the function
x <- 1:5; y <- 6:10; z <- -1:3; x + y + z # "+" is a function
exp(x=x) # application of the exp-function on each element of x
# * non-sense computations: `R` usually gives a _warning_
log(z)


## Functions (4)

# * When applying a function to more than one vector, the vectors must have the same length
# * If not: `–>` _Recycling_ of the shorter vector
x <- 1:6; y <- 1:2; x + y # y is repeated 3 times

# * Some functions return only one value
sum(x=1:6) # sum of the integers 1-6
prod(x=1:6) # product of integers 1-6


## Mathematical Functions (1)

# * The most important mathematical functions and operators are available:
#     + Basic arithmetic: `+`, `-`, `*`, `/`
#     + Modulo operator (remainder from division): `%%`
1:20 %% 3 # rest when dividing by 3

# * Absolute value with `abs()`, square root with `sqrt()`
abs(-5:5)
sqrt(-5:5)


## Mathematical Functions (2)

# * Logarithm with `log()`, exponential function with `exp()`:
log(x=1:5) # by default logarithm naturalis with base=exp(1)
log(x=1:5, base=2) # or analoguously: log2(x=1:5)
exp(1:5)

# * Trigonometric functions: `sin()`, `cos()`, `tan()` (`?Trig`)
cos(seq(from=0, to=90, by=10))


## Functions for Rounding (1)

# * Various functions to round non-integer vectors are available in `R`:
# * `round()`: Rounding vectors to desired number of decimal places (argument _digits_)
x <- exp(1:5); x
round(x, digits=3)
round(x, digits=2)
round(x, digits=1)


## Functions for Rounding (2)

# * If `digits=0` (the default), then rounding to integer
x <- exp(1:5); x
round(x) # digits=0

# * Attention: numbers are rounded to the next even number
round(c(1.5,2.5,3.5,4.5), digits=0)


## Functions for Rounding (3)

# * Rounding possible to multiples of 10 by a negative value for _digits_
x <- seq(from=-100, to=100, length=20); x
# * To multiples of 10 and 100
round(x, digits=-1) # multiples of 10
round(x, digits=-2) # multiples of 100


## Functions for Rounding (4)

# * Further functions for rounding
#     + `floor(x)`: rounding to the next integer less than x
#     + `ceiling(x)`: rounding to the next integer greater than x
#     + `trunc(x)`: rounding to the next integer towards 0
floor(c(-123.123, 123.123))
ceiling(c(-123.123, 123.123))
trunc(c(-123.123, 123.123))


## Probability functions (2)

# * E.g. normal distribution, XY=__norm__
# * Default values to the standard normal distribution
# + `rnorm()`: generate standard normally distributed random numbers
rnumb <- rnorm(n=10); rnumb # standard normal distribution

# + `dnorm(q)`: value of the density function of a normal distribution
dnorm(c(-2, 0, 2)) # standard normal distribution, symmetric


## Probability functions (3)

# * More information on normal distribution
# + `pnorm()`: value of the distribution function at a certain value of the random variable
pnorm(0) # prob. that a standard normally distributed random number <= 0

# * By setting the function argument `lower.tail=FALSE` you get the opposite probability

# prob that a standard normally distributed random number > 0
pnorm(0, lower.tail=F)


## Probability functions (4)

# * Yet more on normal distribution:
# + `qnorm()`: Calculates for a given probability the corresponding quantile
# + helpful e.g. for calculation of z-values
qnorm(0.95, mean=0, sd=1)
qnorm(0.975, mean=0, sd=1)
# * Interpretation: at x=1.64485 the distribution function of the standard normal distribution is equal to 0.95, at x=1.95996 its value is 0.975.

## Probability functions (5)

# * Similar procedure for other distributions
# * E.g. density function of a binomial distribution: `?dbinom`
# Probability for x, given a binomial distribution with n=20 and prob=0.8
dbinom(x=c(10,15,20), size=20, prob=0.8)

# * Random numbers from a binomial distribution `rbinom?`
# 10 random numbers from a binomial distribution with n=20 and prob=0.8
rbinom(n=10, size=20, prob=0.8)


## Writing functions (1)

# * Functions are an essential element in the `R` language
# * Allow to separate repetitive code
# * General syntax:
my_first_function <- function(input1, input2) {
  # R-code is within the curly brackets
  res <- input1 + input2
  # then the result is returned
  return(res)
}
# Call
my_first_function(input1=1:5, input2=5:1)

# * each function can return more than one (but an arbitrarily complex) object
# * either in `return()` or the last expression


## Writing functions (3)

# * A first (poor) function:
f1 <- function(v1, v2) {
  return(sum(v1+v2))
}
f1(v1=1:3, v2=2:5)

# * What if _v1_ and _v2_ have different length?
# * What if _v1_ or _v2_ are not numeric?
# * More meaningful default values possible?
  
## Writing functions (4)
  
# better
f2 <- function(v1=0, v2=1) {
  if (length(v1) != length(v2)) {
    return (NA) # return NA if different number of elements
  }
  if (!is.numeric(v1) | !is.numeric(v2)) {
    return(NA) # return NA if non-numeric vectors
  }
  return(sum(v1+v2))
}

# * Call
f2(v2=4:7, v1=1:3) # different length, sequence of arguments!
f2(v1=1:3, v2=c("a", "b", "c")) # not all inputs are numeric
f2() # use the default values


## Writing functions (5)

# * Local versus global variables (scoping)
x <- 10
fn1 <- function(y=3) {
  y + x # value of this statement is returned
}
fn2 <- function(y=3, x=5) {
  y + x # value of this statement is returned
}
c(fn1(), fn2()) # call with default values

# * no local variable x exists in _fn1()_
# * `->` global x (with value 10) is used
# * local variable x in _fn2()_ is used
# * frequent source of error when creating own functions

## Writing functions (6)

# __Rounding__
# Various sorts of rounding (rounding up, rounding down, rounding to the nearest integer)

# rounding down, the ‘greatest integer less than’ function is floor
floor(5.7)
# rounding up, the ‘next integer’ function is ceiling
ceiling(5.7)
# rounding to the nearest integer by adding 0.5 to the number then using floor
rounded <- function(x) floor(x+0.5)
rounded(5.7)
rounded(5.4)


# How to avoid slow R code


## Create a large data frame
  
col1 <- runif (12^5, 0, 2)
col2 <- rnorm (12^5, 0, 2)
col3 <- rpois (12^5, 3)
col4 <- rchisq (12^5, 2)
df <- data.frame (col1, col2, col3, col4)

# The logic we are about to optimise:
  
# * For every row on this data frame (df), check if the sum of all values is greater than 4. If it is, a new 5th variable gets the value “greater_than_4”, else, it gets “lesser_than_4”.


## Original R code: Before vectorization and pre-allocation

system.time({
  for (i in 1:nrow(df)) { # for every row
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) { # check if > 4
      df[i, 5] <- "greater_than_4" # assign 5th column
    } else {
      df[i, 5] <- "lesser_than_4" # assign 5th column
    }
  }
})
rm(df)

# All the computations below, for processing times, were done on a Windows 10 with i5 1.6 Ghz processor and 6GB RAM.


## Vectorise and pre-allocate data structures

df <- data.frame (col1, col2, col3, col4)
# after vectorization and pre-allocation
output <- character (nrow(df)) # initialize output vector
system.time({
  for (i in 1:nrow(df)) {
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) {
      output[i] <- "greater_than_4"
    } else {
      output[i] <- "lesser_than_4"
    }
  }
  df$output})


## Take statements that check for conditions (if statements) outside the loop

df <- data.frame (col1, col2, col3, col4)
# after vectorization and pre-allocation, taking the condition checking outside the loop.
output <- character (nrow(df))
condition <- (df$col1 + df$col2 + df$col3 + df$col4) > 4  # condition check outside the loop
system.time({
  for (i in 1:nrow(df)) {
    if (condition[i]) {
      output[i] <- "greater_than_4"
    } else {
      output[i] <- "lesser_than_4"
    }
  }
  df$output <- output
})
rm(df)


## Run the loop only for True conditions

df <- data.frame (col1, col2, col3, col4)
output <- as.character(rep("lesser_than_4", nrow(df)))
condition <- (df$col1 + df$col2 + df$col3 + df$col4) > 4
system.time({
  for (i in (1:nrow(df))[condition]) {  # run loop only for true conditions
    if (condition[i]) {
      output[i] <- "greater_than_4"
    } else {
      # output[i] <- "lesser_than_4" # is not necessary any more
    }
  }
  df$output <- output })
rm(df)


## Use ifelse() whenever possible

df <- data.frame (col1, col2, col3, col4)
system.time({
  output <- ifelse ((df$col1 + df$col2 + df$col3 + df$col4) > 4, "greater_than_4", "lesser_than_4")
  df$output <- output
})
rm(df)


## Using which()

df <- data.frame (col1, col2, col3, col4)
# Thanks to Gabe Becker
system.time({
  want <- which(rowSums(df) > 4)
  output <- rep("less than 4", times = nrow(df))
  output[want] <- "greater than 4"
  df$output <- output
}) 
rm(df)


## Use apply family of functions instead of for-loops

df <- data.frame (col1, col2, col3, col4)
# apply family
system.time({
  myfunc <- function(x) {
    if ((x['col1'] + x['col2'] + x['col3'] + x['col4']) > 4) {
      "greater_than_4"
    } else {
      "lesser_than_4"
    }
  }
  output <- apply(df[, c(1:4)], 1, FUN=myfunc)  # apply 'myfunc' on every row
  df$output <- output
})
rm(df)


## Use byte code compilation

df <- data.frame (col1, col2, col3, col4)
# byte code compilation
library(compiler)
myFuncCmp <- cmpfun(myfunc)
system.time({
  output <- apply(df[, c (1:4)], 1, FUN=myFuncCmp)
  df$output <- output
})
rm(df)


## Use Rcpp

df <- data.frame (col1, col2, col3, col4)
library(Rcpp)
sourceCpp("MyFunc.cpp")
system.time({
  output <- myFunc(df) # see Rcpp function below
  df$output <- output
})
rm(df)


## Source for MyFunc.cpp

# // Source for MyFunc.cpp
# #include <Rcpp.h>
# using namespace Rcpp;
# // [[Rcpp::export]]
# CharacterVector myFunc(DataFrame x) {
#   NumericVector col1 = as(x["col1"]);
#   NumericVector col2 = as(x["col2"]);
#   NumericVector col3 = as(x["col3"]);
#   NumericVector col4 = as(x["col4"]);
#   int n = col1.size();
#   CharacterVector out(n);
#   for (int i=0;i <= n; ++i){
#     if(col1[i] + col2[i] + col3[i] + col4[i] > 4){
#       out[i] = "greater_than_4";
#     } else {
#       out[i] = "lesser_than_4";
#     }
#   }
#   return out;
# }


## Use parallel processing if you have a multicore machine

df <- data.frame (col1, col2, col3, col4)
# parallel processing
library(foreach)
library(doSNOW)
cl <- makeCluster(2, type="SOCK") # for 2 cores machine
registerDoSNOW (cl)
condition <- (df$col1 + df$col2 + df$col3 + df$col4) > 4
# parallelization with vectorization
system.time({
  output <- foreach(i = 1:nrow(df), .combine=c) %dopar% {
    if (condition[i]) {
      return("greater_than_4")
    } else {
      return("lesser_than_4")
    }
  }
  df$output <- output
})

rm(df)


## Use data structures that consume lesser memory (1)

df <- data.frame (col1, col2, col3, col4)
dt <- data.table(df)  # create the data.table
system.time({
  for (i in 1:nrow (dt)) {
    if ((dt[i, col1] + dt[i, col2] + dt[i, col3] + dt[i, col4]) > 4) {
      dt[i, col5:="greater_than_4"]  # assign the output as 5th column
    } else {
      dt[i, col5:="lesser_than_4"]  # assign the output as 5th column
    }
  }
})
rm(df,dt)


## Use data structures that consume lesser memory (2)

df <- data.frame (col1, col2, col3, col4)
setDT(df) # create the data.table
system.time({
  df[, output := "greater_than_4"]
  df[(col1 + col2 + col3 + col4) <= 4, output := "less_than_4"]
})
rm(df)


