# 1. Expression & Assignment

# an expression
2 + sqrt(4) + log(exp(2)) + 2^2

# assignment
x <- (pi == 3.14)
print(x)


# 2. Data Structure

# 2.1 Atomic vector

# create R vectors
vec_character <- c("Hello,", "World!")
vec_integer <- c(1L, 2L, 3L)
vec_double <- c(1.1, 2.2, 3.3, -4.4)
vec_logical <- c(TRUE, TRUE, FALSE)

# retrieve the first element of vec_double
print(vec_double[1])

# select multiple elements
print(vec_double[c(1, 3)])
print(vec_double[1:2])
print(vec_double[c(-1, -2)])
print(vec_double[c(TRUE, FALSE, TRUE, FALSE)])
print(vec_double[vec_double < 0])

# display structure of a vector
str(vec_character)
str(vec_integer)
str(vec_double)
str(vec_logical)

# display length of a vector
print(length(vec_double))

# Exercise - 2.1
# Define two vectors (1, 2, -3) and (4, -5, -6).
# Perform an element-wise multiplication, and sum up the positive elements in the resulting vector
# hint: 1. use operator `*` for element-wise multiplication;
# 2. use `sum()` function to sum up elements in a numeric vector).

# your code playground here


# 2.2 List
l1 <- list(
  1:3,
  "a",
  c(TRUE, FALSE, TRUE),
  c(2.3, 5.9),
  c(1L, 2L)
)

str(l1)

# list can contain list as well
l2 <- list(list(list(1)))
str(l2)

# Retrieving elements in a list
str(l1[1:2])
str(l1[c(1, 3)])

# Note that using [] always returns a list
# To return the element in a list as it is, use [[]].
# [[]] only returns one single element, so you can only specify a single index.
str(l1[[1]])

# Exercise - 2.2
# Retrieve the vector (2.3, 5.9) in the below list and sum up its elements.

# define a list
l_ex <- list(
  1:3,
  list(
    "a",
    c(2.3, 5.9)
  )
)

# your code below


# 2.3 Matrix (self-study)

# use the matrix() function to create a matrix
y <- matrix(1:6, nrow = 2, ncol = 3)
print(y)

# fina dimensin of a matrix
print(dim(y))

# Subsetting a matrix is similar to subsetting a vector
print(y[1:2, c(1,3)])
print(y[1:2, -2])

# Note that [] by default simplify the subsetting result to lowest possible dimension
# y[1, 1:2] gives a vector
str(y[1, 1:2])

# Matrix algebra is easy.
# See here for a list of R matrix operations, https://www.statmethods.net/advstats/matrix.html.

# define two matrics
m1 <- matrix(1:4, nrow = 2)
m2 <- matrix(5:8, nrow = 2)
print(m1)
print(m2)

# element-wise multiplication
print(m1 * m2)

# matrix multiplication
print(m1 %*% m2)

# transpose
print(t(m1))

# solve Ax = b problem
b <- matrix(7:8, nrow = 2)
print(b)
print(solve(m1, b))

# 2.4 Data frame

# A data frame is like a 2-D table in Excel
df1 <- data.frame(x = 1:3, y = letters[1:3], z = c(1.1, 2.2, 3.3))

print(df1)

str(df1)

# By default, data.frame() turn character vector into a factor variable (categorical variable).
# Use `stringsAsFactors = FALSE` to disable this behavior.

# use 'stringsAsFactors = FALSE' to keep strings as they are
df2 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
str(df2)

# It's often useful to find out column names and number of columns and rows.
# find out column names using names() or colnames()
print(names(df1))
print(colnames(df1))

# find out number of columns
print(length(df1))
print(ncol(df1))

# find out number of rows
print(nrow(df1))

# Subsetting a dataframe is similar to that of a list or matrix.

# select a single column using []
print(df1['x'])

# note the result of df1['x'] is still a dataframe
str(df1['x'])

# select a single column using [[]]
print(df1[['x']])

# note the result is NOT a dataframe any more. It's a vector.
str(df1[['x']])

# select multiple columns
print(df1[c('x', 'z')])

# select multiple columns
print(df1[1:2, c('x', 'z')])

# 3 Programming Structure (please refer to the notebook for details)

## 3.1 Three basic control flows

### 3.1.1 Sequence

# sum of squares
t <- 1:3
y <- sum(t^2)
print(y)

# 3.1.2 Selection / Conditional execution
if (y > 10) {
  print("result greater than 10")
} else {
  print("result less or equal to 10")
}

# 3.1.3 Iteration
t <- 1:3
y <- 0

for (x in t) {
  y <- y + x^2
}

print(y)

# Exercise - 3.1.3
# Calculate the above sum using a while loop
# your code goes here

#3.2 Functions

ss <- function(n) {
  t <- 1:n
  
  # the last expression is automatically returned
  # or otherwise, you could write return(sum(t^2))
  sum(t^2)
}

print(ss(2))
print(ss(3))

# Exercise - 3.2 (self-study)
# your code here

f_sum <- function(f, n) {
  
  # insert your code below
  
  return(y)
}

# if your code works as intended, the following should run

# define t squared as f1
f1 <- function(t) {
  t^2
}

print(f_sum(f1, 3L))

# let's calculate pi
# calculate pi using the pi formula
f_pi <- function(x) {
  4 * (-1) ^ (x + 1) / (2 * x - 1)
}

print(f_sum(f_pi, 10000L))

