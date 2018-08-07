x <- "Hello World"
y = "Hello World"
"Hello World" -> z
z
print(x)

#logical
l <- TRUE
#intLONG
i <- 123L
#numeric
n <- 1.23
#string
c <- "ABCD"
#date
d <- as.Date("2015-07-04")

#function
f <- function(x) { x + 1 }

f(2)

#vector
v <- c(1, 2, 3);

#sequence
s <- 1:5

#matrix
m <- matrix(
    data = 1:6,
    nrow = 2,
    ncol = 3
)

#array
a <- array(
    data = 1:8,
    dim = c(2, 2, 2)
)

#list (Diff types)
l <- list(TRUE, 123L, 2.34, "abc")
l

#Factor (Enumeration)
categories <- c("Male", "Female", "Male", "Male", "Female")
factor <- factor(categories)
factor
levels(factor)
unclass(factor)


#DATA FRAME (TABLE - cols same data type)
df <- data.frame(
    Name = c("CAT", "DOG", "COW", "PIG"),
    HowMany = c(5, 10, 15, 20),
    IsPet = c(TRUE, TRUE, FALSE, FALSE)
  )
df
#Data Frame indexing df[row, col]
df[1, 2]
df[1, ]
df[ , 2]
df$HowMany

#subset of dataframe
df[c(2, 4), ] #row 2, 4
df[c(TRUE, FALSE, TRUE, FALSE), ]
df[df$IsPet == TRUE, ]
df[df$HowMany > 10, ]
df[df$Name %in% c("CAT", "COW"), ]

#Vectorized Language
1 + 2
c(1, 2, 3) + c(2, 4, 6)

#Named vs. ordered arguments
m <- matrix(data = 1:6, nrow = 2, ncol = 3)
n <- matrix(1:6, 2, 3)

m == n
identical(m, n)

