# A function to print squares of a sequence of numbers up to the given number `a`.
squarenumbers <- function(a) {
   squares <- sapply(1:a, function(x) x^2)
}	

print(squarenumbers(5))-