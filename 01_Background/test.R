possion <- function(n, lambda = 7) {
  lambda^n/factorial(n)*exp(-lambda)
}
