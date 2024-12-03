# Function to generate the Fibonacci sequence starting from 1 and 1
fibonacci <- function(n) {
  if (n <= 0) {
    return(numeric(0))  # Return an empty vector for non-positive n
  }
  fib <- numeric(n)
  fib[1] <- 1  # Starting with 1

  if (n >= 2) {
    fib[2] <- 1
  }
  if (n >= 3) {
    for (i in 3:n) {
      fib[i] <- fib[i - 1] + fib[i - 2]
    }
  }
  return(fib)
}

# Caller function to print the Fibonacci sequence
print_fibonacci <- function(n) {
  fib_sequence <- fibonacci(n)
  print(fib_sequence)
}

print_fibonacci(10)


# Sorry, our program would not allow for recursion using the libraries. So we needed a side thing