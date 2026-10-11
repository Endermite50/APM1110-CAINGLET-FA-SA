
# Section I - Item 1

# Input x values
repeat {
  x1 <- as.numeric(readline("Enter x1 (0.10 to 0.40): "))
  x2 <- as.numeric(readline("Enter x2 (0.10 to 0.40): "))
  x3 <- as.numeric(readline("Enter x3 (0.10 to 0.40): "))
  
  x <- c(x1, x2, x3)
  
  if (!anyNA(x) && all(x >= 0.10 & x <= 0.40) &&
      abs(sum(x) - 1) < 0.000001) {
    break
  }
  
  if (anyNA(x)) {
    message("Void input: Please enter numbers only.")
  }
  
  if (any(x < 0.10 | x > 0.40, na.rm = TRUE)) {
    message("Void input: Each x-value must be between 0.10 and 0.40.")
  }
  
  if (!anyNA(x) && abs(sum(x) - 1) >= 0.000001) {
    message("Invalid total: x1 + x2 + x3 must equal 1.")
    message("Your current total is ", sum(x))
  }
}

# Input y values
repeat {
  y1 <- as.numeric(readline("Enter y1 (0.01 to 0.05): "))
  y2 <- as.numeric(readline("Enter y2 (0.01 to 0.05): "))
  y3 <- as.numeric(readline("Enter y3 (0.01 to 0.05): "))
  
  y <- c(y1, y2, y3)
  
  if (!anyNA(y) && all(y >= 0.01 & y <= 0.05) &&
      abs(sum(y) - 0.12) < 0.000001) {
    break
  }
  
  if (anyNA(y)) {
    message("Void input: Please enter numbers only.")
  }
  
  if (any(y < 0.01 | y > 0.05, na.rm = TRUE)) {
    message("Void input: Each y-value must be between 0.01 and 0.05.")
  }
  
  if (!anyNA(y) && abs(sum(y) - 0.12) >= 0.000001) {
    message("Invalid total: y1 + y2 + y3 must equal 0.12.")
    message("Your current total is ", sum(y))
  }
}


defective_probability <- (sum(x * y)) * 100
defective_probability