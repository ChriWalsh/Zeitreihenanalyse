# Solving problemset 3 in the course 'Zeitreihenanalyse'

# Clearing the workspace to keep professor happy...
rm(list = ls())

#### Exercise 1 ####

gen_moving_average <- function(q, num_t = 200) {
    # Gaussian White Noise is i.i.d. ~N(0,1)
    # Idea: We need observations+q shocks
    set.seed(1)
    epsilon <- rnorm((num_t + q))
    x <- rep(NULL, num_t)
    for (i in 1:num_t) {
        # Is there a vector matrix operation to replace this for-loop?
        x[i] <- sum(epsilon[i:(i + q)])
    }
    return(x)
}

# Writing function to plot the thing:
plot_ma <- function(q, colour="blue", num_t = 200) {
    t_list <- 1:num_t
    x_t <- gen_moving_average(q, num_t)
    plot(t_list, x_t,
        main = paste0("MA(", q, ")-process"),
        ylab = "x_t",
        xlab = "t",
        type = "l",
        col = colour
        )
}

# Building the plot:
par(mfrow = c(3, 1))
plot_ma(5, "red")
plot_ma(10, "green")
plot_ma(50, "blue")
# Note the difference in scale of x_t ... random?


#### Exercise 2 ####

gen_ar1 <- function(a, num_t, tolerance = 0.0001) {
    # AR(1) has MA('inf') representation
    # Cannot draw 'inf' epsilon => find number n
    # s.t. a**n <= tolerance => n = ceiling(log_a(tolerance))
    # as |a| < 1
    n <- ceiling(logb(tolerance, base = abs(a)))
    set.seed(1)
    epsilon_past <- rnorm(n)
    epsilon_future <- rnorm(num_t)
    a_vec <- vector()
    for (i in 1:n) {
        a_vec[i] <- a**i
    }
    x0 <- sum(a_vec * epsilon_past) # x0 (t=-1) is dotproduct of epsilons
    x_vec <- rep(x0, num_t) + epsilon_future
    return(x_vec)
}

# Copied function from above, should really make it more general...
plot_fct <- function(fct, ..., # ... are the inputparameteres for the function
                    colour="blue",
                    main = "Plot of Function:") {

    x_t <- fct(...)
    num_t <- length(x_t)
    t_list <- 1:num_t
    plot(t_list, x_t,
        main = main,
        ylab = "x_t",
        xlab = "t",
        type = "l",
        col = colour
        )
}
a_list <- c(-0.95, -0.5, 0.5, 0.95)
num_t <- 200
par(mfrow = c(length(a_list), 1))
for (a in a_list) {
    plot_fct(gen_ar1, a, num_t,
    main = paste0("AR(1)-Process with a=", a))
}
# Note: The scale changes! Meaning the variance of {x_t} changes

#### Exercise 3 ####

acf_ar1 <- function(a, h_list, sigma=1) {
    acf <- vector()
    # Very ineligant for-loops because of R-syntax
    for (i in seq_along(h_list)) {
        h <- h_list[i]
        acf[i] <- (a**abs(h)) * (sigma**2 / ((1 - a)**2))
    }
    return(acf)
}

h_list <- 0:100
a_list <- c(-0.95, -0.5, 0.5, 0.95)

par(mfrow = c(length(a_list), 1))
for (a in a_list) {
    acf_h <- acf_ar1(a, h_list)
    plot(h_list, acf_h,
        main = paste0("ACF of AR(1) with a=", a),
        ylab = "afc(h)",
        xlab = "h",
        type = "h",
        col = "blue"
        )
}

#### Exercise 4 ####

# For causal ARMA(1,1) Processes:
# A(L)X_t = B(L)eps_t <=> X_t = B(L)/A(L) eps_t =:A(L)C(L)eps_t
# with C(L) = (c0 + c1 * L + c2 * L**2 + ...)
a <- -0.75
b <- 0.5

# First "dry":
c0 <- 1
c1 <- a * c0 + b
c2 <- a * c1
# from c1 on there is a AR(1) representation
c_j <- function(j, a, b) {
    c0 <- 1
    c1 <- c0 * a + b
    return(if (j != 0) a** (j - 1) * c1 else c0)
}
c_j(j = 0, a, b)

n <- 0:100
c_plus <- lapply(n, function(j) c_j(j, a = -0.75, b))
c_minus <- lapply(n, function(j) c_j(j, a = 0.75, b))

# Plotting the result:
par(mfrow = c(2, 1))
plot(n, c_plus, type = "l")
plot(n, c_minus, type = "l")
# The result seems surprising, is it correct?


#### Exercise 5 ####
# acf_arma11 <- function(h, a, b, sigma, tolerance = 0.0001) {
#     print("This specification has a bug!")
#     c_values <- vector()
#     i <- 0
#     while (TRUE) {
#         c_value <- c_j(i, a, b) * c_j(i + h, a, b)
#         if (c_value < tolerance) {
#             break
#         }
#         c_values[i + 1] <- c_value
#         i <- i + 1
#     }
#     return(sigma**2 * sum(c_values))
# }

# acf_arma11(0, -0.75, 0.5, 1)
# acf_arma11(1, -0.75, 0.5, 1) # Should be <0
# acf_arma11(2, -0.75, 0.5, 1)
# acf_arma11(3, -0.75, 0.5, 1) # Should be <0
# acf_many <- lapply(0:100, function(h) acf_arma11(h, -0.75, 0.5, 1))
# acf_many

# Stealing from formular/python spec:
acf_arma11 <- function(h, a, b, sigma, tolerance = 0.0001) {
    return(sigma**2 * a** (h - 1) * (b + a + (b + a)**2 * a / (1 - a**2)))
}

# Assigning values
n <- 0:100
acf_plus <- lapply(n,
            function(h) acf_arma11(h, a = 0.75, b = 0.25, sigma = 1))
acf_minus <- lapply(n,
            function(h) acf_arma11(h, a = -0.75, b = 0.25, sigma = 1))


par(mfrow = c(2, 1))
plot(n, acf_plus, type = "l")
plot(n, acf_minus, type = "l")

