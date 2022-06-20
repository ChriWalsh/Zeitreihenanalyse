# Clearing workspace
rm(list = ls())

#### Exercise 1 ####

# a.) ----

# The time series needs to have a starting point... this could be random,
# but why not start with 0?
start_point <- 0

# Now we want to draw T observations from a AR(1) process
rar_1 <- function(num_obs, a=0.5, start_point=0) {
    # first draw all the noise
    # set.seed(1)
    rar <- rnorm(num_obs, 0, 1)

    # seems like we cannot quite avoid the for-loop
    for (i in 2:num_obs) {
        rar[i] <- a * rar[(i - 1)] + rar[i]
    }
    return(rar)
}

# Sanity check results
plot(rar_1(200), type = "l")
ar_matrix <- sapply(rep(200, 100), rar_1)
typeof(ar_matrix) # type of matrix is double != float with double bits
dim(ar_matrix) # 200x100 matrix
ar_matrix[1, ] # all the same for same seed

# b.) ----

acf_est <- function(h, x_vector) {
    num_obs <- length(x_vector)
    return(sum(x_vector[(1 + h):num_obs] * x_vector[1:(num_obs - h)]) / num_obs)
}

acf_hat <- function(h, ar_matrix) {
    return(apply(ar_matrix, c(2), function(x) acf_est(h, x)))
}

a <- acf_hat(1, ar_matrix) / acf_hat(0, ar_matrix)
plot(a)
mean(a)

# c.) ----
ar_200 <- sapply(rep(200, 100), rar_1)
ar_500 <- sapply(rep(500, 100), rar_1)
ar_1000 <- sapply(rep(1000, 100), rar_1)

a_200 <- acf_hat(1, ar_200) / acf_hat(0, ar_200)
a_500 <- acf_hat(1, ar_500) / acf_hat(0, ar_500)
a_1000 <- acf_hat(1, ar_1000) / acf_hat(0, ar_1000)

# More observations should lead to better results
plot(a_100, col = 1)
points(a_500, col = 2)
points(a_1000, col = 3)

# Mean should be closer
mean(a_200)
mean(a_500)
mean(a_1000)

# And Variance and MSE smaller
mean((a_200 - 0.5) ** 2)
mean((a_500 - 0.5) ** 2)
mean((a_1000 - 0.5) ** 2)


#### Exercise 2 ####

# a.) ----
rar_2 <- function(num_obs, a=c(-1, -0.25), start_point=0) {
    # set.seed(1)
    rar <- rnorm(num_obs, 0, 1)
    rar[2] <- a[1] * rar[1] + rar[2]
    for (i in 3:num_obs) {
        rar[i] <- a * rar[c(i - 1, i - 2)] + rar[i]
    }
    return(rar)
}

ar_2_matrix <- sapply(rep(500, 100), rar_2)

# b.) ----
acf2_hat <- function(h) acf_hat(h, ar_matrix = ar_2_matrix)

g0 <- acf2_hat(0)
g1 <- acf2_hat(1)
g2 <- acf2_hat(2)

# To needlessly complicate matters we are going to use arrays ----

# Gamma needs to be 100x2x2 tensor/array
gamma <- array(c(g0, g1, g1, g0), dim = c(100, 2, 2))
gamma_inv <- apply(gamma, c(1), solve)
dim(gamma_inv) # R ate the dimensions...
gamma_inv <- array(t(gamma_inv), dim = c(100, 2, 2))
gamma_inv[1, , ] == solve(gamma[1, , ]) # actually worked ...

g <- rbind(g1, g2)
dim(g)
dim(gamma_inv)

# Unfortunately don't know how the sweep() function works...
a <- matrix(, 100, 2)
for (i in 1:100) {
    # solve a = inverse(gamma) * g for all N
    a[i, ] <- gamma_inv[i, , ] %*% g[, i]
}

a[1, ]
mean(a[, 1]) # Wow, actually this seems to have worked!!
mean(a[, 2]) # Or not ...


# Array trick didn't work, so here the same by hand ----

a <- matrix(, 100, 2)
for (i in 1:100) {
    a[i, ] <- solve(matrix(c(g0[i], g1[i], g1[i], g0[i]),
                    2, 2)) %*% c(g1[i], g2[i])
}

mean(a[, 1])
mean(a[, 2]) # Same result must have been "correct" all along...
plot(a[, 1])
plot(a[, 2])


# c.) ----
num_obs <- dim(ar_2_matrix)[1]
num_obs
emp_errors <- (ar_2_matrix[3:num_obs, ]
             +  ar_2_matrix[2:(num_obs - 1), ] * a[, 1]
             +  ar_2_matrix[1:(num_obs - 2), ] * a[, 2])

emp_sigma_square <- apply(emp_errors, 2, mean)
length(emp_sigma_square)
plot(emp_sigma_square)

# These measures are much too small... 
# ...maybe this is where the accuracy for a2 was lost ...