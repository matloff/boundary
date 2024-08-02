library(nloptr)

generate_linear_function <- function() {
    beta1 <- 100
    beta2 <- 200
    return(list(beta1 = beta1, beta2 = beta2))
}

generate_random_points <- function(n) {
    x1 <- runif(n, -100, 100)
    x2 <- runif(n, -100, 100)
    return(list(x1 = x1, x2 = x2))
}

add_random_noise <- function(n, sigma) {
    epsilon <- rnorm(n, 0, sigma)
    return(epsilon)
}

estimate_b <- function(X, y) {
    b <- solve(t(X) %*% X) %*% t(X) %*% y
    return(b)
}

calculate_Sigma <- function(X, y, b) {
    p <- ncol(X)
    n <- length(y)
    residual_variance <- sum((y - X %*% b)^2) / (n - p - 1)
    Sigma <- residual_variance * solve(t(X) %*% X)
    return(Sigma)
}

calculate_s2 <- function(X, y, b) {
    grad_g <- c(-1 / b[2], b[1] / (b[2]^2))
    Sigma <- calculate_Sigma(X, y, b)
    s2 <- t(grad_g) %*% Sigma %*% grad_g
    return(s2)
}

calculate_confidence_interval_by_delta_method <- function(X, y, b, confidence) {
    s2 <- calculate_s2(X, y, b)
    z_value <- qnorm((1 + confidence) / 2)
    confidence_interval <- c(-b[1] / b[2] - z_value * sqrt(s2), -b[1] / b[2] + z_value * sqrt(s2))
    return(confidence_interval)
}

# Solve for maximum of -Beta[1] / Beta[2] where (b - Beta)^T * Sigma * (b - Beta) <= q using Lagrange multipliers
calculate_confidence_interval_by_langrange_multipliers <- function(X, y, b, confidence) {
    Sigma <- calculate_Sigma(X, y, b)
    p <- ncol(X)
    n <- length(y)
    q <- qf(confidence, p + 1, n - p - 1)
    f_min <- function(beta) {
        return(-beta[1] / beta[2])
    }
    f_max <- function(beta) {
        return(beta[1] / beta[2])
    }
    g <- function(beta) {
        return(t(b - beta) %*% solve(Sigma) %*% (b - beta) - q)
    }
    lower <- ccsaq(b, f_min, hin = g, deprecatedBehavior = FALSE)$value
    upper <- -ccsaq(b, f_max, hin = g, deprecatedBehavior = FALSE)$value
    return(c(lower, upper))
}

check_accuracy <- function(beta1, beta2, confidence_interval) {
    if (-beta1 / beta2 >= confidence_interval[1] && -beta1 / beta2 <= confidence_interval[2]) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

plot_data <- function(X, y, b, confidence_interval, show_plane = FALSE) {
    library(plotly)

    # Plot plane
    x_axis <- seq(min(X[, 1]), max(X[, 1]), length.out = 100)
    y_axis <- seq(min(X[, 2]), max(X[, 2]), length.out = 50)
    plane <- outer(y_axis, x_axis, FUN = function(y, x) x * b[1] + y * b[2])
    plot <- NULL
    if (show_plane) {
        plot <- plot_ly(x = ~x_axis, y = ~y_axis, z = ~plane) %>% add_surface()
    }

    # Plot X, y
    if (is.null(plot)) {
        plot <- plot_ly(x = ~ X[, 1], y = ~ X[, 2], z = ~y, type = "scatter3d", mode = "markers")
    } else {
        plot <- add_trace(plot, x = ~ X[, 1], y = ~ X[, 2], z = ~y, type = "scatter3d", mode = "markers")
    }

    # Plot the zero line
    x_line <- c(min(X[, 1]), max(X[, 1]))
    plot_line <- function(plot, m) {
        y_line <- m * x_line
        z_line <- rep(0, length(x_line))
        return(add_trace(plot, x = ~x_line, y = ~y_line, z = ~z_line, type = "scatter3d", mode = "lines"))
    }
    plot <- plot_line(plot, -b[1] / b[2])

    # # Plot the confidence interval
    # print(confidence_interval)
    for (m in seq(from = confidence_interval[1], to = confidence_interval[2], length.out = 5)) {
        plot <- plot_line(plot, m)
    }

    plot <- layout(plot, scene = list(xaxis = list(title = "x1"), yaxis = list(title = "x2"), zaxis = list(title = "y")))
    print(plot)
}

calculate_accuracy_rates <- function(n_values, confidence, trials) {
    accuracy_rates <- list()
    for (n in n_values) {
        accurate <- 0
        for (i in 1:trials) {
            linear_function <- generate_linear_function()
            beta1 <- linear_function$beta1
            beta2 <- linear_function$beta2

            points <- generate_random_points(n)
            x1 <- points$x1
            x2 <- points$x2

            epsilon <- add_random_noise(n, sigma = 10)

            y <- beta1 * x1 + beta2 * x2 + epsilon
            X <- cbind(x1, x2)
            b <- estimate_b(X, y)


            y_pred <- b[1] * x1 + b[2] * x2

            # confidence_interval <- calculate_confidence_interval_by_delta_method(X, y, b, confidence)
            # print("Delta method: ")
            # print(confidence_interval)
            confidence_interval <- calculate_confidence_interval_by_langrange_multipliers(X, y, b, confidence)
            # print("Langrange multipliers method: ")
            # print(confidence_interval)
            # print("True: ")
            # print(-beta1 / beta2)

            # plot_data(X, y, b, confidence_interval)

            if (check_accuracy(beta1, beta2, confidence_interval)) {
                accurate <- accurate + 1
            }
        }
        accuracy_rates <- append(accuracy_rates, list(c(n, accurate / trials)))
    }
    return(accuracy_rates)
}

plot_accuracy_vs_points <- function(accuracy_rates) {
    n_values <- sapply(accuracy_rates, function(x) x[[1]])
    accuracy_values <- sapply(accuracy_rates, function(x) x[[2]])

    fit <- lm(accuracy_values ~ n_values)
    m <- coef(fit)[2]
    b <- coef(fit)[1]

    plot(n_values, accuracy_values, type = "l", xlab = "Number of Points", ylab = "Accuracy Rate")
    abline(a = b, b = m, col = "red")
}

confidence <- 0.95
n_values <- seq(10, 100001, 10000)
trials <- 100

# n_values <- seq(10, 12, 1)
# trials <- 1

accuracy_rates <- calculate_accuracy_rates(n_values, confidence, trials)
plot_accuracy_vs_points(accuracy_rates)
