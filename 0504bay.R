sgd_logis <- function(X, y, w, alpha = 0.5, n.samples = 1, tol = 1e-5, max.iter = 1000) {
  n <- length(y)
  w_old <- w
  J <- ws <- list()
  sto.sample <- sample(1:n, n.samples, replace = TRUE)
  ws[[1]] <- w_old
  J[[1]] <- sgd_logis_cost(X, y, w_old)
  w_new <- w_old - alpha * sgd_logis_cost_grad(X[sto.sample, ], y[sto.sample], w_old, m = n)
  ws[[2]] <- w_new
  J[[2]] <- sgd_logis_cost(X, y, w_new)
  iter <- 0
  n.best <- 0
  while ((abs(sgd_logis_cost(X, y, w_new) - sgd_logis_cost(X, y, w_old))> tol) & (iter + 2 < max.iter)) {
    w_old <- w_new
    sto.sample <- sample(1:n, n.samples, replace = TRUE)
    w_new <- w_old - alpha * sgd_logis_cost_grad(X[sto.sample, ], y[sto.sample], w_old, m = n)
    iter <- iter + 1
    ws[[iter + 2]] <- w_new
    J[[iter + 2]] <- sgd_logis_cost(X, y, w_new)
  }
  if (abs(sgd_logis_cost(X, y, w_new) - sgd_logis_cost(X, y, w_old)) > tol) {
    cat("Did not converge. \n")
  } else {
    cat("Converged. \n")
    cat("Iterated ", iter + 1, " times.", "\n")
    cat("Coefficients: ", w_new, "\n")
    return(list(coef = ws, cost = J, niter = iter + 1))
  }
}
sgd_logis_cost <- function(X, y, w) {
  n <- ncol(X)
  if (!is.matrix(X)) {
    X <- matrix(X, nrow = 1)
  }
  p_x <- 1 + exp(w[1:n] %*% t(X)+w[n+1])
  loss <- -t(y) * (w[1:n] %*% t(X)+w[n+1]) + sum(log(p_x)) / length(p_x)
  return(loss)
}
sgd_logis_cost_grad <- function(X, y, w, m) {
  if (!is.matrix(X)) {
    X <- matrix(X, nrow = 1)
  }
  n <- ncol(X)
  p_x <- 1/(1 + exp(X * w[1:n]+w[n+1]))
  grad<- (X * p_x-(y * X))/m
  grad[3] <-sum(p_x-y)/m
  return(grad)
}

logi <- iris[1:100,1:4]
Species <- vector("integer",50)
p <- 1 + Species
Species[51:100] <- p
logi <- cbind(logi,Species)
logi_glm <- glm(Species~ Sepal.Length + Sepal.Width, data = logi, family = binomial(link = "logit"))
logi_glm

x <- logi[, 1:2]
X <- scale(x)
y <- as.matrix(logi[, 5])
logi_sgd <- sgd_logis(X, y, w = c(0, 0, 0),alpha = 0.5, tol = 1e-5, max.iter = 10000)