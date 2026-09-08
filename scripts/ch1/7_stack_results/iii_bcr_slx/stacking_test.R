# Extract cross-fitted predictions
lasso_l <- dml_lasso$predictions$ml_l[, , 1]
lasso_m <- dml_lasso$predictions$ml_m[, , 1]

xgb_l <- dml_xgb$predictions$ml_l[, , 1]
xgb_m <- dml_xgb$predictions$ml_m[, , 1]

# Check dimensions
dim(lasso_l)
dim(lasso_m)
dim(xgb_l)
dim(xgb_m)

# get y and d
y <- dml_lasso$data$data$hr_score
d <- dml_lasso$data$data$lech_hr_mean

# make and execute functions

get_stack_weight <- function(y, pred_lasso, pred_xgb) {
  
  objective <- function(w_lasso) {
    
    w_xgb <- 1 - w_lasso
    
    pred_stack <- 
      w_lasso * pred_lasso +
      w_xgb   * pred_xgb
    
    mean((y - pred_stack)^2)
  }
  
  opt <- optimize(
    f = objective,
    interval = c(0, 1)
  )
  
  w_lasso <- opt$minimum
  w_xgb   <- 1 - w_lasso
  
  list(
    w_lasso = w_lasso,
    w_xgb = w_xgb,
    mse = opt$objective
  )
}

weights_l <- lapply(1:3, function(r) {
  
  get_stack_weight(
    y = y,
    pred_lasso = lasso_l[, r],
    pred_xgb = xgb_l[, r]
  )
})

weights_m <- lapply(1:3, function(r) {
  
  get_stack_weight(
    y = d,
    pred_lasso = lasso_m[, r],
    pred_xgb = xgb_m[, r]
  )
})

stacked_l <- sapply(1:3, function(r) {
  
  w_lasso <- weights_l[[r]]$w_lasso
  w_xgb   <- weights_l[[r]]$w_xgb
  
  w_lasso * lasso_l[, r] +
    w_xgb * xgb_l[, r]
})

stacked_m <- sapply(1:3, function(r) {
  
  w_lasso <- weights_m[[r]]$w_lasso
  w_xgb   <- weights_m[[r]]$w_xgb
  
  w_lasso * lasso_m[, r] +
    w_xgb * xgb_m[, r]
})

u1 <- y - stacked_l[,1]
v1 <- d - stacked_m[,1]
u2 <- y - stacked_l[,2]
v2 <- d - stacked_m[,2]
u3 <- y - stacked_l[,3]
v3 <- d - stacked_m[,3]

sum(v1 * u1) / sum(v1^2)
sum(v2 * u2) / sum(v2^2)
sum(v3 * u3) / sum(v3^2)

get_nnls1 <- function(y, pred_lasso, pred_xgb) {
  
  cv_resid <- cbind(
    y - pred_lasso,
    y - pred_xgb
  )
  
  sq_resid <- crossprod(cv_resid)
  
  A <- cbind(
    matrix(1, 2, 1),
    diag(1, 2)
  )
  
  r <- quadprog::solve.QP(
    Dmat = Matrix::nearPD(sq_resid)$mat,
    dvec = matrix(0, 2, 1),
    Amat = A,
    bvec = c(1, rep(0, 2)),
    meq = 1
  )
  
  r$solution
}

weights_l_test <- lapply(1:3, function(r) {
  get_nnls1(
    y = y,
    pred_lasso = lasso_l[, r],
    pred_xgb = xgb_l[, r]
  )
})

weights_m_test <- lapply(1:3, function(r) {
  get_nnls1(
    y = d,
    pred_lasso = lasso_m[, r],
    pred_xgb = xgb_m[, r]
  )
})






