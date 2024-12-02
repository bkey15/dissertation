# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(glmnet)
library(naniar)

# load data ----
load(here("data/ch1/results/imputations/imp_1_l1.rda"))
load(here("data/ch1/results/imputations/imp_2_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_l1.rda"))

# get imputations ----
## imp_1 (no start year) ----
imp_1.1 <- imp_1_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 1) |> 
  select(1:48)

imp_1.2 <- imp_1_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 2) |> 
  select(1:48)

imp_1.3 <- imp_1_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 3) |> 
  select(1:48)

imp_1.4 <- imp_1_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 4) |> 
  select(1:48)

imp_1.5 <- imp_1_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 5) |> 
  select(1:48)

## imp_2 (1968) ----
imp_2.1 <- imp_2_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 1) |> 
  select(1:48)

imp_2.2 <- imp_2_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 2) |> 
  select(1:48)

imp_2.3 <- imp_2_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 3) |> 
  select(1:48)

imp_2.4 <- imp_2_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 4) |> 
  select(1:48)

imp_2.5 <- imp_2_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 5) |> 
  select(1:48)

## imp_3 (1977) ----
imp_3.1 <- imp_3_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 1) |> 
  select(1:48)

imp_3.2 <- imp_3_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 2) |> 
  select(1:48)

imp_3.3 <- imp_3_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 3) |> 
  select(1:48)

imp_3.4 <- imp_3_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 4) |> 
  select(1:48)

imp_3.5 <- imp_3_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(.imp == 5) |> 
  select(1:48)

# prep ----
## imp_1 ----
### get hr_score ----
dep_1 <- imp_1.1 |> 
  select(hr_score) |> 
  mutate(row = 1:nrow(imp_1.1))

### get missing rows ----
imp_1.na <- imp_1.1 |> 
  where_na() |> 
  as_tibble() |> 
  select(row) |> 
  unique()

### filter out missing rows from hr_score ----
dep_1 <- dep_1 |> 
  anti_join(imp_1.na)
dep_1 <- dep_1$hr_score

### get covars ----
#### 1.1
imp_1.1_covars <- imp_1.1 |> 
  select(-c(hr_score, inforce))
imp_1.1_covars <- model.matrix(~ . - 1, data = imp_1.1_covars)

#### 1.2
imp_1.2_covars <- imp_1.2 |> 
  select(-c(hr_score, inforce))
imp_1.2_covars <- model.matrix(~ . - 1, data = imp_1.2_covars)

#### 1.3
imp_1.3_covars <- imp_1.3 |> 
  select(-c(hr_score, inforce))
imp_1.3_covars <- model.matrix(~ . - 1, data = imp_1.3_covars)

#### 1.4
imp_1.4_covars <- imp_1.4 |> 
  select(-c(hr_score, inforce))
imp_1.4_covars <- model.matrix(~ . - 1, data = imp_1.4_covars)

#### 1.5
imp_1.5_covars <- imp_1.5 |> 
  select(-c(hr_score, inforce))
imp_1.5_covars <- model.matrix(~ . - 1, data = imp_1.5_covars)

## imp_2 ----
### get hr_score ----
dep_2 <- imp_2.1 |> 
  select(hr_score) |> 
  mutate(row = 1:nrow(imp_2.1))

### get missing rows ----
imp_2.na <- imp_2.1 |> 
  select(-inforce) |> 
  where_na() |> 
  as_tibble() |> 
  select(row) |> 
  unique()

### filter out missing rows from hr_score ----
dep_2 <- dep_2 |> 
  anti_join(imp_2.na)
dep_2 <- dep_2$hr_score

### get covars ----
#### 2.1
imp_2.1_covars <- imp_2.1 |> 
  select(-c(hr_score, inforce))
imp_2.1_covars <- model.matrix(~ . - 1, data = imp_2.1_covars)

#### 2.2
imp_2.2_covars <- imp_2.2 |> 
  select(-c(hr_score, inforce))
imp_2.2_covars <- model.matrix(~ . - 1, data = imp_2.2_covars)

#### 2.3
imp_2.3_covars <- imp_2.3 |> 
  select(-c(hr_score, inforce))
imp_2.3_covars <- model.matrix(~ . - 1, data = imp_2.3_covars)

#### 2.4
imp_2.4_covars <- imp_2.4 |> 
  select(-c(hr_score, inforce))
imp_2.4_covars <- model.matrix(~ . - 1, data = imp_2.4_covars)

#### 2.5
imp_2.5_covars <- imp_2.5 |> 
  select(-c(hr_score, inforce))
imp_2.5_covars <- model.matrix(~ . - 1, data = imp_2.5_covars)

## imp_3 ----
### get hr_score ----
dep_3 <- imp_3.1 |> 
  select(hr_score) |> 
  mutate(row = 1:nrow(imp_3.1))

### get missing rows ----
imp_3.na <- imp_3.1 |> 
  select(-inforce) |> 
  where_na() |> 
  as_tibble() |> 
  select(row) |> 
  unique()

### filter out missing rows from hr_score ----
dep_3 <- dep_3 |> 
  anti_join(imp_3.na)
dep_3 <- dep_3$hr_score

### get covars ----
#### 3.1
imp_3.1_covars <- imp_3.1 |> 
  select(-c(hr_score, inforce))
imp_3.1_covars <- model.matrix(~ . - 1, data = imp_3.1_covars)

#### 3.2
imp_3.2_covars <- imp_3.2 |> 
  select(-c(hr_score, inforce))
imp_3.2_covars <- model.matrix(~ . - 1, data = imp_3.2_covars)

#### 3.3
imp_3.3_covars <- imp_3.3 |> 
  select(-c(hr_score, inforce))
imp_3.3_covars <- model.matrix(~ . - 1, data = imp_3.3_covars)

#### 3.4
imp_3.4_covars <- imp_3.4 |> 
  select(-c(hr_score, inforce))
imp_3.4_covars <- model.matrix(~ . - 1, data = imp_3.4_covars)

#### 3.5
imp_3.5_covars <- imp_3.5 |> 
  select(-c(hr_score, inforce))
imp_3.5_covars <- model.matrix(~ . - 1, data = imp_3.5_covars)

# cross validate (lambda) ----
## imp_1 ----
### 1.1
set.seed(15275)
imp_1.1_cv_res <- cv.glmnet(
  x = imp_1.1_covars,
  y = dep_1,
  alpha = 1
)

### 1.2
set.seed(15275)
imp_1.2_cv_res <- cv.glmnet(
  x = imp_1.2_covars,
  y = dep_1,
  alpha = 1
)

### 1.3
set.seed(15275)
imp_1.3_cv_res <- cv.glmnet(
  x = imp_1.3_covars,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_cv_res <- cv.glmnet(
  x = imp_1.4_covars,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_cv_res <- cv.glmnet(
  x = imp_1.5_covars,
  y = dep_1,
  alpha = 1
)

## imp_2 ----
### 2.1
set.seed(15275)
imp_2.1_cv_res <- cv.glmnet(
  x = imp_2.1_covars,
  y = dep_2,
  alpha = 1
)

### 2.2
set.seed(15275)
imp_2.2_cv_res <- cv.glmnet(
  x = imp_2.2_covars,
  y = dep_2,
  alpha = 1
)

### 2.3
set.seed(15275)
imp_2.3_cv_res <- cv.glmnet(
  x = imp_2.3_covars,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_cv_res <- cv.glmnet(
  x = imp_2.4_covars,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_cv_res <- cv.glmnet(
  x = imp_2.5_covars,
  y = dep_2,
  alpha = 1
)

## imp_3 ----
### 3.1
set.seed(15275)
imp_3.1_cv_res <- cv.glmnet(
  x = imp_3.1_covars,
  y = dep_3,
  alpha = 1
)

### 3.2
set.seed(15275)
imp_3.2_cv_res <- cv.glmnet(
  x = imp_3.2_covars,
  y = dep_3,
  alpha = 1
)

### 3.3
set.seed(15275)
imp_3.3_cv_res <- cv.glmnet(
  x = imp_3.3_covars,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_cv_res <- cv.glmnet(
  x = imp_3.4_covars,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_cv_res <- cv.glmnet(
  x = imp_3.5_covars,
  y = dep_3,
  alpha = 1
)

# get min lambds ----
## imp_1 ----
imp_1.1_min_lam <- imp_1.1_cv_res$lambda.min
imp_1.2_min_lam <- imp_1.2_cv_res$lambda.min
imp_1.3_min_lam <- imp_1.3_cv_res$lambda.min
imp_1.4_min_lam <- imp_1.4_cv_res$lambda.min
imp_1.5_min_lam <- imp_1.5_cv_res$lambda.min

## imp_2 ----
imp_2.1_min_lam <- imp_2.1_cv_res$lambda.min
imp_2.2_min_lam <- imp_2.2_cv_res$lambda.min
imp_2.3_min_lam <- imp_2.3_cv_res$lambda.min
imp_2.4_min_lam <- imp_2.4_cv_res$lambda.min
imp_2.5_min_lam <- imp_2.5_cv_res$lambda.min

## imp_3 ----
imp_3.1_min_lam <- imp_3.1_cv_res$lambda.min
imp_3.2_min_lam <- imp_3.2_cv_res$lambda.min
imp_3.3_min_lam <- imp_3.3_cv_res$lambda.min
imp_3.4_min_lam <- imp_3.4_cv_res$lambda.min
imp_3.5_min_lam <- imp_3.5_cv_res$lambda.min

# save lambdas ----
imp_1_lam <- imp_1.1_min_lam
imp_2_lam <- imp_2.1_min_lam
imp_3_lam <- imp_3.1_min_lam*0.8 + imp_3.5_min_lam*.2

imp_1_lam |> save(file = here("data/ch1/results/tunes/imp_1_lam.rda"))
imp_2_lam |> save(file = here("data/ch1/results/tunes/imp_2_lam.rda"))
imp_3_lam |> save(file = here("data/ch1/results/tunes/imp_3_lam.rda"))
