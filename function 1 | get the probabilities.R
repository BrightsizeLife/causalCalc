# You need the brms package
library(brms)
library(tidybayes)  # helpful for posterior extraction, but not strictly required

fit_bayes_var_diff <- function(
    data,
    y,             # Name of the outcome variable (continuous)
    x,             # Name of the binary predictor (0/1)
    covars = NULL, # Optional vector of covariate names, e.g. c("age", "gender")
    rope_mean  = c(-0.1, 0.1),  # ROPE for the slope in the mean model
    rope_sigma = c(-0.1, 0.1),  # ROPE for the difference in log(sigma) or difference in sigma
    # or you can choose your own scale for the ROPE on sigma, see below
    priors = NULL,              # Optionally pass user-defined priors
    iter = 2000,
    warmup = 1000,
    chains = 4,
    seed = 1234
) {
  
  # 1. Construct the formula for the mean
  #    e.g. y ~ x + covariate1 + covariate2
  if (!is.null(covars)) {
    mean_form <- as.formula(
      paste0(y, " ~ ", x, " + ", paste(covars, collapse = " + "))
    )
  } else {
    mean_form <- as.formula(paste0(y, " ~ ", x))
  }
  
  # 2. Construct the formula for sigma
  #    e.g. sigma ~ x + covariate1 + covariate2
  #    NOTE: the sigma formula must start with 'sigma ~'
  if (!is.null(covars)) {
    sigma_form <- as.formula(
      paste0("sigma ~ ", x, " + ", paste(covars, collapse = " + "))
    )
  } else {
    sigma_form <- as.formula(paste0("sigma ~ ", x))
  }
  
  # 3. Combine into a 'brmsformula'
  bf_combined <- bf(mean_form, sigma_form)
  
  # 4. Fit the model
  fit <- brm(
    formula = bf_combined,
    data = data,
    family = gaussian(),
    prior = priors,
    iter = iter,
    warmup = warmup,
    chains = chains,
    seed = seed
  )
  
  # -------- Extract Posteriors -----------
  post <- as_draws_df(fit)
  
  # The slope for the mean of x is typically "b_x" in brms
  #   - If x is a factor, the name might differ. 
  #   - If your x is coded 0/1 numeric, it should appear as "b_x".
  # Double-check summary(fit) or names(post) to see the exact parameter name.
  
  slope_mean_name <- paste0("b_", x)           # e.g. "b_x"
  slope_sigma_name <- paste0("b_sigma_", x)    # e.g. "b_sigma_x"
  
  # If your covariates are included, you'll also see parameters like b_covariate1, etc.
  # But let's focus on the effect of x in both mean and sigma.
  
  # 1) Posterior for slope in the mean
  slope_mean_post <- post[[slope_mean_name]]
  
  # 2) Posterior for slope in the log(sigma)
  #    i.e., how log(sd) changes when x=1 vs x=0
  slope_sigma_post <- post[[slope_sigma_name]]
  
  # 3) Convert that to the difference in actual sigma if you prefer:
  #    sigma(0) = exp(intercept_sigma + sum of covars at X=0)
  #    sigma(1) = exp(intercept_sigma + b_sigma_x + sum of covars at X=1)
  #    But if you just want the difference in log(sigma), slope_sigma_post is enough.
  #
  # Let's illustrate the difference in sigma specifically attributable to x:
  #    difference_in_sigma = exp(... + b_sigma_x) - exp(...), which depends on the intercept
  #
  # For simplicity, let's define the difference in sigma with NO additional covariates changing,
  # i.e. holding them constant. We do that by extracting:
  intercept_sigma_name <- "b_sigma_Intercept"
  intercept_sigma_post <- post[[intercept_sigma_name]]
  
  # sigma0 = exp(intercept_sigma)
  sigma0_post <- exp(intercept_sigma_post)
  
  # sigma1 = exp(intercept_sigma + slope_sigma)
  sigma1_post <- exp(intercept_sigma_post + slope_sigma_post)
  
  diff_sigma_post <- sigma1_post - sigma0_post
  ratio_sigma_post <- sigma1_post / sigma0_post
  
  # --------- Summaries (Prob < 0, Prob > 0, Prob in ROPE) ---------
  
  # Define a helper function to get the probabilities for a single posterior vector
  get_probs <- function(posterior_vec, rope_bounds) {
    prob_less    <- mean(posterior_vec < 0)
    prob_greater <- mean(posterior_vec > 0)
    
    # Probability in ROPE (i.e., within rope_bounds)
    # Note: rope_bounds is a length-2 numeric vector, e.g. c(-0.1, 0.1)
    prob_in_rope <- mean(posterior_vec >= rope_bounds[1] & 
                           posterior_vec <= rope_bounds[2])
    
    return(c(prob_less = prob_less,
             prob_greater = prob_greater,
             prob_in_rope = prob_in_rope))
  }
  
  # 1) For the slope in the mean
  slope_mean_probs <- get_probs(slope_mean_post, rope_mean)
  slope_mean_est   <- mean(slope_mean_post)   # Posterior mean estimate
  
  # 2) For the difference in log(sigma)
  #    If you want to do a "difference in log(sigma)" ROPE, pass rope_sigma accordingly.
  #    Or you might prefer a ROPE on the difference_in_sigma itself.
  
  slope_sigma_probs <- get_probs(slope_sigma_post, rope_sigma)
  slope_sigma_est   <- mean(slope_sigma_post)
  
  # 3) For the difference in sigma (not log(sigma)), up to you if you want a separate ROPE:
  #    E.g. rope_sigma_diff = c(-0.05, 0.05) might be a suitable scale for difference in sd.
  #    This can be tricky because the scale is different. We'll keep it simple:
  
  diff_sigma_probs <- get_probs(diff_sigma_post, rope_sigma)  # if you want the same or a different rope
  diff_sigma_est   <- mean(diff_sigma_post)
  
  # You could also do ratio_sigma if that’s more meaningful:
  # For ratio, the "0" vs. "1" threshold changes. Some people do a rope around 1. 
  # We'll skip that here for brevity, but it is analogous.
  
  # Combine these into a summary data frame:
  summary_df <- data.frame(
    parameter = c("mean_slope_x",
                  "log_sigma_slope_x",
                  "sigma_diff_x1_minus_x0"),
    est_posterior_mean = c(slope_mean_est, 
                           slope_sigma_est, 
                           diff_sigma_est),
    prob_less_than_0   = c(slope_mean_probs["prob_less"],
                           slope_sigma_probs["prob_less"],
                           diff_sigma_probs["prob_less"]),
    prob_greater_than_0 = c(slope_mean_probs["prob_greater"],
                            slope_sigma_probs["prob_greater"],
                            diff_sigma_probs["prob_greater"]),
    prob_in_rope        = c(slope_mean_probs["prob_in_rope"],
                            slope_sigma_probs["prob_in_rope"],
                            diff_sigma_probs["prob_in_rope"])
  )
  
  # Return a list with:
  #  - The fitted brms model
  #  - The summary table
  #  - The posterior draws for the relevant parameters
  #    (You might want to keep them all, or store them in a tibble)
  
  return(list(
    model          = fit,
    summary        = summary_df,
    posterior_draws = list(
      slope_mean       = slope_mean_post,
      slope_sigma      = slope_sigma_post,
      sigma0           = sigma0_post,
      sigma1           = sigma1_post,
      diff_sigma       = diff_sigma_post,
      ratio_sigma      = ratio_sigma_post
    )
  ))
}



# Example data:
# Let's create a synthetic dataset with 200 observations,
# a binary x, and a couple of covariates (e.g., age, gender).

set.seed(2023)
N <- 200
df_example <- data.frame(
  x = rbinom(N, 1, 0.5),
  age = rnorm(N, mean = 50, sd = 10),
  gender = factor(sample(c("M","F"), N, replace = TRUE)),
  y = NA
)

# Let's define "true" parameters for simulation:
#   mean: y ~ 2 + 1.5*x + 0.1*age + (F vs M effect?), just for example
#   sd:   sigma ~ exp(0.1 + 0.3*x)  => sigma0 = exp(0.1), sigma1 = exp(0.4)

b0 <- 2
b1 <- 1.5   # slope for x
b_age <- 0.1
b_genderF <- -0.5  # say females a bit lower on average than males

sd_int <- 0.1       # intercept for log(sigma)
sd_slope_x <- 0.3   # slope for x in log(sigma)
# We'll ignore covariates for sigma in this example
#   but your real usage can handle them as well!

# Generate the linear predictor for the mean
lp_mean <- b0 + b1*df_example$x + b_age*df_example$age +
  b_genderF*(df_example$gender == "F")

# Generate sigma for each row
lp_sigma <- sd_int + sd_slope_x*df_example$x
sigma_vec <- exp(lp_sigma)

# Now generate y from normal with mean=lp_mean, sd=sigma_vec
df_example$y <- rnorm(N, mean = lp_mean, sd = sigma_vec)


# Let's fit using our function, with covariates "age" and "gender".
res <- fit_bayes_var_diff(
  data       = df_example,
  y          = "y",
  x          = "x",
  covars     = c("age","gender"),
  rope_mean  = c(-0.1, 0.1),
  rope_sigma = c(-0.1, 0.1),
  iter       = 2000,
  warmup     = 1000,
  chains     = 2
)

# Check the summary
res$summary

# res$posterior_draws$slope_mean
# res$posterior_draws$diff_sigma
#
# And the full brms model object is in:
res$model
summary(res$model)
plot(res$model)
