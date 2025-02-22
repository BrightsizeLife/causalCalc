

bi_cause_cont_out <- function(
    data,
    y,                      # Continuous outcome variable name (string)
    x,                      # Binary predictor variable name (string)
    covars = NULL,          # Optional vector of covariate names, e.g., c("age", "gender")
    rope_mean = c(-0.1, 0.1),  # ROPE for the slope (mean model)
    rope_sigma = c(-0.1, 0.1), # ROPE for variance difference (sigma)
    priors = NULL,          # Optional user-defined priors
    iter = 2000,            # Number of iterations for Bayesian sampling
    warmup = 1000,          # Warm-up iterations
    chains = 4,             # Number of MCMC chains
    seed = 1234,            # Random seed for reproducibility
    use_rope_for_mean = TRUE,  # Use rope_mean boundaries or zero for slope classification
    use_rope_for_sigma = TRUE  # Use rope_sigma boundaries or zero for variance classification
) {
  
  # -----------------------
  # 1. Fit Bayesian Model
  # -----------------------
  res <- fit_bayes_var_diff(
    data = data,
    y = y,
    x = x,
    covars = covars,
    rope_mean = rope_mean,
    rope_sigma = rope_sigma,
    priors = priors,
    iter = iter,
    warmup = warmup,
    chains = chains,
    seed = seed
  )
  
  # -----------------------
  # 2. Classify Posterior Samples into Causal Categories
  # -----------------------
  classified_df <- calc_joint_causal_classes(
    res,
    rope_mean = rope_mean,
    rope_sigma = rope_sigma,
    use_rope_for_mean = use_rope_for_mean,
    use_rope_for_sigma = use_rope_for_sigma
  )
  
  # -----------------------
  # 3. Summarize Posterior Probabilities
  # -----------------------
  summary_df <- summarize_causal_classes(classified_df)
  
  # -----------------------
  # 4. Visualize Classifications
  # -----------------------
  effect_graph <- visualize_causal_classes(
    classified_df,
    rope_mean = rope_mean,
    rope_sigma = rope_sigma
  )
  
  # -----------------------
  # 5. Return Output as List
  # -----------------------
  return(list(
    model_result = res,             # Raw model object
    classified_df = classified_df, # Data with causal classifications
    summary_df = summary_df,       # Summary of causal classes (proportions & counts)
    effect_graph = effect_graph    # Visualization plot object
  ))
}




# Run the function
# Run full analysis pipeline
result <- bi_cause_cont_out(
  data = df_example,
  y = "y",
  x = "x",
  covars = c("age", "gender"),
  rope_mean = c(-0.1, 0.1),
  rope_sigma = c(-0.1, 0.1),
  iter = 4000,  # Increase iterations for better precision
  chains = 4
)

# Access components:
result$model_result        # Raw Bayesian model object
result$classified_df       # Data with causal classifications
result$summary_df          # Summary table of causal classes
result$effect_graph        # Visualization of classifications

# Plot the effect graph
result$effect_graph

# View summary probabilities
print(result$summary_df)

