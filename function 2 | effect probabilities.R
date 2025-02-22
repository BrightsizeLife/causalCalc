library(tidyverse)

calc_joint_causal_classes <- function(
    res,
    rope_mean = c(-0.1, 0.1),  # ROPE for slope
    rope_sigma = c(-0.1, 0.1), # ROPE for variance difference
    use_rope_for_mean = TRUE,  # If TRUE, use rope_mean boundaries; else use 0 for excitatory/inhibitory
    use_rope_for_sigma = TRUE  # If TRUE, use rope_sigma boundaries; else use 0 for variance difference
) {
  
  # Define thresholds for mean effect (slope)
  slope_lower <- if (use_rope_for_mean) rope_mean[1] else 0
  slope_upper <- if (use_rope_for_mean) rope_mean[2] else 0
  
  # Define thresholds for variance effect (sigma)
  sigma_lower <- if (use_rope_for_sigma) rope_sigma[1] else 0
  sigma_upper <- if (use_rope_for_sigma) rope_sigma[2] else 0
  
  posterior_df <- tibble(
    sigma_diff = res$posterior_draws$diff_sigma,
    slope = res$posterior_draws$slope_mean
  ) %>%
    mutate(
      # Mean effect classification
      in_rope_mean = slope >= slope_lower & slope <= slope_upper,
      excitatory = slope > slope_upper,
      inhibitory = slope < slope_lower,
      
      # Variance effect classification
      in_rope_sigma = sigma_diff >= sigma_lower & sigma_diff <= sigma_upper,
      above_rope_sigma = sigma_diff > sigma_upper,
      below_rope_sigma = sigma_diff < sigma_lower,
      
      # Causal classification
      effect = case_when(
        # Excitatory cases
        excitatory & in_rope_sigma ~ "Excitatory Necessary & Sufficient",
        excitatory & below_rope_sigma ~ "Excitatory Sufficient Only",
        excitatory & above_rope_sigma ~ "Excitatory Necessary Only",
        
        # Inhibitory cases
        inhibitory & in_rope_sigma ~ "Inhibitory Necessary & Sufficient",
        inhibitory & below_rope_sigma ~ "Inhibitory Sufficient Only",
        inhibitory & above_rope_sigma ~ "Inhibitory Necessary Only",
        
        # No effect (slope within rope or zero boundary)
        in_rope_mean ~ "No Effect",
        
        # Catch-all
        TRUE ~ "Unclassified"
      )
    )
  
  return(posterior_df)
}


# Suppose you already fit the model with fit_bayes_var_diff() and got a result object 'res'
# (as in previous examples).

# Example usage with default ROPE boundaries:
result_with_rope <- calc_joint_causal_classes(
  res, 
  rope_mean = c(-0.1, 0.1),
  rope_sigma = c(-0.1, 0.1),
  use_rope_for_mean = TRUE,
  use_rope_for_sigma = TRUE
)

# Example usage with strict zero thresholds:
result_with_zero_cutoffs <- calc_joint_causal_classes(
  res,
  use_rope_for_mean = FALSE,  # Use 0 instead of rope_mean
  use_rope_for_sigma = FALSE  # Use 0 instead of rope_sigma
)


