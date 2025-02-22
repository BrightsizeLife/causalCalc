calc_joint_causal_classes <- function(
    res,
    rope_diff_sigma = c(-0.1, 0.1)
) {
  # 'res' is the output list from our previous fit_bayes_var_diff() function
  # (or any object containing posterior_draws$slope_mean and posterior_draws$diff_sigma).
  #
  # We'll define:
  #  - slope > 0 => excitatory
  #  - slope < 0 => inhibitory
  #
  #  - diff_sigma = sigma(1) - sigma(0)
  #     > 0 => var(1) > var(0)
  #     < 0 => var(1) < var(0)
  #     ~ 0 => "practically equivalent" (in ROPE).
  #
  # Then, within excitatory or inhibitory, we check:
  #   necessary & sufficient => diff_sigma in ROPE
  #   sufficient only        => diff_sigma < 0   (for excitatory, means var(1) < var(0))
  #   necessary only         => diff_sigma > 0   (for excitatory, means var(1) > var(0))
  #
  # (And the mirror version for inhibitory.)
  #
  # We'll return the proportion of posterior draws that meet each condition.
  
  post_slope      <- res$posterior_draws$slope_mean
  post_diff_sigma <- res$posterior_draws$diff_sigma
  
  # Identify the number of posterior draws
  n_draws <- length(post_slope)
  
  # For each draw, we see which category it falls into:
  # We'll create logical vectors:
  slope_gt0 <- (post_slope > 0)
  slope_lt0 <- (post_slope < 0)
  
  diff_in_rope <- (post_diff_sigma >= rope_diff_sigma[1] & 
                     post_diff_sigma <= rope_diff_sigma[2])
  diff_gt0     <- (post_diff_sigma > 0)
  diff_lt0     <- (post_diff_sigma < 0)
  
  # Now we define the categories:
  # 1) excitatory necessary & sufficient => slope>0 & var diff in rope
  excitatory_ns <- slope_gt0 & diff_in_rope
  
  # 2) excitatory sufficient only => slope>0 & var(0)>var(1) => diff_sigma < 0
  excitatory_s_only <- slope_gt0 & diff_lt0
  
  # 3) excitatory necessary only => slope>0 & var(0)<var(1) => diff_sigma > 0
  excitatory_n_only <- slope_gt0 & diff_gt0
  
  # 4) inhibitory necessary & sufficient => slope<0 & var diff in rope
  inhibitory_ns <- slope_lt0 & diff_in_rope
  
  # 5) inhibitory sufficient only => slope<0 & var(0)>var(1) => diff_sigma < 0
  inhibitory_s_only <- slope_lt0 & diff_lt0
  
  # 6) inhibitory necessary only => slope<0 & var(0)<var(1) => diff_sigma > 0
  inhibitory_n_only <- slope_lt0 & diff_gt0
  
  # Compute the proportion of draws that fall into each category
  # (simply sum the logical vector and divide by total draws)
  p_excitatory_ns    <- sum(excitatory_ns)    / n_draws
  p_excitatory_sonly <- sum(excitatory_s_only) / n_draws
  p_excitatory_nonly <- sum(excitatory_n_only) / n_draws
  
  p_inhibitory_ns    <- sum(inhibitory_ns)    / n_draws
  p_inhibitory_sonly <- sum(inhibitory_s_only) / n_draws
  p_inhibitory_nonly <- sum(inhibitory_n_only) / n_draws
  
  # Put them in a data frame
  out <- data.frame(
    excitatory_necessary_sufficient = p_excitatory_ns,
    excitatory_sufficient_only      = p_excitatory_sonly,
    excitatory_necessary_only       = p_excitatory_nonly,
    inhibitory_necessary_sufficient = p_inhibitory_ns,
    inhibitory_sufficient_only      = p_inhibitory_sonly,
    inhibitory_necessary_only       = p_inhibitory_nonly
  )
  
  # Return
  return(out)
}



# Suppose you already fit the model with fit_bayes_var_diff() and got a result object 'res'
# (as in previous examples).

classification_joint <- calc_joint_causal_classes(
  res, 
  rope_diff_sigma = c(-0.1, 0.1)
)

classification_joint
