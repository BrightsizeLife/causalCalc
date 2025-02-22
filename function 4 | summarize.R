summarize_causal_classes <- function(classified_df) {
  classified_df %>%
    count(effect) %>%
    mutate(
      proportion = n / sum(n),
      percent = proportion * 100
    ) %>%
    arrange(desc(proportion)) %>%
    select(effect, n, proportion, percent)
}




#test function 2:4

# Step 1: Calculate causal classes
classified_df <- calc_joint_causal_classes(
  res,
  rope_mean = c(-0.1, 0.1), #will not change the classification here
  rope_sigma = c(-0.1, 0.1),
  use_rope_for_mean = TRUE,
  use_rope_for_sigma = TRUE
)

# Step 2: Summarize results
summary_df <- summarize_causal_classes(classified_df)
print(summary_df)

# Step 3: Visualize posterior classifications
visualize_causal_classes(classified_df, rope_mean = c(-0.1, 0.1), rope_sigma = c(-0.1, 0.1))
