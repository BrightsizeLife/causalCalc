summarize_causal_classes <- function(classified_df) {
  # Base counts for each detailed class
  base_summary <- classified_df %>%
    count(effect) %>%
    mutate(
      proportion = n / sum(n),
      percent = proportion * 100
    )
  
  # Map to 3x2x2 structure
  classified_df <- classified_df %>%
    mutate(
      effect_type = case_when(
        str_detect(effect, "Excitatory") ~ "Excitatory",
        str_detect(effect, "Inhibitory") ~ "Inhibitory",
        TRUE ~ "No Effect"
      ),
      sufficiency = case_when(
        str_detect(effect, "Sufficient") ~ "Yes",
        effect_type == "No Effect" ~ NA_character_,
        TRUE ~ "No"
      ),
      necessity = case_when(
        str_detect(effect, "Necessary") ~ "Yes",
        effect_type == "No Effect" ~ NA_character_,
        TRUE ~ "No"
      )
    )
  
  # Cross-tabulate effect, sufficiency, and necessity
  cross_tab <- classified_df %>%
    count(effect_type, sufficiency, necessity) %>%
    mutate(
      proportion = n / sum(base_summary$n),
      percent = proportion * 100
    ) %>%
    arrange(desc(proportion))
  
  return(list(
    detailed_summary = base_summary,
    cross_tab = cross_tab
  ))
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
