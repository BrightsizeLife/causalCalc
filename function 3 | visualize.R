library(tidyverse)


visualize_causal_classes <- function(
    classified_df,
    rope_mean = c(-0.1, 0.1),
    rope_sigma = c(-0.1, 0.1)
) {
  
  color_map <- c(
    "Excitatory Necessary & Sufficient" = "#E69F00",  # Orange
    "Excitatory Necessary Only" = "#56B4E9",          # Sky Blue
    "Excitatory Sufficient Only" = "#009E73",         # Bluish Green
    "Inhibitory Necessary & Sufficient" = "#D55E00",  # Vermilion
    "Inhibitory Necessary Only" = "#CC79A7",          # Purple
    "Inhibitory Sufficient Only" = "#F0E442",         # Yellow
    "No Effect" = "#000000",                          # Black
    "Unclassified" = "#999999"                        # Grey (fallback)
  )
  
  ggplot(classified_df, aes(x = sigma_diff, y = slope, color = effect)) +
    geom_point(alpha = 0.5, size = 1.2) +
    geom_vline(xintercept = rope_sigma, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = rope_mean, linetype = "dashed", color = "grey50") +
    labs(
      title = "Posterior Samples with Causal Classifications",
      x = "Difference in log(sigma)",
      y = "Slope (Effect on Mean of Y)",
      color = "Causal Classification"
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
    tidybayes::theme_tidybayes() +
    scale_color_manual(
      values = color_map
    )
}


