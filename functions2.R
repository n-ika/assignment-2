randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}


difference_in_means <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- mean(d_1[[var]]) - mean(d_2[[var]])
  return(result)
}




permutation_twogroups <- function(d, 
                                  var, grouping_var, 
                                  group1, group2, 
                                  statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    permuted_data <- randomize(d, var)
    permutation_statistics[i] <- statistic(permuted_data, 
                                           var, grouping_var, 
                                           group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}




estimate_statistical_power <- function(sample_size, obs_mean1, 
                                       obs_mean2, st_dev) {
  alpha <- 0.05
  power_df <- NULL
  df_sample1 <- data.frame("group" = "A", 
                           "value" = rnorm(sample_size, obs_mean1, st_dev))
  df_sample2 <- data.frame("group" = "B", 
                           "value" = rnorm(sample_size, obs_mean2, st_dev))
  df_sample_all <- rbind(df_sample1, df_sample2)
  overall_mean <- mean(c(df_sample1$value, df_sample2$value))
  for (values in df_sample_all$value) {
    n_tests <- 1000
    n_successes <- 0
    for (i in 1:n_tests) {
      # Simulate world NOT from null hypothesis
      df_sample1_h0 <- rnorm(sample_size, overall_mean, st_dev)
      df_sample2_h0 <- rnorm(sample_size, overall_mean, st_dev)
      v <- t.test(df_sample1_h0, df_sample2_h0)
      pval <- v$p.value
      if (pval < alpha) {
        n_successes <- n_successes + 1
      }
    }
    power <- n_successes/n_tests
    power_df_current <- tibble::data_frame(
      power=power,
      values=values,
      alpha=alpha
    )
    power_df <- dplyr::bind_rows(power_df, power_df_current)
  }
  return(power_df)
}
