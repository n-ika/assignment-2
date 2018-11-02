# The functions file.

#####################################################################################

randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}

#####################################################################################

difference_in_means <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- mean(d_1[[var]]) - mean(d_2[[var]])
  return(result)
}


#####################################################################################

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


#####################################################################################


find_two_sided <- function(data, 
                           mean_diff) {
  permuted_values <- data$permuted
  std <- sd(permuted_values)*sqrt((length(permuted_values)-1)/(length(permuted_values)))
  permuted_mean <- mean(permuted_values)
  z <- (mean_diff - permuted_mean) / std
  
  p_left <- pnorm(z)
  p_right <- 1 - p_left
  
  two_sided <- 2*min(p_right,p_left)
  return(two_sided)
}

#####################################################################################



estimate_statistical_power <- function(sample_sizes,
                                        mean_diffs,
                                        st_devs) {
  alpha <- 0.05
  power_df <- NULL
  set.seed(29)
  mean <- sample(c(3,4),1)
  for (sample_size in sample_sizes) {
    for (mean_diff in mean_diffs) {
      for (st_dev in st_devs) {
        n_tests <- 9999
        n_successes <- 0
        for (i in 1:n_tests) {
          df_sample1 <- data.frame("group" = "A",
                                   "size" = sample_size,
                                   "value" = rnorm(sample_size, mean, st_dev))
          df_sample2 <- data.frame("group" = "B",
                                   "size" = sample_size,
                                   "value" = rnorm(sample_size, (mean + mean_diff), st_dev))
          pval <- t.test(df_sample1$value, df_sample2$value)$p.value
          if (pval < alpha) {
            n_successes <- n_successes + 1
          }
        }
        power <- n_successes/n_tests
        power_df_current <- tibble::data_frame(
          power=power,
          sample_size=sample_size,
          mean_diff=mean_diff,
          st_dev=st_dev,
          alpha=alpha
        )
        power_df <- dplyr::bind_rows(power_df, power_df_current)
      }
    }
  }
  return(power_df)
}
