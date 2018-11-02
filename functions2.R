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
