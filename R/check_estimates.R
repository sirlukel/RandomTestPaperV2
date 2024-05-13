#' Create a plot of the true values against the estimated values
#
#' @param normed_ests A data frame with the scaled estimated values
#' @param true_values A data frame with the true values
#'
#' @export
#'
plot_results <- function(normed_ests, true_values)
{
  ns = length(unique(true_values[,1]))
  if(length(unique(true_values[1:ns,3])))
  {
    replication = length(unique(true_values[1:ns,3]))
  }else
  {
    replication = -1
  }
  if (replication != -1){
    nq = replication
  }else{
    nq = length(true_values[,7])/ns
  }
  if (length(true_values[,7])/ns != nq)
  {
    nd <- length(true_values[,7])/ns
    plot(true_values[seq(from = 1, to = nd*ns, by = nd),2],normed_ests$abils, xlab = "True Abilities", ylab = "Estimated Abilites", ylim = c(-5,5))
  }else{
    plot(true_values[seq(from = 1, to = nq*ns, by = nq),2],normed_ests$abils, xlab = "True Abilities", ylab = "Estimated Abilites", ylim = c(-5,5))
  }
  abline(a = 0, b = 1, col = "red")
  plot(true_values[1:nq,5], normed_ests$discs, xlab = "True Discrimination", ylab = "Estimated Discrimination", ylim = c(0,5))
  abline(a = 0, b = 1, col = "red")
  plot(true_values[1:nq,4],normed_ests$diffs, xlab = "True Difficulties", ylab = "Estimated Difficulties", ylim = c(-5,5))
  abline(a = 0, b = 1, col = "red")
}


#' Estimates the bias of a data frame of estimates
#
#' @param est_vals A data frame with the scaled estimated values
#' @param true_vals A data frame with the true values
#'
#' @return The estimated bias
#' @export
#'
est_bias <- function(true_vals, est_vals)
{
  bias <- mean(true_vals - est_vals)
  return(bias)
}

#' Estimates the mean square error of a data frame of estimates
#
#' @param est_vals A data frame with the scaled estimated values
#' @param true_vals A data frame with the true values
#'
#' @return The estimated MSE
#' @export
#'
est_MSE <- function(true_vals, est_vals)
{
  MSE <- mean((true_vals - est_vals)^2)
  return(MSE)
}

#' Estimates the bias, variance, and mean squared error of a data frame of estimates
#
#' @param est_vals A data frame with the scaled estimated values
#' @param true_vals A data frame with the true values
#'
#' @return A list with the estimated bias, variance, and MSE
#' @export
#'
check_ests <- function(est_vals, true_vals)
{
  ns = length(unique(true_vals[,1]))
  if(length(unique(true_vals[1:ns,3])))
  {
    replication = length(unique(true_vals[1:ns,3]))
  }else
  {
    replication = -1
  }
  n <- length(true_vals[,7])
  if (replication != -1){
    nq = replication
  }else{
    nq = n/ns
  }

  true_vector <- c(true_vals[seq(1,n, length.out = ns),2], true_vals[1:nq,5], true_vals[1:nq, 4])
  est_vector <- c(est_vals$abils, est_vals$discs, est_vals$diffs)
  bias <- est_bias(true_vector, est_vector)
  MSE <- est_MSE(true_vector, est_vector)
  vari <- MSE - bias^2

  return(list(bias = bias, variance = vari, MSE = MSE))
}



#' A single function to call one of the 3 optimisation methods
#
#' @param data_set A data frame containing some generated data for a group of students
#' @param estimate_type 1 for random walk, 2 for optim, 3 for brute force
#' @param n Number of iterations for the random walk method
#' @param tuning The tuning parameter for the random walk method
#' @param group The group parameter for the random walk/optim methods
#'
#' @return A list with the scaled parameter estimates
#' @export
#'
estimate <- function(data_set, estimate_type = 1, n = 1000, tuning = 0.01, group = T)
{
  ns = length(unique(data_set[,1]))
  if(length(unique(data_set[1:ns,3])))
  {
    replication = length(unique(data_set[1:ns,3]))
  }else
  {
    replication = -1
  }
  if (replication != -1){
    nq = replication
  }else{
    nq = length(data_set[,7])/ns
  }
  if(estimate_type == 1){
    results <- find_all_optimised_1(cs = data_set[1:nq, 6], scores = data_set[,7], ns = ns, nq = nq, n = n, tuning = tuning, group = group)
  }else if(estimate_type == 2){
    results <- find_all_optimised_2(cs = data_set[1:nq, 6], scores = data_set[,7], ns = ns, nq = nq, group = group)
  }else
  {
    results <- find_all_optimised_3(cs = data_set[1:nq, 6], scores = data_set[,7], ns = ns, nq = nq)
  }

  results <- normalise_results(results, data_set[1,5])
  return(results)
}
