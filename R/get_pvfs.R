#' Calculate Present Value of Future Salaries (PVFS)
#'
#' This function computes the present value of future salaries (PVFS) for a given set of probabilities, interest rates, and salary values over a time period.
#'
#' @param remaining_prob_vec Numeric vector. The remaining survival probabilities for each period.
#' @param interest_vec Numeric vector. The interest rates for each period.
#' @param sal_vec Numeric vector. The projected salaries for each period.
#'
#' @return Numeric vector. A vector of PVFS values, where each element represents the present value of future salaries starting from that period.
#'
#' @details
#' The present value of future salaries is calculated by:
#' - Adjusting projected salaries by the survival probability at each period.
#' - Discounting future salaries to their present value using the interest rates provided.
#'
#' @examples
#' # Example inputs
#' remaining_prob_vec <- c(1, 0.95, 0.90, 0.85)
#' interest_vec <- c(0.03, 0.03, 0.03, 0.03)
#' sal_vec <- c(50000, 52000, 54000, 56000)
#'
#' # Calculate PVFS
#' get_pvfs(remaining_prob_vec, interest_vec, sal_vec)
#'
#' @export
get_pvfs <- function(remaining_prob_vec, interest_vec, sal_vec) {
  N <- length(sal_vec)
  PVFS <- double(length = N)
  for (i in 1:N) {
    remaining_prob_sub <- remaining_prob_vec[i:N] / remaining_prob_vec[i] # Calculate survival probabilities for future periods, using i year survival rate as the base
    interest <- interest_vec[i]                                           # Get the interest rate for the current period
    sal_sub <- sal_vec[i:N]                                               # Subset salaries for future periods from i period
    df_sub  <- (1 + interest)^(-(1:length(sal_sub)))                      # Discount factors in each year based on the interest rate used in t
    PVFS[i] <- sum(sal_sub * remaining_prob_sub * df_sub)                 # The sum of product of the future salaries, survival probability, and discount factor is present value of future salaries
  }
  return(PVFS)
}
