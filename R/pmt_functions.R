#' Calculate Annuity Factors with Survival and Cost-of-Living Adjustments
#'
#' This function computes annuity factors for a series of survival discount rates, incorporating cost-of-living adjustments (COLA). It optionally supports a one-time COLA adjustment.
#'
#' @param surv_DR_vec Numeric vector. A vector of survival discount rates, representing the probabilities of survival for each period.
#' @param cola_vec Numeric vector. A vector of cost-of-living adjustment (COLA) rates for each period.
#' @param one_time_cola Logical, optional. If TRUE, a one-time COLA is applied (default is FALSE), this is not coded as no COLAs.
#'
#' @return Numeric vector. A vector of annuity factors, considering COLAs where each element corresponds to a period's annuity factor.
#'
#' @examples
#' # Example with survival discount rates and COLA rates
#' surv_DR_vec <- c(1.0, 0.9, 0.8, 0.7)
#' cola_vec <- c(0.02, 0.02, 0.02, 0.02)
#' annfactor(surv_DR_vec, cola_vec)
#'
#' # Example with a one-time COLA
#' annfactor(surv_DR_vec, cola_vec, one_time_cola = FALSE)
#'
#' @export
annfactor <- function(surv_DR_vec, cola_vec, one_time_cola = FALSE) {
  N <- length(surv_DR_vec)                                     # Define the length of the input vector
  annfactor_vec <- numeric(N)                                  # Create the output vector with the same length

  for (i in 1:N) {
    cola <- ifelse(one_time_cola, 0, cola_vec[i])              # If one-time COLA, the cola is 0 (Question: This actually means no COLAs)
    cola_project <- c(0, rep(cola, max(0, N - i)))             # Project COLA for future periods with the same COLA rate

    cumprod_cola <- cumprod(1 + cola_project)                  # Calculate the cumulative product of previous COLA rates
    surv_ratio <- surv_DR_vec[i:N] / surv_DR_vec[i]            # Set the base year survival as 1, calculate probability of survival in future years

    annfactor_vec[i] <- sum(surv_ratio * cumprod_cola)         # The sum of product of cumulative COLA increase and survival rates to a future year is the annuity factor considering COLA
  }

  return(annfactor_vec)
}



# functions that need to be tested ----------------------------------------

#' #' Calculate first payment of an annuity due
#' #'
#' #' With interest rate (rate) and remaining period (t)
#' #' payments are made in advance (beginning of each time period)
#' #' Reference: Annuity Due Payment - PV, https://financeformulas.net/Annuity-Due-Payment-from-Present-Value.html
#' #'
#' #' @param rate Annual discount rate (scalar double).
#' #' @param t Number of years in the future (scalar).
#' #'
#' #' @return numeric value.
#' #' @export
#' #'
#' #' @examples
#' #' get_pmt_due(0.05, 5) # 0.219976
#' get_pmt_due <- function(rate, t) {
#'   if (rate == 0) {
#'     pmt <- 1/t
#'   } else {
#'     pmt <- (rate / (1 -(1 + rate) ^ (-t))) * (1 / (1 + rate))
#'   }
#'   return(pmt)
#' }
#'

#' #' Calculate first payment of a growth annuity due
#' #'
#' #' With interest rate (rate), growth rate (g), and remaining period (t).
#' #' Payments are made in advance (beginning of each time period).
#' #'
#' #' @param rate Annual discount rate (scalar double).
#' #' @param growth Annual growth rate (scalar double).
#' #' @param t Number of years in the future (scalar).
#' #'
#' #' @return numeric value.
#' #' @export
#' #'
#' #' @examples
#' #' get_pmt_growth(0.05, 0.02, 5) #0.2117597
#' get_pmt_growth <- function(rate, growth, t) {
#'   if (rate == growth) {
#'     pmt_growth = 1/t
#'   } else {
#'     pmt_growth = ((rate - growth) / (1 - ((1 + growth) / (1 + rate)) ^ t)) * (1 / (1 + rate))
#'   }
#'   return(pmt_growth)
#' }


#' #' Calculate the total first payment of an annuity due
#' #'
#' #' With interest rate (r), number of periods (nper), and present value (pv).
#' #' This function multiplies the payment factor (calculated using get_pmt_due) by the present value.
#' #'
#' #' @param r Annual discount rate (scalar double).
#' #' @param nper Number of periods (scalar integer).
#' #' @param pv Present value of the annuity (scalar numeric).
#' #'
#' #' @return numeric value representing the total first payment.
#' #' @export
#' #'
#' #' @examples
#' #' get_pmt0(0.05, 5, 1000) # Example: calculates the total first payment
#' get_pmt0 <- function(r, nper, pv) {
#'   get_pmt_due(r, nper)*pv
#' }


#' #' Recursive Growing Function with Lag
#' #'
#' #' This function calculates a series of values that grow recursively based on an initial value and a vector of growth rates, incorporating a lag effect.
#' #'
#' #' @param x Numeric vector. A vector where the first element represents the initial value, and the rest are placeholders that will be replaced with recursively calculated values.
#' #' @param g Numeric vector. A vector of growth rates for each period, expressed as decimals (e.g., 0.05 for 5% growth).
#' #'
#' #' @return Numeric vector. A vector of the same length as \code{x}, where the first value is unchanged, and subsequent values are recursively grown based on \code{g}.
#' #'
#' #' @details
#' #' - The cumulative growth is calculated using \code{cumprod(1 + g)}, which computes the cumulative product of \code{1 + g}.
#' #' - Values in \code{x[2:length(x)]} are calculated as \code{x[1] * g_cul}, where \code{g_cul} is the cumulative growth factor.
#' #' - The lag effect ensures that the growth is applied recursively from the initial value.
#' #'
#' #' @examples
#' #' # Example with an initial value and growth rates
#' #' x <- numeric(5)
#' #' x[1] <- 100  # Initial value
#' #' g <- c(0.05, 0.03, 0.02, 0.04)
#' #' recur_grow(x, g)
#' #'
#' #' @export
#' recur_grow <- function(x, g) {
#'   g_cul <- cumprod(1 + g)
#'   x[2:length(x)] <- x[1] * g_cul[1:(length(g) - 1)]
#'   return(x)
#' }


#' #' Recursive Growing Function (No Lag)
#' #'
#' #' This function calculates a series of values that grow recursively based on an initial value and a vector of growth rates, without incorporating a lag effect.
#' #'
#' #' @param x Numeric vector. A vector where the first element represents the initial value, and the rest are placeholders that will be replaced with recursively calculated values.
#' #' @param g Numeric vector. A vector of growth rates for each period, expressed as decimals (e.g., 0.05 for 5% growth).
#' #'
#' #' @return Numeric vector. A vector of the same length as \code{x}, where the first value is unchanged, and subsequent values are recursively grown based on \code{g} without a lag.
#' #'
#' #' @details
#' #' - The growth vector \code{g} is shifted to remove the lag, so \code{g[1]} represents the growth for the first step.
#' #' - The function then calls \code{\link{recur_grow}} to perform the recursive growth calculation.
#' #'
#' #' @examples
#' #' # Example with an initial value and growth rates
#' #' x <- numeric(5)
#' #' x[1] <- 100  # Initial value
#' #' g <- c(0.05, 0.03, 0.02, 0.04)
#' #' recur_grow2(x, g)
#' #'
#' #' @seealso \code{\link{recur_grow}} for the version with lag.
#' #'
#' #' @export
#' recur_grow2 <- function(x, g) {
#'   g[1:length(g)-1] <- g[2:length(g)]
#'   recur_grow(x, g)
#' }


#' #' Recursive Growing Function with a Single Base and Fixed Growth Rate
#' #'
#' #' This function calculates a series of values that grow recursively starting from a single base value, using a fixed growth rate over a specified number of periods.
#' #'
#' #' @param x Numeric. The base value from which the growth calculation begins.
#' #' @param g Numeric. The fixed growth rate, expressed as a decimal (e.g., 0.05 for 5% growth per period).
#' #' @param nper Integer. The total number of periods for the growth calculation, including the initial value.
#' #'
#' #' @return Numeric vector. A vector of length \code{nper}, where the first element is \code{x}, and subsequent elements are calculated by applying the fixed growth rate recursively.
#' #'
#' #' @details
#' #' - The growth rate \code{g} is applied recursively to generate a series of growth factors.
#' #' - The initial value \code{x} is included as the first element of the resulting vector, followed by the recursively grown values.
#' #'
#' #' @examples
#' #' # Example with a base value and a fixed growth rate
#' #' x <- 100  # Initial value
#' #' g <- 0.05  # Growth rate (5%)
#' #' nper <- 5  # Number of periods
#' #' recur_grow3(x, g, nper)
#' #'
#' #' @export
#' recur_grow3 <- function(x, g, nper) {
#'   growth_factors <- cumprod(rep(1 + g, nper - 1))
#'   x_vec <- c(x, x * growth_factors)
#'   return(x_vec)
#' }

