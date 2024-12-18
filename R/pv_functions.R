#' Calculate Present Value Factor
#'
#' This function computes the present value factor for a given interest rate and time period.
#'
#' @param rate Numeric. The interest rate per period as a decimal (e.g., 0.05 for 5%).
#' @param t Numeric. The time period for which the present value factor is calculated.
#'
#' @return Numeric. The present value factor, which is the discount factor (1 + rate)^(-t)
#'
#' @details
#' The present value factor is used to discount future cash flows to their present value.
#'
#' @examples
#' # Calculate the present value factor for a 5% interest rate over 3 periods
#' get_pv(rate = 0.05, t = 3)
#'
#' # Calculate the present value factor for a 10% interest rate over 1 period
#' get_pv(rate = 0.10, t = 1)
#'
#' @export
get_pv <- function (rate, t) {
  vt <- (1 + rate)^(-t)
  return(vt)
}


#' get_pv_cf_roll returns the remaining present value of future cash flows (cf) in every year forward
#'
#' @param rate Annual discount rate (scalar double).
#' @param cf A list of cash flows in n=length(cf) number of years
#'
#' @return A list of numeric values
#' @export
#'
#' @examples
#' cf <- c(100, 200, 300, 400, 500, 600)
#' get_pv_cf_roll (0.05, cf) #$1,704.37 $1,609.13 $1,427.72 $1,168.57 $839.49) $447.73
get_pv_cf_roll <- function(rate, cf) {
  pv <- numeric(length(cf))
  for(i in 1:length(cf)) {
    pv[i] <- npv(rate, cf[i:length(cf)])
  }
  return(pv)
}


#' Calculate Present Value of a Growing Annuity Payment
#'
#' This function computes the present value of a series of growing annuity payments over a specified time period, given an interest rate and growth rate.
#'
#' @param rate Numeric. The interest rate per period as a decimal (e.g., 0.05 for 5%).
#' @param growth Numeric. The growth rate of the payments per period as a decimal (e.g., 0.02 for 2%).
#' @param t Numeric. The total number of periods for the growing annuity payments.
#'
#' @return Numeric. The present value of the growing annuity payments.
#'
#' @examples
#' # Present value of a growing annuity with 5% interest, 2% growth, over 10 periods
#' get_pv_gpmt(rate = 0.05, growth = 0.02, t = 10)
#'
#' # Present value of a growing annuity with 5% interest, 5% growth, over 10 periods
#' get_pv_gpmt(rate = 0.05, growth = 0.05, t = 10)
#'
#' @export
get_pv_gpmt <- function (rate, growth, t) {
  if (rate == growth) {
    pv_gpmt = t
  } else {
    pv_gpmt = (1 - ((1 + growth)/(1 + rate))^t) / (rate - growth)
    return(pv_gpmt)
  }
}


#' Calculate Present Value of an Annuity Payment
#'
#' This function computes the present value of an annuity payment over a given time period and interest rate.
#'
#' @param rate Numeric. The interest rate per period as a decimal (e.g., 0.05 for 5%). If the rate is 0, the present value is equal to the total number of periods.
#' @param t Numeric. The total number of periods for the annuity payments.
#'
#' @return Numeric. The present value of the annuity payments.
#'
#' @examples
#' # Present value of an annuity with 5% interest over 10 periods
#' get_pv_pmt(rate = 0.05, t = 10)
#'
#' # Present value of an annuity with 0% interest over 10 periods
#' get_pv_pmt(rate = 0, t = 10)
#'
#' @export
get_pv_pmt <- function (rate, t) {
  if (rate == 0) {
    pv_pmt <- t
  } else {
    vt <- (1 + rate) ^ (-t)
    pv_pmt <- (1 - vt) / rate
  }
  return(pv_pmt)
}


#' npv calculates the present value of future cashflows (cf)
#'
#' @param rate Annual discount rate (scalar double).
#' @param cf A list of cash flows in n=length(cf) number of years
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' cf <- c(100, 200, 300, 400, 500, 600)
#' npv(0.05, cf)  # 1704.37
npv <- function(rate, cf) {
  df <- (1+rate)^(-(1:(length(cf))))    # Discount factor in each year based on rate
  pv <- sum(cf * df)                    # The sum of the product of cash flow and discount factor in each year is PV
  return(pv)
}

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
get_pvfs <- function(remaining_prob_vec,
                     interest_vec,
                     sal_vec) {
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



#' Present Value of a Growing Annuity
#'
#' This function calculates the present value (PV) of a growing annuity, taking
#' into account the interest rate, growth rate, number of periods, payment
#' amount, and timing of payments.
#'
#' @param rate Numeric. The interest rate per period (e.g., 0.05 for 5%).
#' @param g Numeric. The growth rate of payments per period (default is 0).
#' @param nper Integer. The total number of periods (e.g., 10 for 10 periods).
#' @param pmt Numeric. The payment amount per period.
#' @param t Integer. Timing of payments:
#'   - `t = 1`: Payments occur at the end of the period (default).
#'   - `t = 0`: Payments occur at the beginning of the period.
#' @return Numeric. The present value of the growing annuity.
#'
#' @examples
#' # Example: Calculate PV for a growing annuity
#' pv(rate = 0.05, g = 0.02, nper = 10, pmt = 1000, t = 1)
#'
#' @export
pv <- function(rate,
               g = 0,
               nper,
               pmt,
               t = 1) {
  r <- (1 + rate) / (1 + g) - 1
  PV <- pmt / r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}


#' Rolling Present Value Calculation
#'
#' This function computes the rolling present value of a series of payments (cash flows) over time.
#' The first value in the \code{pmt_vec} vector must be zero, as it is a placeholder for calculations.
#'
#' @param rate Numeric. The interest rate per period (e.g., 0.05 for 5%).
#' @param g Numeric. The growth rate of payments per period (default is 0).
#' @param nper Integer. The total number of periods (e.g., 10 for 10 periods).
#' @param pmt_vec Numeric vector. A vector of payment amounts over the periods.
#'   The first value in this vector must be zero.
#' @param t Integer. Timing of payments:
#'   - \code{t = 1}: Payments occur at the end of the period (default).
#'   - \code{t = 0}: Payments occur at the beginning of the period.
#'
#' @return Numeric vector. A vector containing the rolling present values for each period.
#'
#' @details
#' The rolling present value is calculated iteratively:
#' \itemize{
#'   \item For the first period (\code{i = 1}), the present value is calculated using the \code{pv} function for the second payment.
#'   \item For subsequent periods (\code{i > 1}), the present value is updated by rolling forward the previous value with interest
#'   and subtracting the adjusted payment.
#' }
#' The formula for the rolling present value is:
#' \deqn{PV[i] = PV[i-1] \cdot (1 + rate) - pmt_vec[i] \cdot (1 + rate)^{1 - t}}
#'
#' @examples
#' # Example: Calculate rolling PV for a series of payments
#' pmt_vec <- c(0, 1000, 1200, 1300, 1100, 1400)
#' roll_pv(rate = 0.05, g = 0.02, nper = 10, pmt_vec = pmt_vec, t = 1)
#'
#' @seealso
#' \code{\link{pv}} for the present value calculation used within this function.
#'
#' @export
roll_pv <- function(rate, g = 0, nper, pmt_vec, t = 1) {
  pv_vec <- double(length(pmt_vec))
  for (i in 1:length(pv_vec)) {
    if (i == 1) {
      pv_vec[i] <- pv(rate, g, nper, pmt_vec[2], t)
    } else {
      pv_vec[i] <- pv_vec[i-1] * (1 + rate) - pmt_vec[i] * (1 + rate)^(1 - t)
    }
  }

  return(pv_vec)
}


# functions that need to be tested ----

#' #' npv calculates the present value of future cashflows (cf)
#' #'
#' #' @param rate Annual discount rate (scalar double).
#' #' @param cf A list of cash flows in n=length(cf) number of years
#' #'
#' #' @return Numeric value.
#' #' @export
#' #'
#' #' @examples
#' #' cf <- c(100, 200, 300, 400, 500, 600)
#' #' npv(0.05, cf)  # 1704.37
#' #' @export
#' npv <- function(rate, cf) {
#'   df <- (1+rate)^(-(1:(length(cf))))    # Discount factor in each year based on rate
#'   pv <- sum(cf * df)                    # The sum of the product of cash flow and discount factor in each year is PV
#'   return(pv)
#' }


#' #' Calculate the Present Value of Future Benefits (PVFB)
#' #'
#' #' Given a vector of separation rates, a vector of corresponding interest rates, and a vector of
#' #' future values (benefits), this function computes the present value of these benefits. At each
#' #' period `i`, it calculates the probability of separation and then discounts the subsequent
#' #' future values back to the present using the provided interest rates.
#' #'
#' #' @param sep_rate_vec Numeric vector. The annual separation rates for each future period.
#' #' @param interest_vec Numeric vector. The annual interest (discount) rates use in that period.
#' #' @param value_vec Numeric vector. The future benefits payable at each period.
#' #'
#' #' @return A numeric vector of the same length as `value_vec`, where each element represents the
#' #'         present value of future benefits starting from that period.
#' #' @export
#' #'
#' #' @examples
#' #' sep_rate_vec <- c(0.01, 0.02, 0.03, 0.04)
#' #' interest_vec <- c(0.05, 0.05, 0.05, 0.05)
#' #' value_vec <- c(100, 200, 300, 400)
#' #' get_pvfb(sep_rate_vec, interest_vec, value_vec)
#' get_pvfb <- function(sep_rate_vec, interest_vec, value_vec) {
#'   N <- length(value_vec)
#'   PVFB <- double(length = N)
#'   for (i in 1:N) {
#'     sep_rate <- sep_rate_vec[i:N]
#'     sep_prob <- cumprod(1 - sep_rate) * sep_rate       # Probability of separating in each subsequent period
#'     interest <- interest_vec[i]
#'     if (i < N) {
#'       value_sub <- value_vec[(i+1):N]                  # Payment in t+1 until the end of periods
#'       sep_prob_sub <- sep_prob[-1]                     # Probability of remaining in the plan until the period t
#'       df_sub <- (1 + interest)^(-(1:length(value_sub))) # Discount factors in each year based on the interest rate used in t
#'       PVFB[i] <- sum(value_sub * sep_prob_sub * df_sub) # The product of probability, discount factor, future values (benefits) is PVFB
#'     } else {
#'       PVFB[i = N] <- 0                                 # At the last period, there are no future periods, so PVFB is 0
#'     }
#'   }
#'   return(PVFB)
#' }



# Boyd functions ----------------------------------------------------------
#
# npv <- function(cashflows, rate, immediate=FALSE) {
#   n <- length(cashflows)
#   if (immediate) { # cash flows at beginning of period
#     powers <- 0:(n-1)
#   } else {  # cash flows at end of period
#     powers <- 1:n
#   }
#   sum(cashflows / (1 + rate)^powers)
# }


# get_pvfb <- function(sep_rate_vec, interest_vec, value_vec){
#   pv <- function(i){
#     sep_prob <- cumprod(1 - data.table::shift(sep_rate_vec[i:end], n = 2, fill = 0)) *
#       data.table::shift(sep_rate_vec[i:end], n = 1, fill = 0)
#     cashflow <- sep_prob * value_vec[i:end]
#     npv(cashflow, interest_vec[i], immediate = TRUE)
#   }
#   end <- length(sep_rate_vec)
#   pvfb <- double(length = end)
#   for (i in 1:end) {
#     pvfb[i] <- pv(i)
#   }
#   # CAUTION: temporary treatment to replace zeros with NAs to match FRS model results
#   pvfb[pvfb == 0] <- NA
#   pvfb
# }

