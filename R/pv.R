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
