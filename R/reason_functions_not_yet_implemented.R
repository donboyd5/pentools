#Adding new entrants function
#' @export
add_new_entrants <- function(g, ne_dist, wf1, wf2, ea, age, position_matrix){
  #g is the assumed population growth of the plan
  #ne_dist is a vector representing the distribution of new entrants for each entry age
  #wf1 is the population in period 1. wf2 is the wf1 population after decremented
  #ea and age are two vectors representing entry age and age for active members
  #position_matrix is the matrix that rearranges the new entrant numbers in the right positions to be added to the active workforce array
  ne <- sum(wf1)*(1 + g) - sum(wf2)
  ne_vec <- ne * ne_dist
  ne_matrix <- matrix(ne_vec, nrow = length(ea), ncol = length(age))
  ne_matrix_trans <- ne_matrix * position_matrix

  return(ne_matrix_trans)
}

#Cumulative future values function (with interest being a single value)
#' @export
get_cum_fv <- function(interest, cashflow, first_value = 0){
  cumvalue <- double(length = length(cashflow))
  cumvalue[1] <- first_value
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period, and 0 for beginning of period.
#' @export
get_pmt <- function(r, g = 0, nper, pv, t = 1) {
  a <- get_pmt0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

#Recursive growing function (with lag)
#' @export
recur_grow <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i - 1])
    }
  }
  return(x)
}

#Recursive growing function (no lag)
#' @export
recur_grow2 <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}

#Recursive growing function with a single base and a fixed growth rate
#' @export
recur_grow3 <- function(x, g, nper) {
  x_vec <- double(length = nper)
  x_vec[1] <- x

  for (i in 2:length(x_vec)) {
    x_vec[i] <- x_vec[i-1] * (1 + g)
  }

  return(x_vec)
}


# more functions ----------------------------------------------------------


# one of these is bad -----------------------------------------------------

#Present Value of Future Benefits (PVFB) function (to be applied to a vector of "Pension Wealth") for active members
#sep_rate_vec is a vector containing separation rates. interest_vec is a discount rate (ARR) vector. value_vect is a vector containing the present values of pension benefits at separation ages.
#The purpose of this function is to calculate the PVFB at each active age (not just the entry age)
#' @export
get_pvfb <- function(sep_rate_vec, interest_vec, value_vec) {
  PVFB <- double(length = length(value_vec))
  for (i in 1:length(value_vec)) {
    sep_rate <- sep_rate_vec[i:length(sep_rate_vec)]
    #sep_prob in a given year is the probability that the member will survive all the previous years and get terminated exactly in the given year
    sep_prob <- cumprod(1 - lag(sep_rate, n = 2, default = 0)) * lag(sep_rate, default = 0)
    interest <- interest_vec[i]
    value <- value_vec[i:length(value_vec)]
    value_adjusted <- value * sep_prob
    PVFB[i] <- npv(interest, value_adjusted[2:length(value_adjusted)])
  }
  return(PVFB)
}


#Present Value of Future Salaries (PVFS) function (to be applied to a vector of salaries)
#remaining_prob_vec is a vector containing the remaining probabilities. interest_vec is a discount rate (ARR) vector. sal_vec is a vector containing the salaries.
#' @export
get_pvfs <- function(remaining_prob_vec, interest_vec, sal_vec) {
  PVFS <- double(length = length(sal_vec))
  for (i in 1:length(sal_vec)) {
    remaining_prob_og <- remaining_prob_vec[i:length(remaining_prob_vec)]
    remaining_prob <- remaining_prob_og / remaining_prob_og[1]
    interest <- interest_vec[i]
    sal <- sal_vec[i:length(sal_vec)]
    sal_adjusted <- sal * remaining_prob
    PVFS[i] <- npv(interest, sal_adjusted)
  }
  return(PVFS)
}


# end one of these is bad -------------------------------------------------

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


#' #Annuity factor for current retirees' benefits
#' #We need this function to calculate the annuity factors when a constant COLA is granted after the first year
#' #' @export
# annfactor <- function(surv_DR_vec, cola_vec, one_time_cola = F){
#   annfactor_vec <- double(length(surv_DR_vec))
#   for (i in 1:length(annfactor_vec)) {
#     cola <- ifelse(one_time_cola == F, cola_vec[i], 0)
#
#     if (i == length(annfactor_vec)) {
#       cola_project <- 0
#     } else {
#       cola_project <- c(0, rep(cola, length((i+1):length(cola_vec))))
#     }
#
#     cumprod_cola <- cumprod(1 + cola_project)
#     annfactor_vec[i] <- sum((surv_DR_vec[i:length(surv_DR_vec)] / surv_DR_vec[i]) * cumprod_cola)
#   }
#   return(annfactor_vec)
# }

#Amo payment functions
#pmt0 = basic amo payment calculation, assuming payment beginning of period
#' @export
get_pmt0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- ifelse(nper == 0, 0, pv*r*(1+r)^(nper-1)/((1+r)^nper-1))
  }

  return(a)
}
