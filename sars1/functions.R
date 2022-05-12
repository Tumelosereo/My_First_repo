# Include libraries
library(deSolve)

## The following function calculates the rates for our model, it takes
## parameters, states variables and time as inputs.

COVID <- function(t, statev, parms) {
  with(as.list(c(statev, parms)), {
    lambda <- lambda_value(parms, statev)
    pt <- prob_treat(Hts + Htd, parms)
    
    dSdt <- -lambda * S
    dIadt <- lambda * pa * S - eta * Ia
    dIsdt <- (1 - pa) * lambda * S - alpha * Is
    dMdt <- alpha * pm * Is - sigma * M
    dVdt <- (1 - pm) * alpha * Is - epsilon * V
    dNtdt <- (1 - pt) * epsilon * V - nu * Nt
    dHtsdt <- ps * pt * epsilon * V - gamma * Hts
    dHtddt <- (1 - ps) * pt * epsilon * V - mu * Htd
    dRdt <- eta * Ia + sigma * M + gamma * Hts
    dCMdt <- mu * Htd + nu * Nt
    dAdt <- pt * epsilon*V 
    
    list(c(dSdt, dIadt, dIsdt, dMdt, dVdt, dNtdt, dHtsdt, dHtddt, dRdt, dCMdt, dAdt))
  })
}

## The probability of being treated function.

prob_treat <- function(Ht, parms) {
  with(as.list(parms), {
    pmax / (1 + exp(-a + b * Ht)) # a is capacity, b is steepness and Ht = Hts + Htd
  })
}

## Reproduction number, R0 is R0 <- beta*D where for now we assume values
## D:- the duration of infection equal (5-6 days) and R0 = 2.

beta_value <- function(parms) {
  with(as.list(parms), {
    brn * (eta * sigma * alpha / (sigma * pa * alpha + eta * (sigma + pm * alpha) * (1 - pa)))
  })
}

## Lambda function:

lambda_value <- function(parms, statev, bb) {
  with(as.list(c(statev, parms)), {
    bb <- beta_value(parms = parm_values)
    bb * (Ia + Is + M) / N0
  })
}

## Cumulative mortality function:

CM_out <- function(ts) {
  return(ts$CM[nrow(ts)])
}

## The following function takes in different parameter values and return
## cumulative mortality.

run_covid <- function(t, vals, pp, statev, ret_cm = TRUE) {
  with(as.list(vals), {
    all_pars <- c(pp, ps = ps, mu = 1 / du, a = cc)
    
    ts <- data.frame(lsoda(
      y = statev,
      times = t,
      func = COVID,
      parms = all_pars
    ))
    if (ret_cm) {
      return(CM_out(ts))
    } else {
      return(ts)
    }
  })
}

