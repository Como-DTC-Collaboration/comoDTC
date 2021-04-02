#' SEIR basic model for compartments of symptome 
#' 
#' model diagram:
#'  --> S --(lam)--> E --(gamma), (ihr),((phs))--> [I, HA, VentA, ICUA] --((nu))--> R 
#'  |                                                                               |
#'  |_____________________________________(omega)___________________________________|
#' 
#' 
#' Population groups:
##'
##' S - suspeptable, E - effective, R - recovered
##' Compartments of infectious population groups with different symptomes,
##' currently, we asssume no treatment applied to any group:
##' 
###' I - infectious with mild/no symptome
###' HA - infectious with severe symptomes that need normal hospitalization (no need for icu/vent, H+HC)
###' VentA - infectious with severe symptomes that need venticulartor (Vent+VentC+HCV+ICUCV)
###' ICUA - infectious with severe symptomes that need icu (ICU+ICUC+HCICU)
#' Parameters:
##' lam: numeric(range(0,1)), rate from S to E
##' gamma: numeric(range(0,1)),rate from E to become infectious
##' omega: numeric(range(0,1)),rate from R to S
##' ihr: numeric(range(0,1)),rate from E to infectious groups that have severe symptome (HA+VentA+ICUA) 
##' phs$phs_icua: numeric(range(0,1)),rate from E to ICUA
##' phs$phs_venta: numeric(range(0,1)),rate from severe symptomes to VentA
##' nu - numeric(range(0,1)) vector, rate of recovery for each infectious group.


library(dplyr)
library(methods)
library(deSolve)
library(ggplot2)
library(reshape2)

# rm(list=ls())

setClass(Class = "model_symptome", 
         representation = representation(
           name = "character",
           initial_population = 'list',
           parameters = 'list',
           model_output = 'list'
         ),
         prototype = list(
           name = NA_character_,
           initial_population = vector(mode='list', length=7) %>%
             setNames(list('S', 'E', 'I', 'HA', 'VentA', 'ICUA', 'R')),
           parameters = vector(mode='list', length=6) %>%
             setNames(list('lam', 'gamma', 'omega', 'ihr', 'phs', 'nu')),
           model_output = data.frame()
         ))

#' Check validity of initail param and population settings
#' to be used in set_init method
check_init <- function(object) {
  errors <- character()
  # check if all required parameters are set
  is_na_params <- is.na(object@parameters)
  if (sum(is_na_params) != 0) {
    msg <- paste("Missing parameters:", 
                 paste(names(object@parameters)[is_na_params], collapse=", ")
    )
    errors <- c(errors, msg)
  }
  
  # check if all required initial population groups are set
  is_na_pop <- is.na(object@initial_population)
  if (sum(is_na_pop) != 0) {
    msg <- paste("Missing initial setting for population group:", 
                 paste(names(object@initial_population)[is_na_pop], collapse=", ")
    )
    errors <- c(errors, msg)
  }else{
    # check if the sum of initial population is normalized to 1
    sum_init_pop <- sum(unlist(object@initial_population))
    if (sum_init_pop != 1) {
      msg <- "Sum of initial population is not 1, please normalize"
      errors <- c(errors, msg)
    }
  }
  
  # check if the lengths of phs and nu are correct
  n_phs <- length(object@parameters$phs)
  if (n_phs != 2) {
    msg <- paste("Length of parameter phs,", n_phs, ", is not equal to the setting ", 2)
    errors <- c(errors, msg)
  }
  
  n_nu <- length(object@parameters$nu)
  if (n_nu != 4) {
    msg <- paste0(
      "Length of parameter nu,", n_nu, ", is not equal to the setting ", 4)
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}

#' Define functions for symptome_model class
#' Generic set_init to set initial population sizes (in fraction) and parameters
#' 
setGeneric(name = "set_init",
           def = function(object,...){standardGeneric("set_init")}
           )

setMethod('set_init', signature(object = 'model_symptome'),
          function(object, 
                   S=NA_real_, E=NA_real_, I=NA_real_, HA=NA_real_, VentA=NA_real_, ICUA=NA_real_, R=NA_real_, 
                   lam=NA_real_, gamma=NA_real_, omega=NA_real_, ihr=NA_real_, phs=NA_real_, nu=NA_real_) {
            param_list <- list(lam, gamma, omega, ihr, phs, nu)
            init_pop_list <- list(S, E, I, HA, VentA, ICUA, R)
            names(param_list) <- names(object@parameters)
            names(init_pop_list) <- names(object@initial_population)
            object@initial_population <- init_pop_list
            object@parameters <- param_list
            
            # check if initial settings are valid
            check <- check_init(object)
            if(check==TRUE) object else print(paste(check,", please check and rerun set_init."))
          })


#'  ode_simulate to solve ode systems
setGeneric(name = "ode_simulate",
           def = function(object, ...){standardGeneric("ode_simulate")}
           )

#' param times: a list of time points
#' param method: string, the ode integrator to use. Default is set to 'Isoda', same as the function ode().
#' 
setMethod("ode_simulate", signature(object ='model_symptome'),
  function(object, times, method = 'lsoda') {
    # initial population groups
    pop_groups <- c(S = object@initial_population$S,
                    E = object@initial_population$E,
                    I = object@initial_population$I,
                    HA = object@initial_population$HA,
                    VentA = object@initial_population$VentA,
                    ICUA = object@initial_population$ICUA,
                    R = object@initial_population$R
    )
    
    # parameters
    params <- c(lam = object@parameters$lam,
                gamma = object@parameters$gamma,
                omega = object@parameters$omega,
                ihr = object@parameters$ihr,
                phs = object@parameters$phs,
                nu = object@parameters$nu 
    )
    
    # ODE system RHS
    ode_symptome_rhs <- function(t, pop_groups, parameters) {
      with(
        as.list(c(pop_groups, params)),
        {
          dSdt <- -lam*S - omega*R
          dEdt <- lam*S - gamma*E 
          dIdt <- -nu$nui*I + (1.0-ihr)*gamma*E # - ratetestI*I
          dRdt <-  -omega*R + nu$nui*I + nu$nuhsa*HA + nu$nu_icua*ICUA + nu$nu_venta*VentA
          dHAdt <- gamma*ihr*(1-(phs%>%unlist%>%sum))*E - nu$nuhsa*HA
          dICUAdt <- gamma*ihr*phs$phs_icua*E - nu$nu_icua*ICUA
          dVentAdt <- gamma*ihr*phs$phs_venta*E - nu$nu_venta*VentA
          
          list(c(dSdt, dEdt, dIdt, dRdt, dHAdt, dICUAdt, dVentAdt))
        })
    }
    output <- ode(
      y = pop_groups, 
      times = times, 
      func = ode_symptome_rhs,  
      parms = params,
      method = method)
    
    
    output <- as.data.frame(output)
    
    # normalize: Keep the output population as fraction (range(0,1)) for each time step
    output.pop <- output[,-which(names(output) == "time")]
    output.pop <- sweep(output.pop, 1, apply(output.pop, 1, min))
    output[,-which(names(output) == "time")] <- output.pop/rowSums(output.pop)
    object@model_output <- output
    return(object)
  }
  )


# simple plot to observe population fraction change of each group by time
setGeneric(name = "plot_ode_output",
           def = function(object, ...){standardGeneric("plot_ode_output")}
)
setMethod("plot_ode_output", signature(object ='model_symptome'),
          function(object) {
            # reshape output frame 
            output.melt <- reshape2::melt(object@model_output, id.vars="time")
            names(output.melt) <- c("time","population_group", "fraction")
            
            # plot the population change of each group
            p <- ggplot2::ggplot(data = output.melt, aes(x=time, y=fraction)) +
              geom_line(aes(colour=population_group)) + 
              theme_classic()
            return (p)
          })



# test on simulated initial pop and params

# init_population (fraction)
S = 1-1e-7
E = 1e-7
I = 0
HA = 0
VentA = 0
ICUA = 0
R = 0

# params
lam = 1
gamma = 1
omega = 0.01
ihr = 0.1
phs = list(phs_icua=0.1, # ICUA
           phs_venta=0.05) # VentA

# rate of recovery for each compartment of symptome
nu = list(nui=0.5,# I
          nuhsa=0.1, # HA 
          nu_venta=0.005, # VentA
          nu_icua=0.01) # ICUA

# check_init(nes_model)
nes_model <- new("model_symptome", name = "new_model")
nes_model <- set_init(nes_model, 
                      S=S, E=E, I=I, HA=HA, VentA=VentA, ICUA=ICUA, R=R, # initial population size of each group
                      lam=lam, gamma=gamma, omega=omega, ihr=ihr, phs=phs, nu=nu) # parameters

# simulation period: 100 time points
nes_model <- ode_simulate(nes_model, seq(0, 100, by = 1), method="lsoda")
# head(nes_model@model_output)
# plot outcome
plot_ode_output(nes_model)


