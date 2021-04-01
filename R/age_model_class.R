library(methods)
library(deSolve)

#define the age_model class.
#Slots of this class are as follows:
#name: character representing name of model
#parameters: named list containing parameters of the model. initial values for
#each compartment, S0, I0, R0. other parameters: beta, kappa, gamma
#of age catergoreis
setClass("age_model",
         #slots
         slots = c(
           name = 'character',
           output_names = 'list',
           parameter_names = 'list',
           parameters = 'list',
           n_age_categories = 'numeric'
         ),

         #prototypes for the slots, automatically set output and param
         #names
         prototype = list(
           name = NA_character_,
           output_names = list('S', 'I', 'R', 'Incidence'),
           parameter_names = list('S0', 'I0', 'R0', 'a', 'b'),
           parameters = vector(mode = "list", length = 5),
           n_age_categories = NA_real_

         )
)

#define function that assigns the parameters of an age_model object
#inputs are: object - the age model object
#S0, I0, R0, beta, kappa, gamma: chosen values for each of the parameters
#N.B. this is not an S4 method, when I tried to use such a method I was unable
#to assign slots of the object successfully.
#function returns the age_model object with parameters slot assigned
setGeneric("get_parameters", function(object) standardGeneric("get_parameters"))
setGeneric(
  "set_parameters",
  function(object, S0, I0, R0, a, b){
    standardGeneric("set_parameters")
  })
setMethod("get_parameters", "age_model", function(object) object@parameters)
setMethod(
  "set_parameters", "age_model",
  function(object, S0, I0, R0, a, b) {
    #write error messages
    if(length(S0) != object@n_age_categories){
      stop('Wrong number of age groups for initial susceptible compartments.')
    }
    if(!is.double(S0)){
      stop('Initial susceptibles storage format must be a vector.')
    }
    if(length(I0) != object@n_age_categories){
      stop('Wrong number of age groups for initial infective compartments.')
    }
    if(!is.double(I0)){
      stop('Initial infectives storage format must be a vector.')
    }
    if(length(R0) != object@n_age_categories){
      stop('Wrong number of age groups for initial recovered compartments.')
    }
    if(!is.double(R0)){
      stop('Initial recovered storage format must be a vector.')
    }  
    if(any(length(a) != 1 | length(b) != 1)){
      stop('The rates of change between compartments are 1-dimensional.')
    }
    
    #create list of parameter values
    params <- list(S0, I0, R0, a, b)
  
    #add names to each value
    names(params) = object@parameter_names
  
    #assign the params namelist to the object
    object@parameters <- params
    
    return(object)
  })

#age_model class specific functions

#Generic for function which solves system of odes
setGeneric(name = "simulate",
           def = function(object, times){
             standardGeneric("simulate")
           }
)
#set class specific method to numerically solve odes
setMethod(
  "simulate", "age_model",
  function(object, times) {
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    state <- c(S = get_parameters(object)$S0,
               I = get_parameters(object)$I0,
               R = get_parameters(object)$R0)
    parameters <- c(a = get_parameters(object)$a,
                    b = get_parameters(object)$b)
    
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)),
        {
          age <- object@n_age_categories
          S <- state[1:age]
          I <- state[(age+1):(2*age)]
          R <- state[(2*age+1):(3*age)]
          # rate of change
          dS <- -a*S*I
          dI <- a*S*I - b*I
          dR <- b*I
          # return the rate of change
          list(c(dS, dI, dR))
        })
    }
    
    output <- ode(
      y = state, times = times, func = right_hand_side, parms = parameters)
    
    return(output)
  })

#test case for creating an instance
my_model <- new("age_model", name = "my_model", n_age_categories = 2)

my_model <- set_parameters(my_model, c(1, 1), c(1, 0), c(0, 0), 1, 0.5)
get_parameters(my_model)
simulate(my_model, seq(0, 10, by = 1))
