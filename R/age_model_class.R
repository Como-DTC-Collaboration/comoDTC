library(methods)

#define the age_model class.
#Slots of this class are as follows:
#name: character representing name of model
#parameters: named list containing parameters of the model. initial values for
#each compartment, S0, I0, R0. other parameters: beta, kappa, gamma
#of age catergoreis
setClass("age_model",
         #slots
         slots = c(
           name = "character",
           output_names = "list",
           parameter_names = "list",
           parameters = 'list',
           n_age_categories = "numeric"
         ),

         #prototypes for the slots, automatically set output and param
         #names
         prototype = list(
           name = NA_character_,
           output_names = list('S', 'I', 'R', 'Incidence'),
           parameter_names = list('S0', 'I0', 'R0', 'alpha', 'beta', 'gamma'),
           n_age_categories = NA_real_

         )
)

#Generic for function for setting the parameters of the model instance
#setGeneric(name = "set_parameters",
#           def = function(object, S0, I0, R0, beta, kappa, gamma){
#             standardGeneric("set_parameters")
#           }
#)
#set class specific method assigning parameters
#setMethod(f = "set_parameters", "age_model", function(object, S0, I0, R0,
#                                                      beta, kappa, gamma) {

set_parameters <- function(object, S0, I0, R0, beta, kappa, gamma){
  #create list of parameter values
  params <- list(S0, I0, R0, beta, kappa, gamma)

  #add names to each value
  names(params) = object@parameter_names

  #assign the params namelist to the object
  object@parameters <- params

  return(object)
}


#Generic for function which gives RHS of odes
setGeneric(name = "right_hand_side",
           def = function(object, t, y, c){
             standardGeneric("right_hand_side")
           }
)
#set class specific method for RHS of odes
setMethod(f = "right_hand_side", "age_model", function(object, t, y, c) {
  #RHS code here
})


#Generic for function which gives RHS of odes
setGeneric(name = "right_hand_side",
           def = function(object, t, y, c){
             standardGeneric("right_hand_side")
           }
)
#set class specific method for RHS of odes
setMethod(f = "right_hand_side", "age_model", function(object, t, y, c) {
  #RHS code here
})



#Generic for function which solves system of odes
setGeneric(name = "simulate",
           def = function(object, parameters, times){
             standardGeneric("simulate")
           }
)
#set class specific method to numerically solve odes
setMethod(f = "simulate", "age_model", function(object, parameters,
                                                       times) {

  #call RHS function to then use as input for the ode solver
  RHS = right_hand_side(object, t, y, c)

  #simulate code here output = ode(etc)

  return(output)
})







#test case for creating an instance
my_model <- new("age_model", name = "my first model", n_age_categories = 20)



