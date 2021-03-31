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
           parameter_names = list('S0', 'I0', 'R0', 'beta', 'kappa', 'gamma'),
           n_age_categories = NA_real_

         )
)

#define function that assigns the parameters of an age_model object
#inputs are: object - the age model object
#S0, I0, R0, beta, kappa, gamma: chosen values for each of the parameters
#N.B. this is not an S4 method, when I tried to use such a method I was unable
#to assign slots of the object successfully.
#function returns the age_model object with parameters slot assigned
set_parameters <- function(object, S0, I0, R0, beta, kappa, gamma){
  #create list of parameter values
  params <- list(S0, I0, R0, beta, kappa, gamma)

  #add names to each value
  names(params) = object@parameter_names

  #assign the params namelist to the object
  object@parameters <- params

  return(object)
}




#age_model class specific functions
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



