library(methods)

#define the age_model class with slots for name of model and number
#of age catergoreis
setClass("age_model",
         #slots
         slots = c(
           name = "character",
           n_age_categories = "numeric"
         ),

         #prototypes for the slots
         prototype = list(
           name = NA_character_,
           n_age_categories = NA_real_
         )
)


#method of the S4 age_model class that solves the system of age structured odes
#

#set generic function name for solving system of age structured odes
setGeneric(name="solve_age_ode",
           def=function(object)
           {
             standardGeneric("solve_age_ode")
           }
)
#set class specific method
setMethod(f = "solve_age_ode", "age_model", function(object) {

  #ode solving method here
  object@name <- 'blah'
  return(object)
})



#test case for creating an instance
my_model <- new("age_model", name = "my first model", n_age_categories = 20)



