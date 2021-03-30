library(methods)

#define the age_model class
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




#test case for creating an instance
my_model <- new("age_model", name = "my first model", n_age_categories = 20)



