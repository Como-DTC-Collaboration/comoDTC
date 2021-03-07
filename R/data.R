#' contact_home
#'
#' A collection of contact matrices for 152 countries, describing which age
#' groups have contact with which other age groups AT HOME. The units of entries
#' are the average number of contacts with a person in that age group in a day.
#' The columns are the age groups 0-5 (X1), 5-10 (X2), 10-15 (X3), etc. up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list, consisting of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_home"

#' contact_school
#'
#' A collection of contact matrices for 152 countries, describing which age 
#' groups have contact with which other age groups AT SCHOOL. The units of entries
#' are the average number of contacts with a person in that age group in a day.
#' The columns are the age groups 0-5 (X1), 5-10 (X2), 10-15 (X3), etc. up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds..
#'
#' @format A Large list, consisting of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_school"

#' contact_work
#'
#' A collection of contact matrices for 152 countries, describing which age
#' groups have contact with which other age groups AT WORK. The units of entries
#' are the average number of contacts with a person in that age group in a day.
#' The columns are the age groups 0-5 (X1), 5-10 (X2), 10-15 (X3), etc. up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list, consisting of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_work"

#' contact_other
#'
#' A collection of contact matrices for 152 countries, describing which age
#' groups have contact with which other age groups AT OTHER PLACES (not covered
#' by the other contact matrices). The units of entries
#' are the average number of contacts with a person in that age group in a day.
#' The columns are the age groups 0-5 (X1), 5-10 (X2), 10-15 (X3), etc. up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list, consisting of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_other"

#' mort_sever_default
#'
#' Default infection fatality rate (ifr) and infection hospitalisation rate
#' (ihr) per age group. ifr has been scaled so the maximum value is 1.
#'
#' @format A dataframe with three variables
#' \describe{
#' \item{age_category}{0-4 years, 5-9 years, etc. up to and including 100+ years old}
#' \item{ifr}{fraction of infections that leads to death}
#' \item{ihr}{fraction of infections that leads to hospitalisation}
#' }
"mort_sever_default"

#' population
#'
#' Per country and per age group the number of individuals in the population,
#' the number birth rate and the death rate.
#'
#' @format A dataframe with five variables
#' \describe{
#' \item{country}{country}
#' \item{age_category}{0-4 years, 5-9 years, etc. up to and including 100+ years old}
#' \item{pop}{the number of individuals in this country in this age group}
#' \item{birth}{number of births per person per day}
#' \item{death}{number of deaths per person per day}
#' }
"population"

#' cases
#'
#' NOTE: I think this data is actually imported from the excel file. The sheets
#' "Cases" or "Epidemiology" provide the same information and are imported in the
#' covidage_v16.8.R script. The line where data_Como.Rdata is loaded is commented
#' out by default. However, the excel file only provides data for one
#' country, not for all like this data set does.
#'
#' Data on the number of cases and deaths per country per day from 31-12-2019.
#'
#' @format A dataframe with five variables
#' \describe{
#' \item{date}{Data, starting from 31-12-2019}
#' \item{cases}{Number of cases recorded}
#' \item{deaths}{Number of deaths recorded}
#' \item{country}{country}
#' \item{cumulative_death}{cumulative number of deaths}
#' }
"cases"
