#' contact_home
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT HOME.
#' The units of entries are the average number of contacts with a person in
#' that age group in a day. The top row is individuals in the age group 0-5
#' years, the second row is individuals in the age group 5-10 years, and so
#' on torow 16 which is individuals aged 75-80. The columns represent the
#' people they come into contact with, where column 1 is the age groups 0-5
#' (X1), column to is the age group 5-10 (X2), and so to column 16 (ages 75-80).
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_home"

#' contact_school
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT
#' SCHOOL. The units of entries are the average number of contacts with a person
#' in that age group in a day. The top row is individuals in the age group
#' 0-5 years, the second row is individuals in the age group 5-10 years, and
#' so on to row 16 which is individuals aged 75-80. The columns represent the
#' people they come into contact with, where column 1 is the age groups 0-5
#' (X1), column to is the age group 5-10 (X2), and so to column 16 (ages 75-80).
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_school"

#' contact_work
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT WORK.
#' The units of entries are the average number of contacts with a person in that
#' age group in a day. The top row is individuals in the age group 0-5 years,
#' the second row is individuals in the age group 5-10 years, and so on to
#' row 16 which is individuals aged 75-80. The columns represent the people
#' they come into contact with, where column 1 is the age groups 0-5 (X1),
#' column to is the age group 5-10 (X2), and so to column 16 (ages 75-80).
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_work"

#' contact_other
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT OTHER
#' PLACES (not covered by the other contact matrices). The units of entries are
#' the average number of contacts with a person in that age group in a day.
#' The top row is individuals in the age group 0-5 years,
#' the second row is individuals in the age group 5-10 years, and so on to
#' row 16 which is individuals aged 75-80. The columns represent the people
#' they come into contact with, where column 1 is the age groups 0-5 (X1),
#' column to is the age group 5-10 (X2), and so to column 16 (ages 75-80).
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_other"

#' cases
#'
#' A dataframe with 5 variables: date (Date), cases (num), deaths (num),
#' country (chr) and cumulative_death (num).
#' It records per country and per date, the number of COVID
#' cases and the number of deaths, both on the day and cumulative.
#'
#' @format Dataframe with 5 columns, number of rows will differ per update.
#'
#' @source \url{https://www.ecdc.europa.eu/en/covid-19/data}

#' population
#'
#' A dataframe with 5 variables: country (chr), age_category (Factor),
#' pop (num), birth (num), death(num). It specifies per country and per age
#' category, the size of the population, the number of births per person per
#' day and the number of deaths per person per day. The age categories are
#' 0-4 years, 5-9 years, 10-14 years, and so on, up to the last category,
#' which is 100+ years old.
#'
#' @format Dataframe with 5 columns and 4221 rows.
#'
#' @source \url{https://population.un.org/wpp/Download/Standard/Population/}

#' countries_demog
#'
#' A list of 201 countries (chr) that the model can take, in alphabetical order.
#'
#' @format list of chr
