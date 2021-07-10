#' Creates an excel sheet with country-specific data and returns the name of
#' the excel file. The user should input the country of choice. If they do not,
#' or the country name does not appear in countries_demog, the UK is used.
#' The data_CoMo.Rdata should be loaded before this function is called. This
#' function also relies on the package XLConnect, which requires Java. It
#' takes the /data/Template_CoMo_CountryData_temp.xlsx file, copies it and
#' fills in the Cases and Population sheets with country-specific data.
#'
#' @param country string of characters, optional, the country for which the
#' data should be generated
#'
#' @return name of the excel file that contains the country specific data.
#' @importFrom XLConnect XLC
create_excel <- function(country) {

  # check of argument is correct
  if (missing(country) || !any(countries_demog == country)) {
    print("Country not specified, or not recognized. Using United Kingdom.")
    country <- "United Kingdom"
  }
  # copy template file and load copy to be edited
  excel_name <- paste("../inst/extdata/Data_", country, "_", Sys.Date(), ".xlsx",
                      sep = "")
  file.copy("../inst/extdata/Template_CoMo_CountryData_temp.xlsx", excel_name)
  wb <- XLConnect::loadWorkbook(excel_name)

  # grab country specific date for cases and population, reformat date
  cases_country <- cases[which(cases$country == country), 1:3]
  cases_country$date <- as.Date(cases_country$date, format = "%d-%m-%Y")
  pop_country <- population[which(population$country == country), 3:5]

  # write data to excel file, DON'T CHANGE FORMATTING
  XLConnect::setStyleAction(wb, XLC$STYLE_ACTION.NONE)
  XLConnect::writeWorksheet(wb, data = cases_country, sheet = "Cases",
                       startRow = 2, startCol = 1, header = FALSE)
  XLConnect::writeWorksheet(wb, data = pop_country, sheet = "Population",
                       startRow = 2, startCol = 2, header = FALSE)

  # save edited excel sheet
  XLConnect::saveWorkbook(wb)

  # return name of country-specific excel sheet for further use.
  return(excel_name)
}
