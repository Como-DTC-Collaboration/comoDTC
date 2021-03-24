# install comoDTC library 
install.packages("comoDTC")
# install comoOdeCpp
install.packages("devtools")
library("devtools")
install_github("bogaotory/comoOdeCpp", subdir="comoOdeCpp")
# run simulations side by side

test_that("Matching Rcpp and DTC version at p={0.00,0.01, ... 0.1}", {
    # test from https://github.com/bogaotory/comoOdeCpp/blob/master/comoOdeCpp/tests/testthat/test-scenario01.R#L70
  
  # skip("temp skip")

  rm(list = ls())
  source(paste0(getwd(), "/common.R"), local = environment())
  init(e = environment())

  file_path <- paste0(getwd(), "/data/templates_v16.8/Template_CoMoCOVID-19App_v17_r_v_cpp.xlsx")

  if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), CORE_FILE), local = environment())
  }

  check_parameters_list_for_na(parameters)

  # environment(check_mortality_count) <- environment()

  p_value_list <- seq(0.0, 0.1, by = 0.025)
  # p_value_list <- seq(0.1, 0.1)

  scenario_list <- list(
    vectors0, # Baseline
    vectors   # Hypothetical
  )

  # sss = 0

  for (ss in scenario_list) {
    for (pp in p_value_list) {
      parameters["p"] <- pp

      param_vector <- parameters
      param_vector[parameters_noise] <- parameters[parameters_noise]
        + rnorm(
            length(parameters_noise),
            mean = 0,
            sd = noise * abs(parameters[parameters_noise])
          )

      covidOdeCpp_reset()
      output_message <- capture_output(
        out_cpp <- ode(
                    y = Y, times = times, method = "euler", hini = 0.05,
                    func = covidOdeCpp, parms = param_vector,
                    input = ss, A = A,
                    contact_home = contact_home,
                    contact_school = contact_school,
                    contact_work = contact_work,
                    contact_other = contact_other,
                    popbirth_col2 = popbirth[, 2],
                    popstruc_col2 = popstruc[, 2],
                    ageing = ageing,
                    ifr_col2 = ifr[, 2],
                    ihr_col2 = ihr[, 2],
                    mort_col = mort,
                    age_group_vectors = age_group_vectors
                    )
      )
      expect_equal(output_message, "covidOdeCpp: splinefuns updated")

      processed_cpp_results <- process_ode_outcome(out_cpp, ss, param_vector)

      # expect_equal(
      #   processed_cpp_results$total_reportable_deaths_end,
      #   processed_cpp_results$total_cm_deaths_end,
      #   tolerance = 0.1,
      #   scale = processed_cpp_results$total_cm_deaths_end
      # )

      # stop()

      expect_silent(
        out_r <- ode(
                  y = Y, times = times, method = "euler", hini = 0.05,
                  func = covid, parms = param_vector, input = ss
                  )
      )

      processed_r_results <- process_ode_outcome(out_r, ss, param_vector)

      # expect_equal(
      #   processed_r_results$total_reportable_deaths_end,
      #   processed_r_results$total_cm_deaths_end,
      #   tolerance = 0.1,
      #   scale = processed_r_results$total_cm_deaths_end
      # )

      match_processed_outputs(
          output_a = processed_cpp_results,
          output_b = processed_r_results,
          tlr = 0.0001
      )

      # sss = 1
      # write.csv(out_cpp, paste0("out_cpp_",sss,"_",parameters["p"],".csv"),row.names = FALSE)
      # write.csv(out_r, paste0("out_r_",sss,"_",parameters["p"],".csv"),row.names = FALSE)

      match_outputs(
        output_a = out_r,
        output_b = out_cpp,
        tlr = 0.0001,
        smp = 1000
      )

    }
  }

})
