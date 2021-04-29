test_that("Initialisation checks are running", {
  # Check error is raised when numbers of intervention starts and ends are not
  # the same
  expect_error(
    int_parms <- InterventionParameters(start=c(75, 120.1, 100),
                                      stop=c(120, 150),
                                      coverage=c(0.3, 0.8, 0.1))
  )

  # Check error is raised when numbers of intervention starts and coverage
  # levels are not the same
  expect_error(
    int_parms <- InterventionParameters(start=c(75, 120.1),
                                        stop=c(120, 150),
                                        coverage=c(0.3))
  )

  # Check error is raised when intervention ends before it starts
  expect_error(
    int_parms <- InterventionParameters(start=100,
                                        stop=75,
                                        coverage=0.3)
  )

  # Check that error is raised when subsequent intervention start before the
  # last ends
  expect_error(
    int_parms <- InterventionParameters(start=c(75, 100),
                                        stop=c(120, 150),
                                        coverage=c(0.3, 0.8))
  )

  # Check error is raised when intervention coverage is greater than 1 or
  # smaller than 0
  expect_error(
    int_parms <- InterventionParameters(start=100,
                                        stop=75,
                                        coverage=1.3)
  )
  expect_error(
    int_parms <- InterventionParameters(start=100,
                                        stop=75,
                                        coverage=-0.1)
  )
})

test_that("Smoothing of intervention protocols", {
  int_parms <- InterventionParameters(start=75,
                                      stop=120,
                                      coverage=0.5)
  sim_parms <- SimulationParameters(start=60, stop=150, tstep = 0.1)
  protocol1 <- intervention_protocol(int_parms, sim_parms, 1)

  expect_equal(protocol1$coverage[protocol1$time == 75], 0.5/2)
  expect_equal(protocol1$coverage[protocol1$time == 120], 0.5/2)

  # Stack of intervention protocols
  int_parms <- InterventionParameters(start=c(75, 121),
                                      stop=c(120, 130),
                                      coverage=c(0.5, 0.8))
  sim_parms <- SimulationParameters(start=60, stop=150, tstep = 0.1)
  protocol2 <- intervention_protocol(int_parms, sim_parms, 1)

  protocol1_value <- protocol1$coverage[protocol1$time == 121]
  expect_equal(protocol2$coverage[protocol2$time == 121],
               0.8/2 + protocol1_value,
               tolerance = 1e-7)
})
