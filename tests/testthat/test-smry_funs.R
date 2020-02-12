test_that(
  "Summary functions give right answer",
  {

    x <- c(10, 10, 20, 20)

    x_mean <- mean(x)
    x_sd <- sd(x)

    expect_equal(
      smry_meanSD(x=x, name = 'mean_sd'),
      c(mean_sd = "15.0 (5.77)")
    )

    expect_equal(
      smry_medianIQR(x=x),
      c(`Median [IQR]` = "10.0 [10.0-20.0]")
    )

    expect_equal(
      smry_countP(x),
      c(`10` = "2 (50.0%)", `20` = "2 (50.0%)", Missing = "0 (0.00%)")
    )

    expect_equal(
      smry_countP(x, include_first_cat = FALSE),
      c(`20` = "2 (50.0%)", Missing = "0 (0.00%)")
    )

    time <- rep(1, 50)
    status <- c(rep(1, 25), rep(0, 25))

    expect_equal(
      smry_countP(x, include_first_cat = FALSE),
      c(`20` = "2 (50.0%)", Missing = "0 (0.00%)")
    )

    expect_equal(
      smry_irateCI(time, status, unit = 1),
      c(`Incidence rate (95% CI)` = "0.50 (0.33, 0.72)")
    )

  }
)
