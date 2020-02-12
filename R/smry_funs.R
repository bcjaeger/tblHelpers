
#' Named summaries
#'
#' @description Tabulation of data into character values that
#'   are easily passed into tables. These functions are structured
#'   so that users can easily create their own. A named summary
#'   function will do the following:
#'
#'  - take a numeric vector as input
#'
#'  - return a __named__ character vector as an output
#'
#'   The name of the output is intended to be a label for
#'   the output in your tables. When these functions are used
#'   within the `dabulate` function, these qualifications work
#'   with the structure of `dplyr` and `purrr`.
#'
#' @param x a numeric vector
#'
#' @param name a character value indicating the name of the output.
#'
#' @param na.rm a logical value indicating whether NA values
#'   should be stripped before the computation proceeds.
#'
#' @param lwr a numeric value indicating the lower quantile to report.
#'
#' @param upr a numeric value indicating the upper quantile to report.
#'
#' @param include_first_cat a logical value indicating whether you'd
#'   like to include the first category (i.e., the reference group)
#'   when you present counts and percents for a categorical variable.
#'
#' @param use_na a logical value indicating whether another category
#'   should be included showing the count and percent of missing values.
#'
#' @param na_level a character value indicating how missing categories
#'   should be labeled. This is only relevant if `use_na = TRUE`
#'
#' @param time a numeric vector of time to event values
#'
#' @param status a numeric vector of 0s and 1s. Values of 0 should indicate
#'   censoring, and values of 1 indicate observed events.
#'
#' @param unit a numeric value indicating the unit of incidence. For example,
#'   `unit = 1000` will compute incidence rate per 1,000 person-years.
#'
#' @param level the level of conidence limits. For example, `level = 0.95`
#'   leads to 95% confidence limits.
#'
#' @return a named character value
#' @export
#'
#' @examples
#' smry_meanSD(rnorm(100))
#' smry_medianIQR(rnorm(100))
#'
#'
smry_meanSD <- function(x, name = "Mean (SD)", na.rm = TRUE){

  stopifnot(is.numeric(x), is.character(name), is.logical(na.rm))

  mn_dbl <- mean(x, na.rm = na.rm)
  mn_chr <- tblStrings::tbv_round(mn_dbl)

  sd_dbl <- stats::sd(x, na.rm = na.rm)
  sd_chr <- tblStrings::tbv_round(sd_dbl)

  glue::glue("{mn_chr} ({sd_chr})") %>%
    as.character() %>%
    purrr::set_names(name)

}

#' @rdname smry_meanSD
#' @export
smry_medianIQR <- function(x, name = "Median [IQR]",
  na.rm = TRUE, lwr = 0.25, upr = 0.75){

  stopifnot(is.numeric(x), is.character(name), is.logical(na.rm),
    is.numeric(lwr), is.numeric(upr))

  probs <- c(lwr[1], 1/2, upr[1])

  vals_dbl <- stats::quantile(x, probs = probs, na.rm = na.rm)
  vals_chr <- tblStrings::tbv_round(x)

  glue::glue("{vals_chr[2]} [{vals_chr[1]}-{vals_chr[3]}]") %>%
    as.character() %>%
    purrr::set_names(name)

}

#' @rdname smry_meanSD
#' @export
smry_countP <- function(x, use_na = TRUE,
  include_first_cat = TRUE, na_level = 'Missing'){

  stopifnot(is.numeric(x), is.logical(use_na),
    is.logical(include_first_cat), is.character(na_level))

  useNA <- ifelse(use_na, 'always', 'no')

  count_nmr <- table(x, useNA = useNA)

  if(any(is.na(names(count_nmr))))
    names(count_nmr)[is.na(names(count_nmr))] <- na_level

  perct_nmr <- 100 * count_nmr / sum(count_nmr)
  perct_chr <- tblStrings::tbv_round(perct_nmr)

  output <- as.character(glue::glue("{count_nmr} ({perct_chr}%)"))
  names(output) <- names(count_nmr)

  if(!include_first_cat) output[-1] else output

}


#' @rdname smry_meanSD
#' @export
smry_irateCI <- function(time, status, name = "Incidence rate (95% CI)",
  unit = 1000, level = 0.95){

  stopifnot(is.numeric(time), is.numeric(status))

  if(any(is.na(time)) | any(is.na(status))){
    stop("time and status must not contain missing values", call. = FALSE)
  }

  status_check <- all(status %in% c(0,1))

  if(!status_check)
    stop("All status values should be 0 or 1", call. = FALSE)

  time_check <- all(time > 0)

  if(!time_check)
    stop("All time values should be > 0", call. = FALSE)

  time_sum = sum(time)
  event_sum = sum(status)

  est_point <- unit * event_sum / time_sum

  if(event_sum == 0){

    warning("No events were identified in status.", call. = FALSE)

    output <- "0.00 (0.00, 0.00)"
    names(output) <- name
    return(output)

  }

  fit <- stats::glm(
    formula = event_sum ~ 1,
    family = 'poisson',
    offset = log(time_sum)
  )

  est_interval <- suppressMessages(
    unit * exp(stats::confint(fit, level = level))
  )

  tblStrings::pintr(est_point, est_interval[1], est_interval[2]) %>%
    as.character() %>%
    purrr::set_names(name)

}


