#' Data tabulation
#'
#' @description Tabulating a data frame leads to a summary
#'   frame with rows that correspond to columns of the original
#'   data. Different functions can be applied to the same variable
#'   to create a set of summaries (see examples).
#'
#' @param data a data frame to tabulate.
#'
#' @param ... a collection of named arguments with left hand side
#'   giving the name of the summarized value and right hand side
#'   giving an expression to be applied in `data`.
#'
#' @param names_sep a character value that will be used to separate
#'   the names of groups in column names of the output. If there is
#'   only one group, this parameter has no impact.
#'
#' @param include_overall a logical value. If `TRUE`, then the
#'   output will include a column named `'Overall'` that replicates
#'   the functions supplied in `...` in the entire sample.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#'
#' library(flextable)
#' library(dplyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   dabulate(
#'     `Sepal length` = smry_meanSD(Sepal.Length),
#'     `Sepal length` = smry_medianIQR(Sepal.Length),
#'     `Sepal width`  = smry_meanSD(Sepal.Width),
#'     `Sepal width`  = smry_medianIQR(Sepal.Width)
#'   ) %>%
#'   ft_grouped_by(variable) %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   autofit() %>%
#'   pad_groups(grp_color = 'white')


dabulate <- function(data, ..., names_sep = '_', include_overall = TRUE) {

  grp_vars <- dplyr::group_vars(data)

  exprs <- rlang::enquos(...) %>%
    purrr::map(rlang::quo_get_expr)

  if(purrr::is_empty(grp_vars) | include_overall){

    output_overall <- .dabulate_worker(
      data = dplyr::ungroup(data),
      exprs = exprs
    ) %>%
      dplyr::rename(Overall = value)

    if(purrr::is_empty(grp_vars)) return(output_overall)

  }

  one_group <- length(grp_vars) == 1

  name <- if(one_group) grp_vars else 'name'

  output_groups <- split(data, f = data[, grp_vars], sep = '._.') %>%
    purrr::map(.dabulate_worker, exprs = exprs) %>%
    tibble::enframe(name = name)

  if(!one_group){
    output_groups <- output_groups %>%
      tidyr::separate(col = 'name', into = grp_vars, sep = '._.')
  }

  output_groups <- output_groups %>%
    tidyr::unnest(cols = value) %>%
    tidyr::unnest(cols = value) %>%
    tidyr::pivot_wider(
      names_from = grp_vars,
      values_from = value,
      names_sep = names_sep
    )

  if(include_overall){

    output <- dplyr::left_join(
      x = output_overall,
      y = output_groups,
      by = c('variable', 'level')
    )

  } else {

    output <- output_groups

  }

  output

}

.dabulate_worker <- function(data, exprs){

  purrr::map(exprs, rlang::eval_tidy, data = data) %>%
    purrr::map(tibble::enframe, name = 'level') %>%
    purrr::map(dplyr::mutate, level = as.character(level)) %>%
    dplyr::bind_rows(.id = 'variable')

}

