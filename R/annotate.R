




#' Annotation
#'
#' @description Working with labels in R is a little tricky. Sometimes
#'   functions preserve vector attributes, sometimes they don't. Generally,
#'   it's safer to make a separate object that preserves data labels while
#'   you run whatever functions you need to run on the actual data. Using
#'   annotate should make this easier.
#'
#'
#' @param data a data.frame with labelled columns
#'
#' @param labels_as_names a logical value. If `TRUE`, then the returned
#'   object will use labels as names and variable as values. If `FALSE`,
#'   variables will be used as names and labels will be values.
#'
#' @param add_type a logical value. If `TRUE`, then the returned
#'   `tibble` will include a column named `type` that describes
#'   the type of each variable annotated. If `FALSE`, no `type` column
#'   is added.
#'
#' @param all_names a logical value. If `TRUE`, then the returned
#'   object will have length equal to the number of columns in `data`
#'   and will include variable - label pairs even if variables do
#'   not have labels. If `FALSE`, then the returned object will
#'   only include variables that have labels and will have length
#'   equal to the number of labelled columns in `data`.
#'
#' @return
#'
#'  - `annotate_lst` returns a list
#'
#'  - `annotate_chr` returns a character vector
#'
#'  - `annotate_df` returns a [tibble][tibble::tibble-package]
#'
#' @export
#'
#' @examples
#'
#' # make some data to demonstrate function behavior
#' demo_data <- data.frame(a = 1, b = 2, c = 'unmet expectations')
#'
#' # add labels to the data
#' attr(demo_data$a, 'label') <- 'age, years'
#' attr(demo_data$b, 'label') <- 'sex at birth'
#' attr(demo_data$c, 'label') <-  NULL
#' # that darn c is always a rule breaker
#'
#' # include all the columns
#' annotate_df(demo_data, all_names = TRUE)
#' # include only labelled columns
#' annotate_df(demo_data, all_names = FALSE)
#'
#' # swap the name/value role of labels
#' # this matters if you want to splice labels into
#' # dplyr::rename or dplyr::recode
#' annotate_chr(demo_data, labels_as_names = TRUE)
#' annotate_chr(demo_data, labels_as_names = FALSE)
#'
#' # returns a list if you don't provide a suffix
#' annotate_lst(demo_data, labels_as_names = TRUE)
#' annotate_lst(demo_data, labels_as_names = FALSE)
#'
#' annotate_df(iris)
#'
#'

annotate_lst <- function(data, labels_as_names = TRUE, all_names = TRUE){

  vctrs::vec_assert(labels_as_names, logical())
  vctrs::vec_assert(all_names, logical())
  .annotate(data, labels_as_names, all_names)

}

#' @rdname annotate_lst
#' @export
annotate_df <- function(data, all_names = TRUE, add_type = TRUE){

  vctrs::vec_assert(all_names, logical())
  vctrs::vec_assert(add_type, logical())

  .out <- .annotate(data, labels_as_names=FALSE, all_names) %>%
    tibble::enframe(name = 'variable',value = 'label') %>%
    dplyr::mutate(label = as.character(label))

  if(add_type){
    .out <- dplyr::mutate(.out,
      type = purrr::map_chr(variable, ~class(data[[.x]])[1])
    ) %>%
      dplyr::select(variable, type, label)
  }

  .out


}

#' @rdname annotate_lst
#' @export
annotate_chr <- function(data, labels_as_names = TRUE, all_names = TRUE){

  vctrs::vec_assert(labels_as_names, logical())
  vctrs::vec_assert(all_names, logical())

  .annotate(data, labels_as_names, all_names) %>%
    purrr::map_chr(~.x)

}

.annotate <- function(data, labels_as_names, all_names){

  labels <- purrr::map(data, ~attr(.x, 'label'))

  no_label <- purrr::map_lgl(labels, is.null)

  if(all(no_label)){
    warning("there are no labelled columns in data.", call.=FALSE)
    for(i in names(data)) attr(data[[i]], 'label') <- i
    labels <- purrr::map(data, ~attr(.x, 'label'))
  }

  if(all_names){
    labels[no_label] <- names(no_label)[no_label]
  } else {
    labels <- labels[!no_label]
  }

  if(labels_as_names){
    out <- as.list(purrr::set_names(names(labels), labels))
  } else {
    out <- labels
  }

  out

}
