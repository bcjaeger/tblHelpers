
#' Left-center align
#'
#' This function takes care of some tedious stylistic
#'   changes that users may want to automate. Specifically,
#'   left aligning the far left column and center aligning
#'   every other column.
#'
#' @param object a `flextable` object.
#'
#' @return a `flextable` object with left-centered columns
#'
#' @export
#'
#' @examples
#'
#' library(flextable)
#' library(dplyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   slice(1:3) %>%
#'   ft_grouped_by() %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   pad_groups()
#'

align_lc <- function(object) {

  object %>%
    flextable::align(align = "center", part = 'all') %>%
    flextable::align(j = 1, align = 'left', part = 'all')

}


#' Left-center widths
#'
#' @param object a `flextable` object.
#'
#' @param width_l a numeric value indicating the width of the far left
#'   column in the table (i.e., what normally contains labels)
#'
#' @param width_c a numeric value indicating the width of every
#'   column other than the far left column.
#'
#' @return a `flextable` object with adjusted widths
#'
#' @export
#'
#' @examples
#'
#' library(flextable)
#' library(dplyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   slice(1:3) %>%
#'   ft_grouped_by() %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   pad_groups()
#'

width_lc <- function(
  object,
  width_l = 2.00,
  width_c = 1.25
) {

  object %>%
    flextable::width(width = width_c) %>%
    flextable::width(j = 1, width = width_l)

}

#' Flextables
#'
#' the `ft` functions help accelerate workflows involving
#'   [flextable::flextable()]. In particular, grouped data
#'   from the [dplyr::group_by()] package are handled so that
#'   group columns are automatically turned into groups of rows
#'   in the table.
#'
#' @param object a `flextable` object with group names. To create this
#'   type of object, use [ft_grouped_by].
#'
#' @param pad_width a numeric value indicating the amount of space added
#'   to child rows in each group.
#'
#' @param grp_color a character value (or vector of length equal to the
#'   number of groups in `data`) indicating the background color that
#'   will be applied to rows that label groups.
#'
#' @param bold a logical value. If `TRUE`, then the group labels
#'   will be written in bold text.
#'
#' @param italic a logical value. If `TRUE`, then the group labels
#'   will be written in italic text.
#'
#' @return a `flextable` object with grouped rows padded.
#'
#' @export
#'
#' @examples
#'
#' library(flextable)
#' library(dplyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   slice(1:3) %>%
#'   ft_grouped_by() %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   pad_groups()
#'

pad_groups <- function(object, grp_color = 'grey80', pad_width = 10,
  bold = FALSE, italic = TRUE) {

  if(is.null(object$group_names)){
    stop('the input is not a grouped flextable. Did you remember to',
      'use ft_grouped_by to make this object?', call. = FALSE)
  }

  object %>%
    flextable::padding(i=object$group_index, j=1, padding.left=pad_width) %>%
    flextable::bg(i = object$rowlab_index, bg = grp_color[1]) %>%
    flextable::italic(i = object$rowlab_index, italic = italic) %>%
    flextable::bold(i = object$rowlab_index, bold = bold)

}


#' Flextables
#'
#' Simple workflows involving [flextable::flextable()].
#'   In particular, grouped data from the [dplyr::group_by()]
#'   package are handled so that group columns are automatically
#'   turned into groups of rows in the table.
#'
#' @param data a data frame.
#'
#' @param ... variables to group by. This argument does not need to
#'   be specified if `data` are already grouped (see [dplyr::group_by]).
#'
#' @param hide_group_label a logical value. if `TRUE`, group label
#'   will not be rendered, only level/value will be rendered.
#'
#' @return a `flextable` object with an additional `group_names`
#'   object attached.
#'
#' @export
#'
#' @examples
#'
#' library(flextable)
#' library(dplyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   slice(1:3) %>%
#'   ft_grouped_by() %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   pad_groups()
#'

ft_grouped_by <- function(data, ..., hide_group_label = TRUE,
  use_col_labels = FALSE) {

  .groups <- rlang::enquos(...)

  if(!purrr::is_empty(.groups)){
    data <- dplyr::group_by(data, !!!.groups)
  }

  if (!inherits(data, "grouped_df")){

    warning("Data are not grouped. Did you remember to use group_by?",
      call. = FALSE)

    output <- flextable::flextable(data = data)
    output$group_names <- NULL

  } else {

    grp_names <- attr(data, 'groups') %>%
      names() %>%
      setdiff('.rows')

    if(length(grp_names) > 1) stop("only one grouping variable allowed.\n",
      "If you need to use >1 grouping variables, try tidyr::unite().\n",
      "see vignette for examples.",
        call. = FALSE)

    # groups of size one are ignored on purpose.
    # grp_counts is used to identify them.
    grp_counts <- dplyr::count(data)

    grps_of_1 <- grp_counts %>%
      dplyr::filter(n == 1) %>%
      tibble::deframe() %>%
      names()

    grps_of_2plus <- grp_counts %>%
      dplyr::filter(n > 1)

    # as_grouped_data will remove the vector attributes
    # (these are restored later)
    output <- data %>%
      flextable::as_grouped_data(groups = grp_names)

    # remove groups of size 1
    rows_to_remove <- output[[grp_names]] %in% grps_of_1
    output <- output[!rows_to_remove, ]

    # create group index values
    # these are used to access rows that are part of a group
    # notably, the groups of size 1 are ignored.
    grp_index <- purrr::map2(
      .x = grps_of_2plus[[1]],
      .y = grps_of_2plus[[2]],
      .f = ~ which(output[[grp_names]] == .x) + seq(.y)
    )
    # create row label index values.
    # these are used to access the row headers that describe groups.
    rowlab_index <- purrr::map_dbl(grp_index, ~min(.x) - 1)

    grp_index <- grp_index %>%
      purrr::reduce(base::c) %>%
      sort(decreasing = FALSE)

    # after making the initial data, identify any vctrs_vctr objects
    # that are in the dataset.

    vecs <- names(which(purrr::map_lgl(data, inherits, 'vctrs_vctr')))

    # also create an indicator to make it clear what this is for.
    tbl_has_vecs <- !purrr::is_empty(vecs)

    # use vec_restore to re-create the vectors and keep their handy
    # methods and formats accessible for downstream table operations.
    if(tbl_has_vecs){
      for(v in vecs){
        output[[v]] <- vctrs::vec_restore(output[[v]], to = data[[v]])
      }

      values <- annotate_lst(output, labels_as_names = FALSE)

    }

    # with attributes restored, we can convert the grouped data into
    # a flextable - but we stil have the issue of column labels to
    # work through.
    output <- output %>%
      flextable::as_flextable(hide_grouplabel = hide_group_label)

    # restoring vec attributes will also restore the labels that
    # are intended to be used as column names. Set those up here
    # for convenience
    if(use_col_labels && !purrr::is_empty(values)) output <- output %>%
      flextable::set_header_labels(values = values)

    # assign group names to the output so they can be accessed downstream
    output$group_names <- grp_names
    output$group_index <- grp_index
    output$rowlab_index <- rowlab_index

  }

  output

}


#' Flextable
#'
#' @description this is a basic wrapper of [flextable::flextable()] that
#'   gives some additional functionality for data with labelled columns.
#'   The inclusion of a `...` argument allows you to include name-value
#'   pairs for column names and column labels. This function will also
#'   automatically pick up on column labels in `tblString` objects.
#'
#' @param data dataset
#'
#' @param ... name-value pairs for column names and column labels
#'
#' @param col_keys columns names/keys to display. If some column names
#'   are not in the dataset, they will be added as blank columns.
#'
#' @param cwidth initial width to use for cell sizes in inches.
#'
#' @param cheight initial height to use for cell sizes in inches.
#'
#' @param defaults a list of default values for formats, supported options
#'   are fontname, font.size, color and padding.
#'
#' @param use_col_labels a logical value. If `TRUE`, existing `label`
#'   attributes on columns in `data` will be applied to their corresponding
#'   table columns.
#'
#' @param theme_fun a function theme to apply before returning the
#'   flextable. set to NULL for none.
#'
#' @return a `flextable` object
#'
#' @export
#'
#' @examples
#'
#' # default flextable behavior
#' ft(iris[1:5, ])
#' # adding labels in flextable call
#' ft(iris[1:5, ], 'Sepal Width' = Sepal.Width, cwidth = 1.5)
#'
ft <- function(
  data,
  ...,
  col_keys = names(data),
  cwidth = 0.75,
  cheight = 0.25,
  defaults = list(),
  use_col_labels = TRUE,
  theme_fun = flextable::theme_booktabs) {

  .dot_labs <- rlang::ensyms(...)

  # if a user supplies labels through ..., put them in data
  if(!purrr::is_empty(.dot_labs)){

    .lab_vals <- names(.dot_labs)
    .lab_keys <- purrr::set_names(.dot_labs, NULL) %>%
      purrr::map_chr(.f = ~ if(is.symbol(.x)) {deparse(.x)} else{.x})

    # if the supplied column names aren't actually in the table data
    # throw an informative error message
    if( !all( .lab_keys %in% col_keys ) ){
      mismatched_names <- setdiff(.lab_keys, col_keys)
      stop("Column names unrecognized: ", list_things(mismatched_names))
    }

    # loop along label keys to set label values in data
    for(i in seq_along(.lab_keys)){
      attr(data[[ .lab_keys[i] ]], 'label') <- .lab_vals[i]
    }

  }

  output <- flextable::flextable(
    data      = data,
    col_keys  = col_keys,
    cwidth    = cwidth,
    cheight   = cheight,
    defaults  = defaults,
    theme_fun = theme_fun
  )

  if(use_col_labels){

    # look for labels in data
    labs <- purrr::map(data, ~attr(.x, 'label')) %>%
      purrr::discard(is.null)

    # if the data have labels:
    if(!purrr::is_empty(labs)){

      # get labels from the data if there are any
      values <- annotate_lst(data, labels_as_names = FALSE)
      # set those into the flextable here
      output <- output %>%
        flextable::set_header_labels(values = values)

    }

  }



  output$group_names <- NULL
  output

}



#' Flextable headers
#'
#' This function helps to simplify table headers, assuming
#'   the table's `col_keys` have the following structure:
#'
#'   ||LOWER COLUMN NAME|| ||PATTERN|| ||HIGHER COLUMN NAME|| ||PATTERN|| ...
#'
#'   for example, 'sepal_width' will be split into sepal and width if
#'   the pattern is '_' and then a lower header row will feature the
#'   text 'sepal', and an upper header row will show 'width'.
#'
#'   But what are `col_keys` of a `flextable`? By default, they are
#'   names of the `data` used to make the `flextable`. For a grouped
#'   `flextable`, group names are not included in `col_keys`.
#'
#'   This function also lets you enter mappings from the column names
#'   in your data to the desired column headers you'd like to see in
#'   the table.
#'
#' @param object a `flextable` object
#'
#' @param ... named inputs of the form `<table header label>` =
#'   `<column name chunk>`. Examples given below should make this
#'   a little more clear.
#'
#' @param pattern a string indicating the pattern that separates
#'   column header patterns in your `flextable`'s `col_keys`.
#'
#' @param front_to_back a logical value. If `TRUE`, then the table headers
#'   are constructed from front to back, meaning variable names `a_b` and
#'   `a_c` will be turned into a header value `a` comprising lower rows
#'   `b` and `c`. If `FALSE`, then the names `b_a` and `c_a` would be needed
#'   to make the same structure.
#'
#'
#' @return a `flextable` object
#' @export
#'
#' @examples
#'
#' df <- data.frame(a_c = c("made", "column"), b_c = c("with", "names"))
#' ft <- flextable::flextable(df)
#' ft <- header_split(ft, pattern = '_',
#'   c = 'Headers', a = 'are', b = 'all')
#' ft <- flextable::theme_box(ft)
#' ft <- flextable::align(ft, align = 'center', part = 'all')
#' ft
#'

header_split <- function(object, ..., pattern = "_", front_to_back = TRUE){

  .dots <- rlang::enquos(...)

  # make the mapping dataset that will be passed into
  # flextables set_header_df function.
  mapping <- object$col_keys %>%
    stringr::str_split(pattern = pattern, simplify = TRUE) %>%
    tibble::as_tibble()

  mapping <- mapping %>%
    dplyr::mutate(col_keys = object$col_keys) %>%
    dplyr::select(col_keys, dplyr::everything())

  # needed to put col_keys as the first variable for
  # the following loop to be reliable.
  for (i in seq(3, ncol(mapping))) {

    blanks <- mapping[[i]] == ''
    if(any(blanks)) mapping[blanks, i] <- mapping[blanks, 2L]

  }

  # If user supplies labels for the chunks in their names,
  # apply the recoding function to map them in here.
  if(!purrr::is_empty(.dots)){
    mapping <- mapping %>%
      dplyr::mutate_at(
        .vars = dplyr::vars(dplyr::starts_with("V")),
        .funs = ~dplyr::recode(.x, !!!.dots)
      )
  }

  # the names of mapping need to be flipped to make it so that
  # lower columns come first in name - pattern - name sequence.
  mapping <- mapping[, c('col_keys', rev(names(mapping)[-1]))]

  # call on the merge functions because I can't imagine why you wouldnt.
  # should this be make optional by a logical input?
  flextable::set_header_df(object, mapping = mapping, key = 'col_keys') %>%
    flextable::merge_v(part = 'header') %>%
    flextable::merge_h(part = 'header')

}


#' Flip names
#'
#' @description This function is a convenient re-name tool in case
#'   you want to use [header_split] but the names of your variables
#'   are set up in the opposite order of what [header_split] expects.
#'
#' @param df a data frame
#' @param pattern a string indicating the pattern that separates
#'   column header patterns
#'
#' @return a data frame with names flipped around `pattern`
#' @export
#'
#' @examples
#'
#' df = data.frame(a_c = 1, a_b = 2)
#'
#' flip_names(df, '_')
#'
#'
flip_names <- function(df, pattern){

  new_names <- names(df) %>%
    stringr::str_split(pattern = pattern) %>%
    purrr::map(rev) %>%
    purrr::map_chr(paste, collapse = pattern)

  purrr::set_names(df, new_names)

}


