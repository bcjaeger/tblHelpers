
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
#'   as_grouped_ft() %>%
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
#'   as_grouped_ft() %>%
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
#'   type of object, use [as_grouped_ft].
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
#'   as_grouped_ft() %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   pad_groups()
#'

pad_groups <- function(object, grp_color = 'grey80', pad_width = 5,
  bold = FALSE, italic = TRUE) {

  if(is.null(object$group_names)){
    stop('the input does not have ft group names. Did you remember to',
      'use group_by before making this ft?', call. = FALSE)
  }

  grp_names <- object$group_names
  n_grp <- length(grp_names)

  if(length(grp_color) == 1){

    grp_color <- rep(grp_color, n_grp)

  }

  if (length(grp_color) != n_grp){

    stop(
      "Specify one overall grp_color or one color per group",
      call. = FALSE
    )

  }

  # copy object into output1, then modify output1
  # object <- object

  for(i in seq_along(grp_names)){

    grp_name <- grp_names[i]

    grp_index_expr <- rlang::parse_expr(
      glue::glue('~!is.na({grp_name})')
    )

    padding_call <- rlang::call2(
      .fn = flextable::padding,
      x = object,
      i = grp_index_expr,
      j = 1,
      padding.left = pad_width
    )

    output2 <- rlang::eval_tidy(expr = padding_call)

    bg_call <- rlang::call2(
      .fn = flextable::bg,
      x = output2,
      i = grp_index_expr,
      bg = grp_color[i]
    )

    output3 <- rlang::eval_tidy(expr = bg_call)

    italic_call <- rlang::call2(
      .fn = flextable::italic,
      x = output3,
      i = grp_index_expr,
      italic = italic
    )

    output4 <- rlang::eval_tidy(expr = italic_call)

    bold_call <- rlang::call2(
      .fn = flextable::bold,
      x = output4,
      i = grp_index_expr,
      bold = bold
    )

    output5 <- rlang::eval_tidy(expr = italic_call)

    pad_width <- pad_width + pad_width

    object <- output5

  }

  # pad the rows that are contained in groups

  # first identify the index of these rows as an expression
  grp_index_expr <- glue::glue("is.na({grp_names})") %>%
    glue::glue_collapse(sep = ' & ') %>%
    paste("~", .) %>%
    rlang::parse_expr()

  # pass the expression into flextable's padding function
  padding_call <- rlang::call2(
    .fn = flextable::padding,
    x = object,
    i = grp_index_expr,
    j = 1,
    padding.left = pad_width
  )

  rlang::eval_tidy(exp = padding_call)

}


#' Flextables
#'
#' Simple workflows involving [flextable::flextable()].
#'   In particular, grouped data from the [dplyr::group_by()]
#'   package are handled so that group columns are automatically
#'   turned into groups of rows in the table.
#'
#' @param data a grouped dataset.
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
#'   as_grouped_ft() %>%
#'   theme_vanilla() %>%
#'   align_lc() %>%
#'   pad_groups()
#'

as_grouped_ft <- function(data, hide_group_label = TRUE) {

  if (!inherits(data, "grouped_df")){

    warning("Data are not grouped. Did you remember to use group_by?",
      call. = FALSE)

    output <- flextable::flextable(data = data)
    output$group_names <- NULL

  } else {

    grp_names <- attr(data, 'groups') %>%
      names() %>%
      setdiff('.rows')

    output <- data %>%
      flextable::as_grouped_data(groups = grp_names) %>%
      flextable::as_flextable(hide_grouplabel = hide_group_label)

    output$group_names <- grp_names


  }

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
#' @param pattern a string indicating the pattern that separates
#'   column header patterns in your `flextable`'s `col_keys`,
#' @param ... named inputs of the form ||table header label|| =
#'   ||column name chunk||. Examples given below should make this
#'   a little more clear.
#'
#' @return a `flextable` object
#' @export
#'
#' @examples
#'
#' df <- data.frame(a_c = c("easy", "make"), b_c = c("to", "now!"))
#' ft <- flextable::flextable(df)
#' ft <- header_split(ft, pattern = '_', c = 'Headers', a = 'are', b = 'so')
#' ft <- flextable::theme_box(ft)
#' ft <- flextable::align(ft, align = 'center', part = 'all')
#' ft
#'
#'
header_split <- function(object, pattern = "_", ...){

  .dots <- rlang::enquos(...)

  # make the mapping dataset that will be passed into
  # flextables set_header_df function.
  mapping <- object$col_keys %>%
    stringr::str_split(pattern = pattern, simplify = TRUE) %>%
    tibble::as_tibble() %>%
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



