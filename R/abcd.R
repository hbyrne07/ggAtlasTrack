#' Show ABCD DTI variable names matched to AtlasTrack tracts
#'
#' @param version ABCD data release version. One of `"6.0"`, `"5.0"`, or `"all"`.
#' @param metric DTI metric suffix to use. Defaults to `"fa"`. Other common
#'   ABCD DTI metrics include `"md"`, `"ld"`, and `"td"`.
#' @param shell ABCD 6.0 DTI shell to use. Defaults to `"fs"` for full shell.
#'   Use `"is"` for inner shell.
#' @param only_plotted Logical. If `TRUE`, return only tracts available in
#'   the `ggAtlasTrack` plotting atlas.
#'
#' @return A data frame mapping ABCD DTI variable names to AtlasTrack tract names.
#'
#' @examples
#' atlastrack_abcd_lookup(metric = "fa")
#' atlastrack_abcd_lookup(metric = "md")
#' atlastrack_abcd_lookup(metric = "fa", shell = "is")
#'
#' @export
atlastrack_abcd_lookup <- function(
    version = c("6.0", "5.0", "all"),
    metric = "fa",
    shell = "fs",
    only_plotted = TRUE
) {

  version <- match.arg(version)

  if (!is.character(metric) || length(metric) != 1) {
    stop(
      "`metric` must be a single character value, such as 'fa' or 'md'.",
      call. = FALSE
    )
  }

  if (!is.character(shell) || length(shell) != 1) {
    stop(
      "`shell` must be a single character value, such as 'fs' or 'is'.",
      call. = FALSE
    )
  }

  metric <- tolower(metric)
  shell <- tolower(shell)

  lookup <- atlastrack_abcd_tract_lookup

  lookup <- lookup |>
    dplyr::mutate(
      ABCD6_var_name = .data$ABCD6_fa_var_name |>
        gsub(
          "__is__",
          paste0("__", shell, "__"),
          x = _,
          fixed = TRUE
        ) |>
        gsub(
          "__fa__",
          paste0("__", metric, "__"),
          x = _,
          fixed = TRUE
        ),
      ABCD5_var_name = gsub(
        "dmri_dtifa_",
        paste0("dmri_dti", metric, "_"),
        .data$ABCD5_fa_var_name,
        fixed = TRUE
      ),
      metric = metric,
      shell = shell
    )

  if (only_plotted) {
    lookup <- lookup |>
      dplyr::filter(.data$in_atlastrack_plot)
  }

  if (version == "6.0") {
    lookup <- lookup |>
      dplyr::filter(!is.na(.data$ABCD6_var_name)) |>
      dplyr::select(
        tract_name,
        tract_label,
        ABCD6_var_name,
        metric,
        shell,
        in_atlastrack_plot
      )
  } else if (version == "5.0") {
    lookup <- lookup |>
      dplyr::filter(!is.na(.data$ABCD5_var_name)) |>
      dplyr::select(
        tract_name,
        tract_label,
        ABCD5_var_name,
        metric,
        in_atlastrack_plot
      )
  } else {
    lookup <- lookup |>
      dplyr::select(
        tract_name,
        tract_label,
        ABCD5_var_name,
        ABCD6_var_name,
        metric,
        shell,
        in_atlastrack_plot
      )
  }

  lookup
}

#' Convert ABCD DTI columns to AtlasTrack long format
#'
#' @param data A data frame containing ABCD DTI variables.
#' @param version ABCD data release version. One of `"6.0"` or `"5.0"`.
#' @param metric DTI metric suffix to use. Defaults to `"fa"`. Other common
#'   ABCD DTI metrics include `"md"`, `"ld"`, and `"td"`.
#' @param shell ABCD 6.0 DTI shell to use. Defaults to `"fs"` for full shell.
#'   Use `"is"` for inner shell. This argument is ignored for ABCD 5.0.
#' @param id_cols Optional character vector of identifier columns to keep,
#'   such as `"src_subject_id"` or `"eventname"`. Supplying `id_cols` is
#'   recommended, especially when the input data contains multiple DTI metrics.
#' @param value_name Name of the output value column. If `NULL`, this defaults
#'   to the value of `metric`.
#' @param only_plotted Logical. If `TRUE`, use only tracts available in
#'   the `ggAtlasTrack` plotting atlas.
#'
#' @return A long-format data frame containing identifier columns,
#'   `tract_name`, `tract_label`, the ABCD variable name, and the DTI value.
#'
#' @examples
#' \dontrun{
#' abcd_fa_long <- atlastrack_abcd_to_long(
#'   abcd_data,
#'   version = "6.0",
#'   metric = "fa",
#'   shell = "fs",
#'   id_cols = c("src_subject_id", "eventname")
#' )
#'
#' abcd_md_long <- atlastrack_abcd_to_long(
#'   abcd_data,
#'   version = "6.0",
#'   metric = "md",
#'   shell = "fs",
#'   id_cols = c("src_subject_id", "eventname")
#' )
#' }
#'
#' @export
atlastrack_abcd_to_long <- function(
    data,
    version = c("6.0", "5.0"),
    metric = "fa",
    shell = "fs",
    id_cols = NULL,
    value_name = NULL,
    only_plotted = TRUE
) {

  version <- match.arg(version)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(metric) || length(metric) != 1) {
    stop(
      "`metric` must be a single character value, such as 'fa' or 'md'.",
      call. = FALSE
    )
  }

  if (!is.character(shell) || length(shell) != 1) {
    stop(
      "`shell` must be a single character value, such as 'fs' or 'is'.",
      call. = FALSE
    )
  }

  metric <- tolower(metric)
  shell <- tolower(shell)

  if (is.null(value_name)) {
    value_name <- metric
  }

  if (!is.character(value_name) || length(value_name) != 1) {
    stop("`value_name` must be a single character value.", call. = FALSE)
  }

  lookup <- atlastrack_abcd_lookup(
    version = version,
    metric = metric,
    shell = shell,
    only_plotted = only_plotted
  )

  abcd_var_col <- if (version == "6.0") {
    "ABCD6_var_name"
  } else {
    "ABCD5_var_name"
  }

  abcd_cols <- lookup[[abcd_var_col]]
  value_cols <- intersect(abcd_cols, names(data))

  if (length(value_cols) == 0) {
    stop(
      "No ABCD ", version, " DTI columns for metric `", metric,
      "` were found in `data`.",
      call. = FALSE
    )
  }

  if (is.null(id_cols)) {
    id_cols <- setdiff(names(data), abcd_cols)
  } else {
    missing_id_cols <- setdiff(id_cols, names(data))

    if (length(missing_id_cols) > 0) {
      stop(
        "The following `id_cols` were not found in `data`: ",
        paste(missing_id_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  data |>
    dplyr::select(
      dplyr::all_of(c(id_cols, value_cols))
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = abcd_var_col,
      values_to = value_name
    ) |>
    dplyr::left_join(
      lookup,
      by = abcd_var_col
    ) |>
    dplyr::select(
      dplyr::all_of(id_cols),
      tract_name,
      tract_label,
      dplyr::all_of(abcd_var_col),
      metric,
      dplyr::all_of(value_name),
      dplyr::everything()
    )
}

#' Convert ABCD 6.0 DTI columns to AtlasTrack long format
#'
#' @param data A data frame containing ABCD 6.0 DTI variables.
#' @param metric DTI metric suffix to use. Defaults to `"fa"`.
#' @param shell ABCD 6.0 DTI shell to use. Defaults to `"fs"` for full shell.
#'   Use `"is"` for inner shell.
#' @param id_cols Optional character vector of identifier columns to keep.
#' @param value_name Name of the output value column. If `NULL`, this defaults
#'   to the value of `metric`.
#' @param only_plotted Logical. If `TRUE`, use only tracts available in
#'   the `ggAtlasTrack` plotting atlas.
#'
#' @return A long-format data frame.
#'
#' @examples
#' \dontrun{
#' abcd_fa_long <- atlastrack_abcd6_to_long(
#'   abcd_data,
#'   metric = "fa",
#'   shell = "fs",
#'   id_cols = c("src_subject_id", "eventname")
#' )
#' }
#'
#' @export
atlastrack_abcd6_to_long <- function(
    data,
    metric = "fa",
    shell = "fs",
    id_cols = NULL,
    value_name = NULL,
    only_plotted = TRUE
) {

  atlastrack_abcd_to_long(
    data = data,
    version = "6.0",
    metric = metric,
    shell = shell,
    id_cols = id_cols,
    value_name = value_name,
    only_plotted = only_plotted
  )
}
