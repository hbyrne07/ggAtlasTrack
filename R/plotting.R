#' Plot AtlasTrack white-matter tracts
#'
#' @param data Optional data frame containing tract-level values.
#' @param value Optional unquoted column name in `data` to map to tract fill.
#' @param tract_name Name of the tract-name column in `data`.
#' @param view_set View set to plot. One of `"orthogonal"` or `"ggseg"`.
#' @param show_ventricles Logical. Whether to show ventricles.
#' @param low Low colour for continuous values.
#' @param high High colour for continuous values.
#' @param limits Optional numeric limits for continuous scale.
#' @param legend_title Optional legend title.
#'
#' @return A `ggplot` object.
#' @export
atlastrack_plot <- function(
    data = NULL,
    value = NULL,
    tract_name = "tract_name",
    view_set = c("orthogonal", "ggseg", "axial"),
    show_ventricles = TRUE,
    low = "#E8EEF7",
    high = "#2166AC",
    limits = NULL,
    legend_title = NULL
) {

  view_set <- match.arg(view_set)
  value_quo <- rlang::enquo(value)

  # ============================================================
  # Filter to requested view set
  # ============================================================

  tract_sf <- atlastrack_tracts |>
    dplyr::filter(.data$view_set == view_set)

  bg_gm <- atlastrack_background$gm |>
    dplyr::filter(.data$view_set == view_set)

  bg_wm <- atlastrack_background$wm |>
    dplyr::filter(.data$view_set == view_set)

  bg_vent <- atlastrack_background$vent |>
    dplyr::filter(.data$view_set == view_set)

  if (nrow(tract_sf) == 0) {
    stop("No AtlasTrack tract polygons found for view_set = ", view_set)
  }

  # ============================================================
  # Panel order and labels
  # ============================================================

  make_panel_label <- function(x) {
    dplyr::case_when(
      x == "upper_axial" ~ "upper\naxial",
      x == "lower_axial" ~ "lower\naxial",
      x == "inferior_axial" ~ "inferior\naxial",
      x == "middle_axial" ~ "middle\naxial",
      x == "superior_axial" ~ "superior\naxial",
      x == "axial" ~ "axial",
      x == "coronal" ~ "coronal",
      x == "sagittal" ~ "sagittal",
      TRUE ~ x
    )
  }

  panel_levels <- if (view_set == "orthogonal") {
    c("axial", "coronal", "sagittal")
  } else if (view_set == "ggseg") {
    c("upper\naxial", "coronal", "lower\naxial")
  } else if (view_set == "inferior") {
    c("axial", "coronal", "inferior\naxial")
  } else if (view_set == "axial") {
    c("inferior\naxial", "middle\naxial", "superior\naxial")
  }

  add_panel_label <- function(x) {
    x |>
      dplyr::mutate(
        panel_label = make_panel_label(.data$display_view),
        panel_label = factor(.data$panel_label, levels = panel_levels)
      ) |>
      dplyr::filter(!is.na(.data$panel_label))
  }

  tract_sf <- add_panel_label(tract_sf)
  bg_gm <- add_panel_label(bg_gm)
  bg_wm <- add_panel_label(bg_wm)
  bg_vent <- add_panel_label(bg_vent)

  # ============================================================
  # Base brain background
  # ============================================================

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = bg_gm,
      fill = "grey74",
      colour = NA
    ) +
    ggplot2::geom_sf(
      data = bg_wm,
      fill = "white",
      colour = "grey82",
      linewidth = 0.08
    )

  # ============================================================
  # Categorical atlas plot OR continuous value plot
  # ============================================================

  if (rlang::quo_is_null(value_quo) || is.null(data)) {

    tract_levels <- sort(unique(tract_sf$region))

    tract_cols <- stats::setNames(
      grDevices::hcl.colors(
        length(tract_levels),
        palette = "Dark 3"
      ),
      tract_levels
    )

    p <- p +
      ggplot2::geom_sf(
        data = tract_sf,
        ggplot2::aes(fill = .data$region),
        colour = NA
      ) +
      ggplot2::scale_fill_manual(
        values = tract_cols,
        na.value = "grey74",
        name = ifelse(is.null(legend_title), "Tract", legend_title),
        drop = FALSE
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          nrow = 3,
          byrow = TRUE,
          keywidth = grid::unit(0.35, "cm"),
          keyheight = grid::unit(0.35, "cm"),
          override.aes = list(colour = NA)
        )
      )

  } else {

    data <- data |>
      dplyr::rename(
        .atlastrack_join_tract = dplyr::all_of(tract_name)
      )

    plot_data <- tract_sf |>
      dplyr::left_join(
        data,
        by = c("tract_name" = ".atlastrack_join_tract")
      )

    p <- p +
      ggplot2::geom_sf(
        data = plot_data,
        ggplot2::aes(fill = !!value_quo),
        colour = "grey96",
        linewidth = 0.05
      ) +
      ggplot2::scale_fill_gradient(
        low = low,
        high = high,
        limits = limits,
        na.value = "grey74",
        name = legend_title
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(
          direction = "horizontal",
          title.position = "left",
          title.hjust = 0.5,
          barwidth = grid::unit(5.5, "cm"),
          barheight = grid::unit(0.35, "cm")
        )
      )
  }

  # ============================================================
  # Ventricles
  # ============================================================

  if (show_ventricles) {
    p <- p +
      ggplot2::geom_sf(
        data = bg_vent,
        fill = "grey55",
        colour = NA
      )
  }

  # ============================================================
  # Facets and theme
  # ============================================================

  p +
    ggplot2::facet_wrap(
      ~ panel_label,
      nrow = 1,
      drop = TRUE
    ) +
    ggplot2::coord_sf(
      datum = NA,
      expand = FALSE
    ) +
    ggplot2::labs(
      title = NULL
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        family = "mono",
        size = 7,
        colour = "grey30",
        margin = ggplot2::margin(t = 2, b = 2)
      ),
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      panel.spacing = grid::unit(0.4, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(
        family = "mono",
        size = 7,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        family = "mono",
        size = 6
      ),
      legend.key.height = grid::unit(0.35, "cm"),
      legend.key.width = grid::unit(0.35, "cm"),
      legend.box.margin = ggplot2::margin(t = -3),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}
