# usethis::use_data(build_atlastrack_2d, overwrite = TRUE)

# ============================================================
# Build AtlasTrack 2D package data
# ============================================================
# This script reads Python-exported contour CSVs, converts them to sf objects,
# builds ggseg-style background layers, and saves package data objects:
#
#   atlastrack_tracts
#   atlastrack_background
#   atlastrack_views
#
# ============================================================

library(dplyr)
library(readr)
library(sf)
library(smoothr)
library(purrr)
library(tibble)

# ============================================================
# Paths to Python-exported contour CSVs
# ============================================================

gm_path <- file.path("data-raw", "csv", "MNI_gm_contours_2D.csv")
wm_bg_path <- file.path("data-raw", "csv", "MNI_wm_contours_2D.csv")
vent_path <- file.path("data-raw", "csv", "MNI_ventricular_csf_contours_2D.csv")
tract_path <- file.path("data-raw", "csv", "AtlasTrack_slice_contours_slab_thr_0.30_THICK.csv")

# Check files exist

csv_paths <- c(
  gm_path,
  wm_bg_path,
  vent_path,
  tract_path
)

missing_paths <- csv_paths[!file.exists(csv_paths)]

if (length(missing_paths) > 0) {
  stop(
    "The following CSV files could not be found:\n",
    paste(missing_paths, collapse = "\n")
  )
}

# ============================================================
# Read raw contour CSVs
# ============================================================

gm_all <- readr::read_csv(
  gm_path,
  show_col_types = FALSE
)

wm_bg_all <- readr::read_csv(
  wm_bg_path,
  show_col_types = FALSE
)

vent_all <- readr::read_csv(
  vent_path,
  show_col_types = FALSE
)

wm_all <- readr::read_csv(
  tract_path,
  show_col_types = FALSE
)

# ============================================================
# Helper functions
# ============================================================

make_poly_sf <- function(df, group_cols) {

  if (nrow(df) == 0) {
    stop("make_poly_sf(): input data frame has zero rows.")
  }

  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      x = list(x),
      y = list(y),
      .groups = "drop"
    ) %>%
    mutate(
      geometry = purrr::map2(x, y, ~ {

        xy <- cbind(.x, .y)

        if (nrow(xy) < 3) {
          return(st_polygon())
        }

        if (!all(xy[1, ] == xy[nrow(xy), ])) {
          xy <- rbind(xy, xy[1, ])
        }

        st_polygon(list(xy))
      })
    ) %>%
    select(-x, -y) %>%
    st_as_sf() %>%
    st_make_valid() %>%
    filter(!st_is_empty(geometry))
}


drop_small_polys <- function(x, min_area = 20) {

  x %>%
    st_make_valid() %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    filter(area >= min_area) %>%
    select(-area)
}


keep_largest_per_slice <- function(sf_obj) {

  sf_obj %>%
    st_make_valid() %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    group_by(
      view_set,
      display_view,
      view_order,
      view,
      slice_mm
    ) %>%
    slice_max(
      area,
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    select(-area)
}


make_outer_line <- function(poly_sf) {

  poly_sf <- poly_sf %>%
    st_make_valid() %>%
    filter(!st_is_empty(geometry))

  if (nrow(poly_sf) == 0) {
    warning("make_outer_line(): no non-empty polygons available.")
    return(
      st_sf(
        view_set = character(),
        display_view = character(),
        view = character(),
        view_order = numeric(),
        slice_mm = numeric(),
        geometry = st_sfc()
      )
    )
  }

  poly_sf %>%
    mutate(
      geometry = st_boundary(geometry)
    ) %>%
    st_as_sf() %>%
    st_make_valid()
}


centre_sf_by_display_view <- function(sf_obj, ref_obj) {

  centres <- ref_obj %>%
    group_by(
      view_set,
      display_view,
      view_order
    ) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_make_valid() %>%
    mutate(
      bb = purrr::map(geometry, st_bbox),
      x_mid = purrr::map_dbl(
        bb,
        ~ mean(c(.x["xmin"], .x["xmax"]))
      ),
      y_mid = purrr::map_dbl(
        bb,
        ~ mean(c(.x["ymin"], .x["ymax"]))
      )
    ) %>%
    st_drop_geometry() %>%
    select(
      view_set,
      display_view,
      view_order,
      x_mid,
      y_mid
    )

  sf_obj %>%
    st_make_valid() %>%
    left_join(
      centres,
      by = c(
        "view_set",
        "display_view",
        "view_order"
      )
    ) %>%
    group_by(
      view_set,
      display_view,
      view_order
    ) %>%
    group_modify(~ {

      x_shift <- unique(.x$x_mid)
      y_shift <- unique(.x$y_mid)

      .x <- .x %>%
        select(
          -x_mid,
          -y_mid
        )

      st_geometry(.x) <- st_geometry(.x) + c(-x_shift, -y_shift)

      .x
    }) %>%
    ungroup() %>%
    st_as_sf() %>%
    st_make_valid()
}


make_display_view_label <- function(x) {

  dplyr::case_when(
    x == "upper_axial" ~ "upper\naxial",
    x == "lower_axial" ~ "lower\naxial",
    x == "axial" ~ "axial",
    x == "coronal" ~ "coronal",
    x == "sagittal" ~ "sagittal",
    x == "upper_coronal" ~ "upper\ncoronal",
    x == "lower_coronal" ~ "lower\ncoronal",
    TRUE ~ x
  )
}


add_display_labels <- function(sf_obj) {

  sf_obj %>%
    mutate(
      display_view_label = make_display_view_label(display_view),
      display_view_label = factor(
        display_view_label,
        levels = unique(
          make_display_view_label(
            display_view[order(view_order)]
          )
        )
      )
    )
}


shift_coronal_layer <- function(sf_obj, y_shift = -2) {

  sf_obj <- sf_obj %>%
    st_make_valid()

  is_coronal <- sf_obj$display_view == "coronal"

  if (any(is_coronal)) {
    st_geometry(sf_obj[is_coronal, ]) <-
      st_geometry(sf_obj[is_coronal, ]) + c(0, y_shift)
  }

  sf_obj %>%
    st_make_valid()
}


make_ggseg_background <- function(
    gm_obj,
    wm_obj,
    vent_obj,
    gm_close = 1.0,
    gm_expand = 0.20,
    gm_smoothness = 3,
    gm_simplify = 0.4,
    wm_expand = 0.25,
    wm_smoothness = 5,
    wm_simplify = 0.6,
    vent_expand = 0,
    vent_smoothness = 2
) {

  gm_bg <- gm_obj %>%
    st_make_valid() %>%
    group_by(
      view_set,
      display_view,
      display_view_label,
      view_order,
      view,
      slice_mm
    ) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_make_valid() %>%
    st_buffer(gm_close) %>%
    smoothr::smooth(
      method = "ksmooth",
      smoothness = gm_smoothness
    ) %>%
    st_buffer(-gm_close) %>%
    st_buffer(gm_expand) %>%
    st_simplify(
      dTolerance = gm_simplify,
      preserveTopology = TRUE
    ) %>%
    st_make_valid()

  wm_cutout <- wm_obj %>%
    st_make_valid() %>%
    group_by(
      view_set,
      display_view,
      display_view_label,
      view_order,
      view,
      slice_mm
    ) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_make_valid() %>%
    st_buffer(wm_expand) %>%
    smoothr::smooth(
      method = "ksmooth",
      smoothness = wm_smoothness
    ) %>%
    st_simplify(
      dTolerance = wm_simplify,
      preserveTopology = TRUE
    ) %>%
    st_make_valid()

  vent_bg <- vent_obj %>%
    st_make_valid() %>%
    group_by(
      view_set,
      display_view,
      display_view_label,
      view_order,
      view,
      slice_mm
    ) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_make_valid() %>%
    st_buffer(vent_expand) %>%
    smoothr::smooth(
      method = "ksmooth",
      smoothness = vent_smoothness
    ) %>%
    st_make_valid()

  list(
    gm = gm_bg,
    wm = wm_cutout,
    vent = vent_bg
  )
}


# ============================================================
# Build one view set
# ============================================================

build_atlastrack_view_set <- function(
    plot_view_set,
    gm_all,
    wm_bg_all,
    vent_all,
    wm_all,
    coronal_y_shift = -2
) {

  message("Building view set: ", plot_view_set)

  gm <- gm_all %>%
    filter(view_set == plot_view_set)

  wm_bg <- wm_bg_all %>%
    filter(view_set == plot_view_set)

  vent <- vent_all %>%
    filter(view_set == plot_view_set)

  wm <- wm_all %>%
    filter(view_set == plot_view_set)

  if (nrow(gm) == 0) {
    stop("No GM rows found for view_set = ", plot_view_set)
  }

  if (nrow(wm_bg) == 0) {
    stop("No WM background rows found for view_set = ", plot_view_set)
  }

  if (nrow(vent) == 0) {
    stop("No ventricle rows found for view_set = ", plot_view_set)
  }

  if (nrow(wm) == 0) {
    stop("No tract rows found for view_set = ", plot_view_set)
  }

  message("GM views:")
  print(
    gm %>%
      distinct(
        view_set,
        display_view,
        view,
        slice_mm,
        view_order
      )
  )

  message("Tract views:")
  print(
    wm %>%
      distinct(
        view_set,
        display_view,
        view,
        slice_mm,
        view_order
      )
  )

  # ------------------------------------------------------------
  # Build polygons
  # ------------------------------------------------------------

  gm_poly <- make_poly_sf(
    gm,
    c(
      "view_set",
      "display_view",
      "view",
      "view_order",
      "slice_mm",
      "polygon_id"
    )
  )

  wm_bg_poly <- make_poly_sf(
    wm_bg,
    c(
      "view_set",
      "display_view",
      "view",
      "view_order",
      "slice_mm",
      "polygon_id"
    )
  )

  vent_poly <- make_poly_sf(
    vent,
    c(
      "view_set",
      "display_view",
      "view",
      "view_order",
      "slice_mm",
      "polygon_id"
    )
  )

  wm_poly <- make_poly_sf(
    wm,
    c(
      "tract_id",
      "tract_name",
      "view_set",
      "display_view",
      "view",
      "view_order",
      "slice_mm",
      "polygon_id"
    )
  )

  # ------------------------------------------------------------
  # Remove tiny polygons
  # ------------------------------------------------------------

  gm_poly <- drop_small_polys(
    gm_poly,
    min_area = 30
  )

  vent_poly <- drop_small_polys(
    vent_poly,
    min_area = 10
  )

  wm_poly <- drop_small_polys(
    wm_poly,
    min_area = 2
  )

  wm_bg_poly <- drop_small_polys(
    wm_bg_poly,
    min_area = 100
  )

  # ------------------------------------------------------------
  # Smooth polygons
  # ------------------------------------------------------------

  gm_smooth <- smoothr::smooth(
    gm_poly,
    method = "ksmooth",
    smoothness = 3
  ) %>%
    st_make_valid() %>%
    st_buffer(0.5) %>%
    st_make_valid()

  wm_bg_smooth <- smoothr::smooth(
    wm_bg_poly,
    method = "ksmooth",
    smoothness = 3
  ) %>%
    st_make_valid()

  vent_smooth <- vent_poly %>%
    st_make_valid() %>%
    smoothr::smooth(
      method = "ksmooth",
      smoothness = 2
    ) %>%
    st_make_valid()

  wm_smooth <- wm_poly %>%
    st_make_valid() %>%
    smoothr::smooth(
      method = "ksmooth",
      smoothness = 3
    ) %>%
    st_make_valid()

  # ------------------------------------------------------------
  # WM cut-out and main WM background
  # ------------------------------------------------------------

  wm_bg_cutout <- wm_bg_smooth %>%
    drop_small_polys(min_area = 60) %>%
    st_make_valid()

  wm_bg_main <- wm_bg_smooth %>%
    keep_largest_per_slice() %>%
    st_buffer(-0.5) %>%
    st_make_valid()

  # ------------------------------------------------------------
  # Optional outer boundaries
  # ------------------------------------------------------------

  gm_outer_poly <- gm_smooth %>%
    group_by(
      view_set,
      display_view,
      view_order,
      view,
      slice_mm
    ) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_as_sf() %>%
    st_make_valid() %>%
    keep_largest_per_slice()

  gm_outer_line <- make_outer_line(gm_outer_poly)
  wm_bg_outer_line <- make_outer_line(wm_bg_main)

  # ------------------------------------------------------------
  # Centre layers
  # ------------------------------------------------------------

  gm_smooth_c <- centre_sf_by_display_view(
    gm_smooth,
    gm_smooth
  )

  wm_bg_main_c <- centre_sf_by_display_view(
    wm_bg_main,
    gm_smooth
  )

  wm_bg_cutout_c <- centre_sf_by_display_view(
    wm_bg_cutout,
    gm_smooth
  )

  vent_smooth_c <- centre_sf_by_display_view(
    vent_smooth,
    gm_smooth
  )

  wm_values_c <- centre_sf_by_display_view(
    wm_smooth,
    gm_smooth
  )

  gm_outer_line_c <- centre_sf_by_display_view(
    gm_outer_line,
    gm_smooth
  )

  wm_bg_outer_line_c <- centre_sf_by_display_view(
    wm_bg_outer_line,
    gm_smooth
  )

  # ------------------------------------------------------------
  # Add display labels
  # ------------------------------------------------------------

  gm_smooth_c <- add_display_labels(gm_smooth_c)
  wm_bg_main_c <- add_display_labels(wm_bg_main_c)
  wm_bg_cutout_c <- add_display_labels(wm_bg_cutout_c)
  vent_smooth_c <- add_display_labels(vent_smooth_c)
  wm_values_c <- add_display_labels(wm_values_c)
  gm_outer_line_c <- add_display_labels(gm_outer_line_c)
  wm_bg_outer_line_c <- add_display_labels(wm_bg_outer_line_c)

  # ------------------------------------------------------------
  # Add ggseg-style atlas columns to tract layer
  # ------------------------------------------------------------

  wm_values_c <- wm_values_c %>%
    mutate(
      atlas = "atlastrack",
      type = "white_matter",
      hemi = "bilateral",
      side = display_view,
      region = tract_name,
      label = tract_name
    )

  # ------------------------------------------------------------
  # Shift coronal WM cut-out, tracts, and ventricles together
  # ------------------------------------------------------------

  wm_bg_cutout_shifted_c <- shift_coronal_layer(
    wm_bg_cutout_c,
    y_shift = coronal_y_shift
  )

  wm_values_shifted_c <- shift_coronal_layer(
    wm_values_c,
    y_shift = coronal_y_shift
  )

  vent_smooth_shifted_c <- shift_coronal_layer(
    vent_smooth_c,
    y_shift = coronal_y_shift
  )

  # ------------------------------------------------------------
  # Build ggseg-style background
  # ------------------------------------------------------------

  ggseg_bg <- make_ggseg_background(
    gm_obj = gm_smooth_c,
    wm_obj = wm_bg_cutout_shifted_c,
    vent_obj = vent_smooth_shifted_c,
    gm_close = 1.0,
    gm_expand = 0.20,
    gm_smoothness = 3,
    gm_simplify = 0.4,
    wm_expand = 0.25,
    wm_smoothness = 5,
    wm_simplify = 0.8,
    vent_expand = 0,
    vent_smoothness = 2
  )

  # ------------------------------------------------------------
  # Return processed layers for this view set
  # ------------------------------------------------------------

  list(
    tracts = wm_values_shifted_c,
    background = ggseg_bg,
    outer_lines = list(
      gm = gm_outer_line_c,
      wm = wm_bg_outer_line_c
    )
  )
}


# ============================================================
# Build all available view sets
# ============================================================

preferred_view_sets <- c(
  "ggseg",
  "orthogonal"
)

available_view_sets <- preferred_view_sets[
  preferred_view_sets %in% unique(gm_all$view_set)
]

if (length(available_view_sets) == 0) {
  stop("No recognised view sets found in the contour CSVs.")
}

if (!all(preferred_view_sets %in% available_view_sets)) {
  warning(
    "Not all preferred view sets were found. Available view sets are: ",
    paste(available_view_sets, collapse = ", ")
  )
}

built_view_sets <- purrr::map(
  available_view_sets,
  build_atlastrack_view_set,
  gm_all = gm_all,
  wm_bg_all = wm_bg_all,
  vent_all = vent_all,
  wm_all = wm_all,
  coronal_y_shift = -2
)

names(built_view_sets) <- available_view_sets

# ============================================================
# Combine final package data objects
# ============================================================

atlastrack_tracts <- purrr::map(
  built_view_sets,
  "tracts"
) %>%
  dplyr::bind_rows() %>%
  st_as_sf() %>%
  st_make_valid()

atlastrack_background <- list(
  gm = purrr::map(
    built_view_sets,
    ~ .x$background$gm
  ) %>%
    dplyr::bind_rows() %>%
    st_as_sf() %>%
    st_make_valid(),

  wm = purrr::map(
    built_view_sets,
    ~ .x$background$wm
  ) %>%
    dplyr::bind_rows() %>%
    st_as_sf() %>%
    st_make_valid(),

  vent = purrr::map(
    built_view_sets,
    ~ .x$background$vent
  ) %>%
    dplyr::bind_rows() %>%
    st_as_sf() %>%
    st_make_valid()
)

atlastrack_views <- gm_all %>%
  distinct(
    view_set,
    display_view,
    view,
    slice_mm,
    view_order
  ) %>%
  arrange(
    view_set,
    view_order
  )

# ============================================================
# Basic checks before saving
# ============================================================

message("Final atlastrack_tracts views:")
print(
  atlastrack_tracts %>%
    st_drop_geometry() %>%
    count(
      view_set,
      display_view,
      view,
      slice_mm,
      view_order
    )
)

message("Final atlastrack_background$gm views:")
print(
  atlastrack_background$gm %>%
    st_drop_geometry() %>%
    count(
      view_set,
      display_view,
      view,
      slice_mm,
      view_order
    )
)

message("Final atlastrack_background$wm views:")
print(
  atlastrack_background$wm %>%
    st_drop_geometry() %>%
    count(
      view_set,
      display_view,
      view,
      slice_mm,
      view_order
    )
)

message("Final atlastrack_background$vent views:")
print(
  atlastrack_background$vent %>%
    st_drop_geometry() %>%
    count(
      view_set,
      display_view,
      view,
      slice_mm,
      view_order
    )
)

# ============================================================
# Save package data
# ============================================================

usethis::use_data(
  atlastrack_tracts,
  atlastrack_background,
  atlastrack_views,
  overwrite = TRUE
)
