# Build ABCD-to-AtlasTrack DTI lookup table
#
# This script creates the package data object:
# atlastrack_abcd_tract_lookup
#
# It maps ABCD 5.0 and ABCD 6.0 DTI variable names onto the
# tract_name values used by ggAtlasTrack.
#
# The stored variable names are based on FA columns. Helper functions
# in R/abcd.R can then generate other metric names such as MD, LD, and TD.

lookup_path <- file.path(
  "data-raw",
  "csv",
  "dti_label_comparison.csv"
)

lookup_raw <- read.csv(
  lookup_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Load plotting atlas data so we can flag which lookup rows are available
# in the ggAtlasTrack plotting atlas.
if (!exists("atlastrack_tracts")) {
  load(file.path("data", "atlastrack_tracts.rda"))
}

# Clean descriptive label typos from the original lookup.
clean_tract_label <- function(x) {
  x |>
    gsub("foreceps", "forceps", x = _, fixed = TRUE) |>
    gsub("cotex", "cortex", x = _, fixed = TRUE)
}

atlastrack_abcd_tract_lookup <- lookup_raw |>
  dplyr::transmute(
    ABCD5_fa_var_name = .data$ABCD5_var_name,
    ABCD6_fa_var_name = .data$ABCD6_var_name,
    tract_name = .data$AtlasTrack_fiber_name,
    tract_label = clean_tract_label(.data$AtlasTrack_fiber_label),
    in_atlastrack_plot = .data$AtlasTrack_fiber_name %in%
      unique(atlastrack_tracts$tract_name)
  ) |>
  dplyr::arrange(
    .data$tract_name
  )

usethis::use_data(
  atlastrack_abcd_tract_lookup,
  overwrite = TRUE
)
