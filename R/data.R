#' AtlasTrack 2D tract polygons
#'
#' A preprocessed `sf` object containing two-dimensional AtlasTrack
#' white-matter tract polygons for ggseg-style plotting.
#'
#' @format An `sf` object with columns including:
#' \describe{
#'   \item{tract_id}{Numeric AtlasTrack tract identifier.}
#'   \item{tract_name}{AtlasTrack tract name.}
#'   \item{view_set}{View set, either `"ggseg"` or `"orthogonal"`.}
#'   \item{display_view}{Displayed panel name.}
#'   \item{view}{Original anatomical view.}
#'   \item{slice_mm}{Slice coordinate in MNI space.}
#'   \item{view_order}{Panel order within view set.}
#'   \item{atlas}{Atlas name.}
#'   \item{type}{Atlas type.}
#'   \item{hemi}{Hemisphere label.}
#'   \item{side}{Panel side/view label.}
#'   \item{region}{Region label used for plotting.}
#'   \item{label}{Region label.}
#'   \item{geometry}{Polygon geometry.}
#' }
"atlastrack_tracts"


#' AtlasTrack 2D background polygons
#'
#' A list containing ggseg-style background layers for plotting AtlasTrack.
#'
#' @format A list with three `sf` objects:
#' \describe{
#'   \item{gm}{Grey matter / cortical background polygons.}
#'   \item{wm}{White matter cut-out polygons.}
#'   \item{vent}{Ventricle polygons.}
#' }
"atlastrack_background"


#' AtlasTrack available views
#'
#' Metadata describing the available AtlasTrack 2D plotting views.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{view_set}{View set, either `"ggseg"` or `"orthogonal"`.}
#'   \item{display_view}{Displayed panel name.}
#'   \item{view}{Original anatomical view.}
#'   \item{slice_mm}{Slice coordinate in MNI space.}
#'   \item{view_order}{Panel order within view set.}
#' }
"atlastrack_views"
