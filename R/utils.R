#' List available AtlasTrack tract names
#'
#' @return A data frame containing AtlasTrack tract abbreviations and full names.
#' @export
atlastrack_tract_names <- function() {

  tract_lookup <- tibble::tribble(
    ~tract_name, ~tract_full_name,

    "CC",      "Corpus callosum",
    "Fmaj",    "Forceps major",
    "Fmin",    "Forceps minor",

    "R_Fx",    "Right fornix",
    "L_Fx",    "Left fornix",

    "R_CgC",   "Right cingulate cingulum",
    "L_CgC",   "Left cingulate cingulum",

    "R_CgH",   "Right parahippocampal cingulum",
    "L_CgH",   "Left parahippocampal cingulum",

    "R_CST",   "Right corticospinal tract",
    "L_CST",   "Left corticospinal tract",

    "R_ATR",   "Right anterior thalamic radiation",
    "L_ATR",   "Left anterior thalamic radiation",

    "R_Unc",   "Right uncinate fasciculus",
    "L_Unc",   "Left uncinate fasciculus",

    "R_ILF",   "Right inferior longitudinal fasciculus",
    "L_ILF",   "Left inferior longitudinal fasciculus",

    "R_IFO",   "Right inferior fronto-occipital fasciculus",
    "L_IFO",   "Left inferior fronto-occipital fasciculus",

    "R_SLF",   "Right superior longitudinal fasciculus",
    "L_SLF",   "Left superior longitudinal fasciculus",

    "R_tSLF",  "Right temporal superior longitudinal fasciculus",
    "L_tSLF",  "Left temporal superior longitudinal fasciculus",

    "R_pSLF",  "Right parietal superior longitudinal fasciculus",
    "L_pSLF",  "Left parietal superior longitudinal fasciculus",

    "R_SCS",   "Right superior corticostriate tract",
    "L_SCS",   "Left superior corticostriate tract",

    "R_fSCS",  "Right frontal superior corticostriate tract",
    "L_fSCS",  "Left frontal superior corticostriate tract",

    "R_pSCS",  "Right parietal superior corticostriate tract",
    "L_pSCS",  "Left parietal superior corticostriate tract",

    "R_SIFC",  "Right striatal inferior frontal cortex tract",
    "L_SIFC",  "Left striatal inferior frontal cortex tract",

    "R_IFSFC", "Right inferior frontal to superior frontal cortical tract",
    "L_IFSFC", "Left inferior frontal to superior frontal cortical tract"
  )

  available_tracts <- tibble::tibble(
    tract_name = sort(unique(atlastrack_tracts$tract_name))
  )

  available_tracts |>
    dplyr::left_join(
      tract_lookup,
      by = "tract_name"
    )
}

#' Show available AtlasTrack views
#'
#' @return A data frame of available view sets and display views.
#' @export
atlastrack_views_available <- function() {
  atlastrack_views
}
