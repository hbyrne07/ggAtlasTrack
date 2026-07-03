# Avoid R CMD check notes for package data objects and tidy evaluation pronouns
utils::globalVariables(c(
  ".data",
  "atlastrack_tracts",
  "atlastrack_background",
  "atlastrack_views"
))
