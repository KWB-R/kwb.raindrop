#' Default calculation-engine version
#'
#' Central pin for the engine release that the package vignettes and helpers
#' default to. Update this string when adopting a new engine release on
#' GitHub.
#'
#' @return `character(1)` — release tag suffix (e.g. `"2026-02-24"`), used to
#'   build the GitHub Release URL `engine-<version>` and as a sub-directory
#'   name in the local cache.
#' @keywords internal
#' @noRd
default_engine_version <- function() {
  "2026-02-24"
}

#' Download the Tandler "Regenwasserbewirtschaftung" calculation engine
#'
#' Fetches the Windows executable from the corresponding GitHub Release of
#' this repository and caches it under a per-version sub-directory of the
#' user cache. Subsequent calls are no-ops once the file is present.
#'
#' Releases follow the tag scheme `engine-<version>` and contain a single
#' asset named `Regenwasserbewirtschaftung.exe` (the version is encoded in
#' the tag, not in the filename, so multiple engine versions can coexist
#' side-by-side in the cache).
#'
#' @param version `character(1)`
#'   Engine release version, matching the part after `engine-` in the
#'   GitHub Release tag. Defaults to the package's pinned version.
#' @param cache_dir `character(1)`
#'   Directory where engine binaries are cached. A sub-directory named after
#'   `version` is used so multiple versions can coexist. Defaults to
#'   [`tools::R_user_dir`]`("kwb.raindrop", "cache")`.
#' @param force `logical(1)`
#'   If `TRUE`, re-download even if the executable already exists in the
#'   cache. Default: `FALSE`.
#'
#' @return `character(1)` — absolute path to the cached `.exe`.
#'
#' @details
#' The executable is a Windows binary; on non-Windows platforms the file is
#' still downloaded but cannot be executed directly. The function does not
#' validate the download with a checksum — pin a specific `version` to
#' guarantee reproducibility.
#'
#' @examples
#' \dontrun{
#' exe <- download_engine()
#' exe <- download_engine("2026-02-24")
#' }
#'
#' @export
download_engine <- function(version = default_engine_version(),
                            cache_dir = tools::R_user_dir("kwb.raindrop", "cache"),
                            force = FALSE) {
  stopifnot(
    is.character(version), length(version) == 1L, nzchar(version),
    is.character(cache_dir), length(cache_dir) == 1L,
    is.logical(force), length(force) == 1L
  )

  dir <- fs::path(cache_dir, version)
  exe <- fs::path(dir, "Regenwasserbewirtschaftung.exe")

  if (force || !fs::file_exists(exe)) {
    fs::dir_create(dir, recurse = TRUE)
    url <- sprintf(
      "https://github.com/KWB-R/kwb.raindrop/releases/download/engine-%s/Regenwasserbewirtschaftung.exe",
      version
    )
    utils::download.file(url, destfile = exe, mode = "wb")
  }

  as.character(fs::path_abs(exe))
}
