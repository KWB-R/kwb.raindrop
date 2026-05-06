# Download the Tandler "Regenwasserbewirtschaftung" calculation engine

Fetches the Windows executable from the corresponding GitHub Release of
the companion `KWB-R/kwb.raindrop.binaries` repository and caches it
under a per-version sub-directory of the user cache. Subsequent calls
are no-ops once the file is present.

## Usage

``` r
download_engine(
  version = default_engine_version(),
  cache_dir = tools::R_user_dir("kwb.raindrop", "cache"),
  force = FALSE
)
```

## Arguments

- version:

  `character(1)` Engine release version, matching the part after
  `engine-` in the `KWB-R/kwb.raindrop.binaries` Release tag. Defaults
  to the package's pinned version.

- cache_dir:

  `character(1)` Directory where engine binaries are cached. A
  sub-directory named after `version` is used so multiple versions can
  coexist. Defaults to
  [`tools::R_user_dir`](https://rdrr.io/r/tools/userdir.html)`("kwb.raindrop", "cache")`.

- force:

  `logical(1)` If `TRUE`, re-download even if the executable already
  exists in the cache. Default: `FALSE`.

## Value

`character(1)` — absolute path to the cached `.exe`.

## Details

Releases in `KWB-R/kwb.raindrop.binaries` follow the tag scheme
`engine-<version>` and contain a single asset named
`Regenwasserbewirtschaftung.exe` (the version is encoded in the tag, not
in the filename, so multiple engine versions can coexist side-by-side in
the cache).

The executable is a Windows binary; on non-Windows platforms the file is
still downloaded but cannot be executed directly. The function does not
validate the download with a checksum — pin a specific `version` to
guarantee reproducibility.

## Examples

``` r
if (FALSE) { # \dontrun{
exe <- download_engine()
exe <- download_engine("2026-02-24")
} # }
```
