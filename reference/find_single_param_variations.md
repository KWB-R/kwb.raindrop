# Find scenarios that differ from a reference in exactly one parameter

Identifies rows that differ from a reference in *exactly one* parameter.
You may pass `param_cols` explicitly, or let the function infer them by
excluding `id_col`, `exclude_cols`, and any columns matching
`exclude_cols_regex` (default excludes result columns like `h_*`).

## Usage

``` r
find_single_param_variations(
  data,
  ref_scenario = "s00001",
  id_col = "scenario_name",
  param_cols = NULL,
  exclude_cols = NULL,
  exclude_cols_regex = "^h_",
  tol = 1e-09,
  quiet = FALSE,
  include_reference = c("per_param", "none")
)
```

## Arguments

- data:

  A data frame with scenarios.

- ref_scenario:

  Reference scenario ID (default `"s00001"`).

- id_col:

  Name of the scenario ID column (default `"scenario_name"`).

- param_cols:

  Optional character vector of parameter columns. If `NULL`, parameters
  are inferred.

- exclude_cols:

  Character vector to exclude from parameter detection.

- exclude_cols_regex:

  Regex to auto-exclude non-parameter columns (default `"^h_"`).

- tol:

  Numeric tolerance for numeric comparisons (default `1e-9`).

- quiet:

  Logical; print diagnostics if `FALSE` (default).

- include_reference:

  `"per_param"` (default) to include one reference row per parameter
  with hits, or `"none"`.

## Value

A data frame with columns: `id_col`, `param_name`, `param_value`,
`is_reference`. Attributes: `"param_cols"`, `"ref_row"`, `"diff_mat"`,
`"n_diff"`.
