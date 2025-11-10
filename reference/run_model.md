# Run an rainwater management model executable with an input file

Builds and runs a system command of the form
`"<path_exe> <path_input>"`, after normalising both paths to absolute
paths. Progress and (optionally) command output are wrapped with
[`kwb.utils::catAndRun()`](https://rdrr.io/pkg/kwb.utils/man/catAndRun.html)
for neat logging.

## Usage

``` r
run_model(path_exe, path_input, print_output = FALSE, debug = TRUE, ...)
```

## Arguments

- path_exe:

  `character(1)` Path to the model executable (e.g., a `.exe` on
  Windows). The file must exist.

- path_input:

  `character(1)` Path to the model input file passed as the single
  argument to the executable. The file must exist.

- print_output:

  `logical(1)` If `TRUE`, stream the process output to the console and
  return the exit status (integer). If `FALSE` (default), capture and
  return the command output as a character vector.

- debug:

  `logical(1)` Forwarded to `kwb.utils::catAndRun(dbg = ...)` to
  enable/disable the progress message. Default: `TRUE`.

- ...:

  Additional arguments passed to
  [`base::shell()`](https://rdrr.io/r/base/shell.html), e.g. `timeout`
  on Windows. See [`?base::shell`](https://rdrr.io/r/base/shell.html)
  for details.

## Value

If `print_output = FALSE`, a character vector containing the captured
standard output of the command. If `print_output = TRUE`, the
(invisible) integer exit status returned by
[`shell()`](https://rdrr.io/r/base/shell.html) (0 indicates success).

## Details

Both `path_exe` and `path_input` are converted to absolute, normalised
paths via
[`fs::path_abs()`](https://fs.r-lib.org/reference/path_math.html) and
[`base::normalizePath()`](https://rdrr.io/r/base/normalizePath.html).
The command is executed with
[`base::shell()`](https://rdrr.io/r/base/shell.html), which on Windows
invokes the system shell. On non-Windows platforms, prefer
[`base::system()`](https://rdrr.io/r/base/system.html) if you need full
POSIX semantics.

## Side effects

Executes an external program that may read/write files depending on the
model. Ensure you trust the executable and paths provided.

## See also

[`base::shell()`](https://rdrr.io/r/base/shell.html),
[`fs::path_abs()`](https://fs.r-lib.org/reference/path_math.html),
[`kwb.utils::catAndRun()`](https://rdrr.io/pkg/kwb.utils/man/catAndRun.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: run a hypothetical model with an input file
exe   <- "C:/path/to/model.exe"
input <- "C:/path/to/input.h5"

# Capture output as character vector
out <- run_model(exe, input, print_output = FALSE)

# Stream output and get exit status
status <- run_model(exe, input, print_output = TRUE)
} # }
```
