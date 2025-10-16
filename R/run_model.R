#' Run an rainwater management model executable with an input file
#'
#' Builds and runs a system command of the form `"<path_exe> <path_input>"`,
#' after normalising both paths to absolute paths. Progress and (optionally)
#' command output are wrapped with [`kwb.utils::catAndRun()`] for neat logging.
#'
#' @param path_exe   \code{character(1)}
#'   Path to the model executable (e.g., a `.exe` on Windows). The file must exist.
#' @param path_input \code{character(1)}
#'   Path to the model input file passed as the single argument to the executable.
#'   The file must exist.
#' @param print_output \code{logical(1)}
#'   If `TRUE`, stream the process output to the console and return the exit
#'   status (integer). If `FALSE` (default), capture and return the command
#'   output as a character vector.
#' @param debug \code{logical(1)}
#'   Forwarded to `kwb.utils::catAndRun(dbg = ...)` to enable/disable the
#'   progress message. Default: `TRUE`.
#' @param ... 
#'   Additional arguments passed to [base::shell()], e.g. `timeout` on Windows.
#'   See `?base::shell` for details.
#'
#' @details
#' Both `path_exe` and `path_input` are converted to absolute, normalised
#' paths via [`fs::path_abs()`] and [`base::normalizePath()`]. The command is
#' executed with [base::shell()], which on Windows invokes the system shell.
#' On non-Windows platforms, prefer [base::system()] if you need full POSIX semantics.
#'
#' @return
#' If `print_output = FALSE`, a character vector containing the captured
#' standard output of the command.  
#' If `print_output = TRUE`, the (invisible) integer exit status returned by
#' `shell()` (0 indicates success).
#'
#' @section Side effects:
#' Executes an external program that may read/write files depending on the model.
#' Ensure you trust the executable and paths provided.
#'
#' @seealso
#' [base::shell()], [fs::path_abs()], [kwb.utils::catAndRun()]
#'
#' @examples
#' \dontrun{
#' # Example: run a hypothetical model with an input file
#' exe   <- "C:/path/to/model.exe"
#' input <- "C:/path/to/input.h5"
#'
#' # Capture output as character vector
#' out <- run_model(exe, input, print_output = FALSE)
#'
#' # Stream output and get exit status
#' status <- run_model(exe, input, print_output = TRUE)
#' }
#'
#' @export
#' @importFrom fs dir_create file_copy file_exists path_abs
#' @importFrom hdf5r H5File
#' @importFrom stringr str_c 
#' @importFrom kwb.utils catAndRun resolve
run_model <- function(
    path_exe,
    path_input,
    print_output = FALSE,
    debug = TRUE,
    ...) {
  path_exe <- fs::path_abs(path_exe)
  path_input <- fs::path_abs(path_input)

  stopifnot(fs::file_exists(path_exe))
  stopifnot(fs::file_exists(path_input)) 
  
  cmd_model <- sprintf("%s %s", 
                       normalizePath(path_exe), 
                       normalizePath(path_input))
  
  kwb.utils::catAndRun(messageText = sprintf("Running model '%s'", 
                                             cmd_model), 
                       expr = {

                         shell(
                           cmd = cmd_model,
                           intern = !print_output,
                           ...)
                       },
                       dbg = debug)
}
