#' Canonical water-balance variable names emitted by the Tandler engine
#'
#' Returns the list of water-balance scalar names the Tandler
#' "Regenwasserbewirtschaftung" engine writes into the `Wasserbilanz`
#' group of each result HDF5 (without the `element.` / `connectedarea.`
#' prefix and without the trailing `_` that
#' [`add_overflow_events_and_waterbalance()`] appends when building the
#' output columns). Pass this to that function's `canonical_variables`
#' argument to keep the expected water-balance column structure visible
#' in the rendered datatable even when every scenario in a batch is
#' `NULL` (e.g. the engine returns Status 1 for every input and writes
#' no result HDF5).
#'
#' @return A `character()` vector of variable names.
#'
#' @section Source:
#' Values observed in the May 2026 432-row Wien sweep (Michael Rustler)
#' and referenced explicitly by [`plot_wb_tradeoff_overflows()`].
#'
#' @examples
#' default_canonical_wb_variables()
#'
#' @export
default_canonical_wb_variables <- function() {
  c(
    "WB_Regen",
    "WB_Evapotranspiration",
    "WB_InfiltrationNetto",
    "WB_Oberflaechenablauf_Ueberlauf",
    "WB_Oberflaechenablauf_Verschaltungen"
  )
}
