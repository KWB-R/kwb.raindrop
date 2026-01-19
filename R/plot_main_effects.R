#' Plot main effects of multiple parameters on an outcome (violin/box/jitter)
#'
#' Creates a facetted overview plot showing the distribution of an outcome
#' (e.g., `n_overflows`) across the tested levels of multiple (varied) parameters.
#' Parameters are sorted by a simple effect-size proxy: the range of median outcome
#' values across parameter levels.
#'
#' The function is intended for optimisation / sensitivity grids with many
#' parameters, where a single 2D scatter plot is not informative.
#'
#' @param df A data.frame (or tibble) containing the outcome column `y` and the
#'   parameter columns listed in `params`.
#' @param y Character scalar. Name of the outcome column to plot on the y-axis.
#'   Defaults to `"n_overflows"`.
#' @param params Character vector of parameter column names in `df` to consider.
#' @param max_levels Integer. Parameters with more than `max_levels` distinct
#'   values are dropped to keep the plot readable. Defaults to 25.
#'
#' @return A ggplot object (facetted violin + boxplot + jitter).
#'
#' @examples
#' \dontrun{
#' library(readr)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' library(forcats)
#'
#' df <- read_csv("simulation_results_optimisation.csv", show_col_types = FALSE)
#' params <- c("connected_area", "mulde_area", "mulde_height",
#'             "filter_hydraulicconductivity", "filter_height",
#'             "storage_height", "bottom_hydraulicconductivity", "rain_factor")
#'
#' p <- plot_main_effects(df, y = "n_overflows", params = params, max_levels = 20)
#' p
#' }
#'
#' @export
#' @importFrom dplyr %>% select all_of group_by summarise left_join mutate n_distinct
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_jitter facet_wrap theme_bw theme labs element_text
#' @importFrom forcats fct_reorder
#' @importFrom stats median
plot_main_effects <- function(df, y = "n_overflows", params, max_levels = 25) {
  
  # Parameter mit zu vielen Auspraegungen ggf. droppen (damit es lesbar bleibt)
  lvl <- sapply(df[params], function(x) dplyr::n_distinct(x, na.rm = TRUE))
  params_use <- params[lvl <= max_levels]
  if (length(params_use) == 0) stop("Alle Parameter haben > max_levels Ausprägungen. Erhöhe max_levels oder wähle params gezielt.")
  
  dl <- df %>%
    dplyr::select(all_of(c(y, params_use))) %>%
    tidyr::pivot_longer(cols = all_of(params_use), names_to = "parameter", values_to = "value") %>%
    dplyr::mutate(value = as.factor(value))
  
  # Effektmass pro Parameter (Range der Medianwerte) zum Sortieren
  eff <- dl %>%
    dplyr::group_by(parameter, value) %>%
    dplyr::summarise(med = median(.data[[y]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(effect = max(med, na.rm = TRUE) - min(med, na.rm = TRUE), .groups = "drop")
  
  dl <- dl %>%
    dplyr::left_join(eff, by = "parameter") %>%
    dplyr::mutate(parameter = forcats::fct_reorder(parameter, effect, .desc = TRUE))
  
  ggplot2::ggplot(dl, ggplot2::aes(x = value, y = .data[[y]])) +
    ggplot2::geom_violin(trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.18, outlier.alpha = 0) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
    ggplot2::facet_wrap(~parameter, scales = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(#angle = 35, 
                                              #hjust = 1
      )) +
    ggplot2::labs(x = NULL, y = y, title = paste0("Main effects: '", y, "' for each parameter (sorted by effect)"))
}
