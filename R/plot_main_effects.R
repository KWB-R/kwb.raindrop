#' Plot main effects of multiple parameters on an outcome (violin/box/jitter)
#'
#' Creates a facetted overview plot showing the distribution of an outcome
#' (for example \code{n_overflows}) across the tested levels of multiple varied
#' parameters. Parameters are sorted by a simple effect-size proxy: the range of
#' median outcome values across parameter levels.
#'
#' The function is intended for optimisation or sensitivity grids with many
#' parameters, where a single 2D scatter plot is not informative.
#'
#' The plot language can be switched via \code{lang = "de"} or
#' \code{lang = "en"}. This affects the title, y-axis label, and selected
#' parameter labels.
#'
#' @param df A data.frame or tibble containing the outcome column \code{y} and
#'   the parameter columns listed in \code{params}.
#' @param y Character scalar. Name of the outcome column to plot on the y-axis.
#'   Defaults to \code{"n_overflows"}.
#' @param params Character vector of parameter column names in \code{df} to
#'   consider.
#' @param max_levels Integer. Parameters with more than \code{max_levels}
#'   distinct values are dropped to keep the plot readable. Defaults to 25.
#' @param ylim_lower Numeric scalar or \code{NULL}. Optional lower display limit
#'   for the y-axis. Uses \code{ggplot2::coord_cartesian()}, so data are not
#'   removed before computing violin and boxplots. Defaults to \code{0}.
#' @param lang Character. Plot language: \code{"de"} or \code{"en"}.
#'
#' @return A ggplot object with facetted violin, boxplot, and jitter layers.
#' @export
#' @importFrom dplyr %>% select all_of group_by summarise left_join mutate n_distinct
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_jitter facet_wrap
#'   theme_bw theme labs element_text coord_cartesian
#' @importFrom forcats fct_reorder
#' @importFrom stats median
#' @importFrom rlang .data
plot_main_effects <- function(df,
                              y = "n_overflows",
                              params,
                              max_levels = 25,
                              ylim_lower = 0,
                              lang = c("de", "en")) {
  
  lang <- match.arg(lang)
  
  txt <- switch(
    lang,
    de = list(
      title = "Haupteffekte: '%s' f\u00fcr jeden Parameter (nach Effekt sortiert)",
      dropped = "Alle Parameter haben > max_levels Auspr\u00e4gungen. Erh\u00f6he max_levels oder w\u00e4hle params gezielt.",
      y_lab = function(y_name) {
        if (identical(y_name, "n_overflows")) {
          "Anzahl \u00dcberlaufereignisse"
        } else {
          y_name
        }
      }
    ),
    en = list(
      title = "Main effects: '%s' for each parameter (sorted by effect)",
      dropped = "All parameters have > max_levels distinct values. Increase max_levels or choose params more selectively.",
      y_lab = function(y_name) y_name
    )
  )
  
  param_labels_de <- c(
    "mulde_area" = "Muldenfl\u00e4che [m\u00b2]",
    "mulde_height" = "Muldenh\u00f6he [mm]",
    "filter_hydraulicconductivity" = "hydr. Leitf\u00e4higkeit des Bodenfilters [mm/h]",
    "storage_height" = "Speicherh\u00f6he [mm]",
    "connected_area" = "Angeschlossene Fl\u00e4che [m\u00b2]",
    "filter_height" = "Filterh\u00f6he [mm]",
    "bottom_hydraulicconductivity" = "hydr. Leitf\u00e4higkeit des Untergrunds [mm/h]",
    "rain_factor" = "Regenfaktor"
  )
  
  translate_param <- function(x) {
    if (lang == "de" && x %in% names(param_labels_de)) {
      param_labels_de[[x]]
    } else {
      x
    }
  }
  
  lvl <- vapply(df[params], function(x) dplyr::n_distinct(x, na.rm = TRUE), numeric(1))
  params_use <- params[lvl <= max_levels]
  
  if (length(params_use) == 0) {
    stop(txt$dropped)
  }
  
  dl <- df %>%
    dplyr::select(dplyr::all_of(c(y, params_use))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(params_use),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      value = as.factor(.data$value)
    )
  
  eff <- dl %>%
    dplyr::group_by(.data$parameter, .data$value) %>%
    dplyr::summarise(
      med = stats::median(.data[[y]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::summarise(
      effect = max(.data$med, na.rm = TRUE) - min(.data$med, na.rm = TRUE),
      .groups = "drop"
    )
  
  dl <- dl %>%
    dplyr::left_join(eff, by = "parameter") %>%
    dplyr::mutate(
      parameter_label = vapply(.data$parameter, translate_param, character(1)),
      parameter_label = forcats::fct_reorder(.data$parameter_label, .data$effect, .desc = TRUE)
    )
  
  p <- ggplot2::ggplot(dl, ggplot2::aes(x = .data$value, y = .data[[y]])) +
    ggplot2::geom_violin(trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.18, outlier.alpha = 0) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
    ggplot2::facet_wrap(~parameter_label, scales = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text()
    ) +
    ggplot2::labs(
      x = NULL,
      y = txt$y_lab(y),
      title = sprintf(txt$title, txt$y_lab(y))
    )
  
  if (!is.null(ylim_lower)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim_lower, NA))
  }
  
  p
}