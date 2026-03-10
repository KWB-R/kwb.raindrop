#' Trade-off plot: Infiltration vs. Evapotranspiration (discrete colors by overflow threshold)
#'
#' Creates a scatter plot from optimisation results with element infiltration on
#' the x-axis and element evapotranspiration on the y-axis.
#'
#' Coloring is \emph{discrete} based on \code{n_overflows} and a threshold
#' \code{x}:
#' \itemize{
#'   \item \strong{dark green} for \code{n_overflows = 0}
#'   \item a discrete palette from dark green to yellow-green for integer
#'   levels \code{1..(x-1)}
#'   \item \strong{red} for \code{n_overflows >= x}, shown as category
#'   \code{">=x"}
#' }
#'
#' The plot language can be switched via \code{lang = "de"} or
#' \code{lang = "en"}. This affects title, axis labels, legend title, and
#' tooltip labels unless custom labels are supplied explicitly.
#'
#' Tooltip text additionally includes all parameters from \code{param_grid} that
#' vary across scenarios, excluding \code{scenario_name}.
#'
#' @param simulation_results_optimisation Data frame with simulation results.
#'   Required columns are \code{scenario_name}, \code{n_overflows},
#'   \code{sum_overflows}, \code{element.WB_InfiltrationNetto_},
#'   \code{element.WB_Evapotranspiration_}, and
#'   \code{element.WB_Oberflaechenablauf_Ueberlauf_}.
#' @param param_grid Data frame with parameter grid. Must contain
#'   \code{scenario_name}.
#' @param x Numeric, typically integer. Threshold for overflow coloring. Values
#'   greater than or equal to \code{x} are mapped to the red category
#'   \code{">=x"}.
#' @param filter_n_gtx Logical. If \code{TRUE}, scenarios with
#'   \code{n_overflows >= x} are removed before plotting.
#' @param use_jitter Logical. If \code{TRUE}, slight jitter is applied to reduce
#'   overplotting.
#' @param jitter_width,jitter_height Numeric. Jitter strength in x- and y-
#'   direction, only used if \code{use_jitter = TRUE}.
#' @param jitter_seed Integer. Seed for reproducible jitter.
#' @param digits Integer. Number of digits used for rounding water balance
#'   values in the tooltip.
#' @param digits_params Integer. Number of digits used for rounding numeric
#'   parameter values in the tooltip.
#' @param lang Character. Plot language: \code{"de"} or \code{"en"}.
#' @param title Character or \code{NULL}. Plot title. If \code{NULL}, a
#'   language-specific default title is used.
#' @param lab_x Character or \code{NULL}. X-axis label. If \code{NULL}, a
#'   language-specific default label is used.
#' @param lab_y Character or \code{NULL}. Y-axis label. If \code{NULL}, a
#'   language-specific default label is used.
#' @param legend_position Character. Legend position, e.g. \code{"top"},
#'   \code{"bottom"}, \code{"left"}, or \code{"right"}. Default \code{"top"}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#'
#' @importFrom dplyr %>% select summarise across everything n_distinct
#' @importFrom dplyr filter pull mutate group_by left_join
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual labs theme_minimal position_jitter theme
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang .data
plot_wb_tradeoff_overflows <- function(simulation_results_optimisation,
                                       param_grid,
                                       x = 1,
                                       filter_n_gtx = FALSE,
                                       use_jitter = TRUE,
                                       jitter_width = 0.15,
                                       jitter_height = 0.15,
                                       jitter_seed = 1L,
                                       digits = 2L,
                                       digits_params = 4L,
                                       lang = c("de", "en"),
                                       title = NULL,
                                       lab_x = NULL,
                                       lab_y = NULL,
                                       legend_position = "top") {
  
  lang <- match.arg(lang)
  
  txt <- switch(
    lang,
    de = list(
      title = paste0(
        "Wasserbilanz vs. \u00DCberlaufereignisse (Anzahl \u2264 ",
        x,
        ")"
      ),
      x = "Mulde-Rigole: Infiltration [%]",
      y = "Mulde-Rigole: Evapotranspiration [%]",
      legend = "Anzahl \u00dcberlaufereignisse",
      tt_scenario = "Szenario",
      tt_n_overflows = "Anzahl \u00dcberlaufereignisse",
      tt_infil = "Infiltration [%]",
      tt_evap = "Evapotranspiration [%]",
      tt_overflow = "\u00dcberlauf [%]",
      tt_sum_overflows = "Summe \u00dcberl\u00e4ufe",
      tt_params = "Variierende Parameter"
    ),
    en = list(
      title = paste0(
        "Water balance vs. overflow events (number \u2264 ",
        x,
        ")"
      ),
      x = "Element: Infiltration [%]",
      y = "Element: Evapotranspiration [%]",
      legend = "Number of overflow events",
      tt_scenario = "Scenario",
      tt_n_overflows = "Number of overflow events",
      tt_infil = "Infiltration [%]",
      tt_evap = "Evapotranspiration [%]",
      tt_overflow = "Overflow [%]",
      tt_sum_overflows = "Sum of overflows",
      tt_params = "Varying parameters"
    )
  )
  
  if (is.null(title)) {
    title <- txt$title
  }
  if (is.null(lab_x)) {
    lab_x <- txt$x
  }
  if (is.null(lab_y)) {
    lab_y <- txt$y
  }
  
  req_grid <- c("scenario_name")
  req_res  <- c(
    "scenario_name", "n_overflows", "sum_overflows",
    "element.WB_InfiltrationNetto_",
    "element.WB_Evapotranspiration_",
    "element.WB_Oberflaechenablauf_Ueberlauf_"
  )
  
  miss_grid <- setdiff(req_grid, names(param_grid))
  miss_res  <- setdiff(req_res, names(simulation_results_optimisation))
  
  if (length(miss_grid) > 0) {
    stop("param_grid is missing column(s): ", paste(miss_grid, collapse = ", "))
  }
  if (length(miss_res) > 0) {
    stop(
      "simulation_results_optimisation is missing column(s): ",
      paste(miss_res, collapse = ", ")
    )
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    stop("x must be a single non-negative numeric value.")
  }
  
  x_int <- as.integer(round(x))
  if (!isTRUE(all.equal(x, x_int))) {
    warning("x is not an integer; using x_int = ", x_int, " for discrete palette/legend.")
  }
  
  varying_params <- param_grid %>%
    dplyr::select(-scenario_name) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "param", values_to = "vary") %>%
    dplyr::filter(vary) %>%
    dplyr::pull(param)
  
  if (length(varying_params) == 0) {
    param_tooltip <- param_grid %>%
      dplyr::select(scenario_name) %>%
      dplyr::mutate(params_html = "")
  } else {
    param_tooltip <- param_grid %>%
      dplyr::select(scenario_name, dplyr::all_of(varying_params)) %>%
      tidyr::pivot_longer(-scenario_name, names_to = "param", values_to = "val") %>%
      dplyr::mutate(
        val_chr = purrr::map_chr(val, ~ paste(.x, collapse = ",")),
        val_num = suppressWarnings(as.numeric(val_chr)),
        val_fmt = ifelse(
          is.na(val_num),
          val_chr,
          format(round(val_num, digits_params), trim = TRUE)
        ),
        kv = paste0(param, "=", val_fmt)
      ) %>%
      dplyr::group_by(scenario_name) %>%
      dplyr::summarise(params_html = paste(kv, collapse = "<br>"), .groups = "drop")
  }
  
  df <- simulation_results_optimisation %>%
    dplyr::left_join(param_tooltip, by = "scenario_name") %>%
    dplyr::filter(!isTRUE(filter_n_gtx) | is.na(.data$n_overflows) | .data$n_overflows <= x_int)
  hi_lab <- paste0(">", x_int)
  
  df <- df %>%
    dplyr::mutate(
      overflow_cat = dplyr::case_when(
        is.na(.data$n_overflows)  ~ NA_character_,
        .data$n_overflows > x_int ~ hi_lab,
        TRUE                      ~ as.character(.data$n_overflows)
      )
    )
  
  base_levels <- as.character(0:x_int)
  levs <- c(base_levels, hi_lab)
  
  df <- df %>%
    dplyr::mutate(
      overflow_cat = factor(.data$overflow_cat, levels = levs)
    )
  
  if (x_int == 0L) {
    pal <- c("0" = "orange", ">0" = "red")
  } else if (x_int == 1L) {
    pal <- c("0" = "darkgreen", "1" = "orange", ">1" = "red")
  } else {
    pal_green <- grDevices::colorRampPalette(c("darkgreen", "yellowgreen"))(x_int)
    pal_vals <- c(pal_green, "orange", "red")
    pal_names <- c(base_levels, hi_lab)
    pal <- stats::setNames(pal_vals, pal_names)
  }
  
  legend_breaks <- levs
  
  pos <- if (isTRUE(use_jitter)) {
    ggplot2::position_jitter(
      width = jitter_width,
      height = jitter_height,
      seed = jitter_seed
    )
  } else {
    "identity"
  }
  
  legend_direction <- if (legend_position %in% c("top", "bottom")) {
    "horizontal"
  } else {
    "vertical"
  }
  
  legend_nrow <- if (legend_direction == "horizontal") 1 else NULL
  legend_ncol <- if (legend_direction == "vertical") 1 else NULL
  
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = element.WB_InfiltrationNetto_,
    y = element.WB_Evapotranspiration_,
    color = overflow_cat,
    text = paste0(
      txt$tt_scenario, ": ", scenario_name,
      "<br>", txt$tt_n_overflows, ": ", n_overflows,
      "<br>", txt$tt_infil, ": ", round(element.WB_InfiltrationNetto_, digits),
      "<br>", txt$tt_evap, ": ", round(element.WB_Evapotranspiration_, digits),
      "<br>", txt$tt_overflow, ": ", round(element.WB_Oberflaechenablauf_Ueberlauf_, digits),
      "<br>", txt$tt_sum_overflows, ": ", sum_overflows,
      "<br><br><b>", txt$tt_params, "</b><br>", params_html
    )
  )) +
    ggplot2::geom_point(alpha = 0.7, position = pos) +
    ggplot2::scale_color_manual(
      values = pal,
      breaks = legend_breaks,
      limits = levs,
      drop = FALSE,
      name = txt$legend
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        direction = legend_direction,
        nrow = legend_nrow,
        ncol = legend_ncol,
        byrow = TRUE
      )
    ) +
    ggplot2::labs(
      title = title,
      x = lab_x,
      y = lab_y
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.direction = legend_direction
    )
  
  p
}