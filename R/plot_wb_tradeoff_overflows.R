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
#' vary across scenarios, excluding \code{scenario_name} (translated via
#' \code{\link{default_param_labels}}; mixed numeric / character parameters
#' such as \code{storage_type} are supported).
#'
#' If \code{simulation_results_optimisation} carries a \code{storage_type}
#' column, the points are additionally **shaped by the storage type** (filled
#' square = infiltration box / Sickerbox, filled triangle = gravel trench /
#' Schotterrigol, as in the cost plots) and the tooltip names the storage
#' type; older single-type result sets plot exactly as before.
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
#' @importFrom dplyr %>% filter mutate left_join case_when
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_shape_manual labs theme_bw position_jitter theme guides guide_legend
#' @importFrom grDevices colorRampPalette
#' @importFrom utils modifyList
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
        "Wasserbilanz vs. \u00DCberlaufereignisse (Anzahl <= ",
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
        "Water balance vs. overflow events (number <= ",
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
  
  # Shared helper (same as the cost plots): handles mixed numeric / character
  # parameter columns (e.g. storage_type) and translates the parameter names
  # via default_param_labels().
  param_tooltip <- build_varying_param_html(param_grid, lang,
                                            param_labels = NULL,
                                            digits_params = digits_params)

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

  # Optional storage-type tagging (filled square = infiltration box, filled
  # triangle = gravel trench, as in the cost plots): active when the results
  # carry a storage_type column; older single-type result sets plot as before.
  st_labels <- cost_tooltip_labels(lang)
  has_storage_type <- "storage_type" %in% names(df)
  if (has_storage_type) {
    # short language-specific names for the legend keys ...
    st <- storage_type_shapes(df$storage_type, lang)
    df$storage_type_disp <- st$display
    # ... but the bilingual names for the tooltip line, matching the cost
    # plots' tooltips
    st_raw <- as.character(df$storage_type)
    df$storage_type_tooltip <- ifelse(
      !is.na(st_raw) & st_raw == "gravel_trench",
      st_labels$st_gravel_trench, st_labels$st_infiltration_box)
  }
  # Usable storage volume of the storage layer [m3] (precomputed column or
  # derived from the storage_theta* columns); line omitted if not derivable.
  storage_volume <- storage_volume_from_df(df)

  df$tooltip_html <- paste0(
    txt$tt_scenario, ": ", df$scenario_name,
    "<br>", txt$tt_n_overflows, ": ", df$n_overflows,
    "<br>", txt$tt_infil, ": ",
    round(df[["element.WB_InfiltrationNetto_"]], digits),
    "<br>", txt$tt_evap, ": ",
    round(df[["element.WB_Evapotranspiration_"]], digits),
    "<br>", txt$tt_overflow, ": ",
    round(df[["element.WB_Oberflaechenablauf_Ueberlauf_"]], digits),
    "<br>", txt$tt_sum_overflows, ": ", df$sum_overflows,
    if (has_storage_type) {
      paste0("<br><br><b>", st_labels$tt_storage_type, ": ",
             df$storage_type_tooltip, "</b>")
    } else {
      ""
    },
    if (!is.null(storage_volume)) {
      paste0("<br>", st_labels$tt_storage_volume, ": ",
             round(storage_volume, digits))
    } else {
      ""
    },
    "<br><br><b>", txt$tt_params, "</b><br>", df$params_html
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
  
  mapping <- ggplot2::aes(
    x = .data[["element.WB_InfiltrationNetto_"]],
    y = .data[["element.WB_Evapotranspiration_"]],
    color = .data$overflow_cat,
    text = .data$tooltip_html
  )
  if (has_storage_type) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(shape = .data$storage_type_disp)
    )
  }

  p <- ggplot2::ggplot(df, mapping) +
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
        byrow = TRUE,
        order = 1
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
      legend.direction = legend_direction,
      # stack the colour and shape legends so both fit at the top
      legend.box = if (legend_direction == "horizontal") "vertical" else "horizontal"
    )

  if (has_storage_type) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = st$shape_values,
        drop = FALSE,
        name = st_labels$tt_storage_type
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          direction = legend_direction,
          nrow = legend_nrow,
          ncol = legend_ncol,
          byrow = TRUE,
          order = 2
        )
      )
  }

  p
}
