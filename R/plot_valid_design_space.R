#' Valid solutions in design space (x × y) with overflow-threshold discrete color scale
#'
#' Visualises model results in a 2D design space (by default \code{mulde_area}
#' vs. \code{mulde_height}) and highlights valid solutions (by default
#' \code{n_overflows <= valid_max}). The plot is prepared for direct use with
#' \code{plotly::ggplotly(..., tooltip = "text")} by mapping an HTML tooltip via
#' \code{aes(text = ...)} that lists all other varied parameters from
#' \code{param_grid}, excluding \code{x} and \code{y}.
#'
#' Varied parameters are detected automatically as columns in \code{param_grid}
#' with more than one distinct value. Optionally, scenarios with
#' \code{overflow_col > valid_max} can be removed entirely
#' (\code{drop_overflow_gt_valid_max = TRUE}). To help identify multiple
#' scenarios that share identical \code{x}/\code{y} coordinates but differ in
#' other parameters, optional jitter and/or variable transparency can be
#' applied.
#'
#' Discrete color mapping (threshold-style):
#' \itemize{
#'   \item \strong{dark green} at \code{n_overflows = 0}
#'   \item a discrete palette from dark green to yellow-green for integer
#'   levels \code{1..(valid_max-1)}
#'   \item \strong{orange} at \code{n_overflows = valid_max}
#'   \item \strong{red} for \code{n_overflows > valid_max}, shown as level
#'   \code{">valid_max"}
#' }
#'
#' The plot language can be switched via \code{lang = "de"} or
#' \code{lang = "en"}. This affects title, subtitle, legend title, tooltip
#' labels, and selected axis labels.
#'
#' @param param_grid A data.frame/tibble containing the scenario id
#'   (\code{id_col}) and parameters.
#' @param sim_results A data.frame/tibble containing the scenario id
#'   (\code{id_col}) and \code{overflow_col}.
#' @param id_col Character. Join key (scenario id), default
#'   \code{"scenario_name"}.
#' @param overflow_col Character. Overflow outcome column, default
#'   \code{"n_overflows"}.
#' @param valid_max Numeric. Validity threshold:
#'   \code{overflow_col <= valid_max}. Also used as the orange breakpoint in
#'   the color scale.
#' @param x Character. Name of the x-axis column, default
#'   \code{"mulde_area"}.
#' @param y Character. Name of the y-axis column, default
#'   \code{"mulde_height"}.
#' @param max_levels Integer. Parameter columns with more than
#'   \code{max_levels} distinct values are ignored in the tooltip list.
#'   Default 50.
#' @param alpha_invalid Numeric. Extra alpha multiplier for invalid solutions
#'   (background layer).
#' @param size Numeric. Base point size.
#' @param digits Integer. Significant digits for numeric tooltip values.
#' @param drop_overflow_gt_valid_max Logical. If \code{TRUE}, scenarios with
#'   \code{overflow_col > valid_max} are not plotted at all. Default
#'   \code{FALSE}.
#' @param jitter Logical. If \code{TRUE}, apply jitter, useful when many
#'   points share identical x/y.
#' @param jitter_width Numeric or \code{NULL}. Jitter width in data units. If
#'   \code{NULL}, defaults from x-range.
#' @param jitter_height Numeric or \code{NULL}. Jitter height in data units. If
#'   \code{NULL}, defaults from y-range.
#' @param jitter_factor Numeric. Factor used to derive default jitter
#'   width/height from x/y ranges. Default 0.005.
#' @param alpha_mode Character. Either \code{"none"} for fixed alpha or
#'   \code{"duplicates"} to vary alpha by the number of scenarios sharing the
#'   same x/y. Default \code{"none"}.
#' @param alpha_min Numeric. Minimum alpha used when
#'   \code{alpha_mode = "duplicates"}. Default 0.20.
#' @param alpha_max Numeric. Maximum alpha used when
#'   \code{alpha_mode = "duplicates"}. Default 1.00.
#' @param keep_param_grid_limits Logical. If \code{TRUE} and
#'   \code{drop_overflow_gt_valid_max = TRUE}, the x/y axis limits are fixed to
#'   the full range or full set of levels found in \code{param_grid}, so the
#'   design-space axes do not shrink after filtering. Default \code{TRUE}.
#' @param lang Character. Plot language: \code{"de"} or \code{"en"}.
#' @param title Character or \code{NULL}. Plot title. If \code{NULL}, a
#'   language-specific default title is used.
#' @param subtitle Character or \code{NULL}. Plot subtitle. If \code{NULL}, a
#'   language-specific default subtitle is used.
#' @param legend_position Character. Legend position, e.g. \code{"top"},
#'   \code{"bottom"}, \code{"left"}, or \code{"right"}. Default \code{"top"}.
#' @return A ggplot object. Tooltip text is mapped via \code{aes(text = ...)}.
#'
#' @importFrom dplyr %>% select all_of left_join mutate filter n_distinct
#' @importFrom dplyr case_when group_by ungroup n
#' @importFrom ggplot2 ggplot aes geom_point theme_bw labs position_jitter
#' @importFrom ggplot2 position_identity scale_alpha_identity coord_cartesian
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete scale_colour_manual
#' @importFrom ggplot2 guides guide_legend theme scale_x_continuous scale_y_continuous
#' @importFrom rlang .data
#' @importFrom grDevices colorRampPalette
#' @export
plot_valid_design_space <- function(param_grid,
                                    sim_results,
                                    id_col = "scenario_name",
                                    overflow_col = "n_overflows",
                                    valid_max = 1,
                                    x = "mulde_area",
                                    y = "mulde_height",
                                    max_levels = 50,
                                    alpha_invalid = 0.12,
                                    size = 2.4,
                                    digits = 4,
                                    drop_overflow_gt_valid_max = FALSE,
                                    jitter = FALSE,
                                    jitter_width = NULL,
                                    jitter_height = NULL,
                                    jitter_factor = 0.005,
                                    alpha_mode = c("none", "duplicates"),
                                    alpha_min = 0.20,
                                    alpha_max = 1.00,
                                    keep_param_grid_limits = TRUE,
                                    lang = c("de", "en"),
                                    title = NULL,
                                    subtitle = NULL,
                                    legend_position = "top") {
  
  alpha_mode <- match.arg(alpha_mode)
  lang <- match.arg(lang)
  
  axis_labels_de <- c(
    "mulde_area" = "Muldenfl\u00e4che [m\u00b2]",
    "mulde_height" = "Muldenh\u00f6he [mm]",
    "filter_hydraulicconductivity" = "hydr. Leitf\u00e4higkeit des Bodenfilters: hydr. Leitf\u00e4higkeit [mm/h]",
    "storage_height" = "Speicherh\u00f6he [mm]"
  )
  
  lab_x <- if (lang == "de" && x %in% names(axis_labels_de)) axis_labels_de[[x]] else x
  lab_y <- if (lang == "de" && y %in% names(axis_labels_de)) axis_labels_de[[y]] else y
  
  txt <- switch(
    lang,
    de = list(
      title = paste0(
        "G\u00fcltige L\u00f6sungen (Anzahl \u00dcberlaufereignisse \u2264 ",
        valid_max,
        ") im Designraum: ",
        lab_x,
        " \u00d7 ",
        lab_y
      ),
      subtitle = "Tooltip zeigt alle \u00fcbrigen variierenden Parameter",
      legend = "Anzahl \u00dcberlaufereignisse",
      tt_id = id_col,
      tt_overflow = "Anzahl \u00dcberlaufereignisse",
      tt_other = "Weitere Parameter"
    ),
    en = list(
      title = paste0(
        "Valid solutions (Number of overflow events \u2264 ",
        valid_max,
        ") in design space: ",
        x,
        " \u00d7 ",
        y
      ),
      subtitle = "Tooltip lists all other varied parameters",
      legend = "Number of overflow events",
      tt_id = id_col,
      tt_overflow = "Number of overflow events",
      tt_other = "Other parameters"
    )
  )
  
  if (is.null(title)) {
    title <- txt$title
  }
  if (is.null(subtitle)) {
    subtitle <- txt$subtitle
  }
  
  stopifnot(is.data.frame(param_grid), is.data.frame(sim_results))
  
  if (!is.numeric(valid_max) || length(valid_max) != 1 || is.na(valid_max) || valid_max < 0) {
    stop("valid_max must be a single non-negative numeric value.")
  }
  
  valid_max_int <- as.integer(round(valid_max))
  if (!isTRUE(all.equal(valid_max, valid_max_int))) {
    warning("valid_max is not an integer; using valid_max_int = ", valid_max_int, " for discrete palette/legend.")
  }
  
  need_pg  <- c(id_col, x, y)
  need_res <- c(id_col, overflow_col)
  
  miss_pg  <- setdiff(need_pg, names(param_grid))
  miss_res <- setdiff(need_res, names(sim_results))
  
  if (length(miss_pg) > 0) {
    stop("Missing columns in param_grid: ", paste(miss_pg, collapse = ", "))
  }
  if (length(miss_res) > 0) {
    stop("Missing columns in sim_results: ", paste(miss_res, collapse = ", "))
  }
  
  cand <- setdiff(names(param_grid), id_col)
  lvl  <- vapply(param_grid[cand], function(v) dplyr::n_distinct(v, na.rm = TRUE), numeric(1))
  varied_params <- cand[lvl > 1 & lvl <= max_levels]
  
  keep_pg <- unique(c(id_col, x, y, varied_params))
  d <- dplyr::left_join(
    dplyr::select(param_grid, dplyr::all_of(keep_pg)),
    dplyr::select(sim_results, dplyr::all_of(c(id_col, overflow_col))),
    by = id_col
  )
  
  d <- dplyr::mutate(
    d,
    valid = .data[[overflow_col]] <= valid_max_int
  )
  
  if (isTRUE(drop_overflow_gt_valid_max)) {
    d <- dplyr::filter(d, .data[[overflow_col]] <= valid_max_int)
  }
  
  other_params <- setdiff(varied_params, c(x, y))
  
  fmt <- function(v) {
    if (is.numeric(v)) {
      format(signif(v, digits), trim = TRUE, scientific = FALSE)
    } else {
      as.character(v)
    }
  }
  
  other_block <- if (length(other_params) > 0) {
    m <- as.data.frame(d[other_params])
    m[] <- lapply(m, fmt)
    lines <- apply(m, 1, function(r) paste0(other_params, ": ", r, collapse = "<br>"))
    paste0("<br><br><b>", txt$tt_other, "</b><br>", lines)
  } else {
    ""
  }
  
  d$hover <- paste0(
    "<b>", txt$tt_id, ":</b> ", d[[id_col]],
    "<br><b>", txt$tt_overflow, ":</b> ", fmt(d[[overflow_col]]),
    "<br><b>", lab_x, ":</b> ", fmt(d[[x]]),
    "<br><b>", lab_y, ":</b> ", fmt(d[[y]]),
    other_block
  )
  
  hi_lab <- paste0(">", valid_max_int)
  
  d <- dplyr::mutate(
    d,
    overflow_cat = dplyr::case_when(
      is.na(.data[[overflow_col]])          ~ NA_character_,
      .data[[overflow_col]] > valid_max_int ~ hi_lab,
      TRUE                                  ~ as.character(.data[[overflow_col]])
    )
  )
  
  base_levels <- as.character(0:valid_max_int)
  levs <- c(base_levels, hi_lab)
  
  d <- dplyr::mutate(
    d,
    overflow_cat = factor(.data$overflow_cat, levels = levs)
  )
  
  if (valid_max_int == 0L) {
    pal <- c("0" = "orange", ">0" = "red")
  } else if (valid_max_int == 1L) {
    pal <- c("0" = "darkgreen", "1" = "orange", ">1" = "red")
  } else {
    pal_green <- grDevices::colorRampPalette(c("darkgreen", "yellowgreen"))(valid_max_int)
    pal_vals <- c(pal_green, "orange", "red")
    pal_names <- c(base_levels, hi_lab)
    pal <- stats::setNames(pal_vals, pal_names)
  }
  
  legend_breaks <- levs
  
  if (alpha_mode == "duplicates") {
    d <- d %>%
      dplyr::group_by(.data[[x]], .data[[y]]) %>%
      dplyr::mutate(dup_n = dplyr::n()) %>%
      dplyr::ungroup()
    
    alpha_dup <- alpha_max / sqrt(d$dup_n)
    alpha_dup <- pmax(alpha_min, pmin(alpha_max, alpha_dup))
    
    d$alpha_valid   <- alpha_dup
    d$alpha_invalid <- alpha_dup * alpha_invalid
  } else {
    d$alpha_valid   <- 0.95
    d$alpha_invalid <- alpha_invalid
  }
  
  if (isTRUE(jitter)) {
    xr <- range(d[[x]], na.rm = TRUE)
    yr <- range(d[[y]], na.rm = TRUE)
    dx <- diff(xr)
    dy <- diff(yr)
    
    if (is.null(jitter_width)) {
      jitter_width <- if (is.finite(dx) && dx > 0) jitter_factor * dx else 0
    }
    if (is.null(jitter_height)) {
      jitter_height <- if (is.finite(dy) && dy > 0) jitter_factor * dy else 0
    }
    
    pos <- ggplot2::position_jitter(width = jitter_width, height = jitter_height)
  } else {
    pos <- ggplot2::position_identity()
  }
  
  
  legend_direction <- if (legend_position %in% c("top", "bottom")) {
    "horizontal"
  } else {
    "vertical"
  }
  
  legend_nrow <- if (legend_direction == "horizontal") 1 else NULL
  legend_ncol <- if (legend_direction == "vertical") 1 else NULL
  
  if (isTRUE(drop_overflow_gt_valid_max)) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(
        ggplot2::aes(
          colour = .data$overflow_cat,
          text = .data$hover,
          alpha = .data$alpha_valid
        ),
        size = size + 0.6,
        position = pos
      ) +
      ggplot2::scale_colour_manual(
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
      ggplot2::scale_alpha_identity(guide = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = legend_position,
        legend.direction = legend_direction
      ) +
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = title,
        subtitle = subtitle
      )
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(
        data = dplyr::filter(d, !.data$valid),
        ggplot2::aes(
          colour = .data$overflow_cat,
          text = .data$hover,
          alpha = .data$alpha_invalid
        ),
        size = size,
        position = pos
      ) +
      ggplot2::geom_point(
        data = dplyr::filter(d, .data$valid),
        ggplot2::aes(
          colour = .data$overflow_cat,
          text = .data$hover,
          alpha = .data$alpha_valid
        ),
        size = size + 0.8,
        position = pos
      ) +
      ggplot2::scale_colour_manual(
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
      ggplot2::scale_alpha_identity(guide = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = legend_position,
        legend.direction = legend_direction
      ) +
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = title,
        subtitle = subtitle
      )
  }
  
  if (isTRUE(keep_param_grid_limits)) {
    if (is.numeric(param_grid[[x]]) && is.numeric(param_grid[[y]])) {
      x_vals <- sort(unique(param_grid[[x]]))
      y_vals <- sort(unique(param_grid[[y]]))
      
      p <- p +
        ggplot2::scale_x_continuous(breaks = x_vals) +
        ggplot2::scale_y_continuous(breaks = y_vals)
      
      if (isTRUE(drop_overflow_gt_valid_max)) {
        p <- p + ggplot2::coord_cartesian(
          xlim = range(x_vals),
          ylim = range(y_vals)
        )
      }
    } else {
      x_vals <- unique(param_grid[[x]])
      y_vals <- unique(param_grid[[y]])
      
      p <- p +
        ggplot2::scale_x_discrete(
          limits = x_vals,
          breaks = x_vals,
          drop = FALSE
        ) +
        ggplot2::scale_y_discrete(
          limits = y_vals,
          breaks = y_vals,
          drop = FALSE
        )
    }
  }
  
  p
}
