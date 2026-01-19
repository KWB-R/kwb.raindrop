#' Valid solutions in design space (mulde_area × storage_height) with tooltip labels
#'
#' Visualises model results in a 2D design space (by default \code{mulde_area} vs.
#' \code{storage_height}) and highlights valid solutions (by default \code{n_overflows <= 1}).
#' The function also creates an HTML tooltip string via \code{aes(text = ...)} listing all
#' other varied parameters from \code{param_grid} (excluding \code{x} and \code{y}), making
#' the plot directly usable with \code{plotly::ggplotly(..., tooltip="text")}.
#'
#' Varied parameters are detected automatically as columns in \code{param_grid} with more than
#' one distinct value. Optionally, scenarios with \code{overflow_col > valid_max} can be removed
#' entirely (\code{drop_overflow_gt_valid_max = TRUE}). To help identify multiple scenarios that
#' share identical \code{x}/\code{y} coordinates but differ in other parameters, optional jitter
#' and/or variable transparency can be applied.
#'
#' @param param_grid A data.frame/tibble containing the scenario id (\code{id_col}) and parameters.
#' @param sim_results A data.frame/tibble containing the scenario id (\code{id_col}) and \code{overflow_col}.
#' @param id_col Character. Join key (scenario id), default \code{"scenario_name"}.
#' @param overflow_col Character. Overflow outcome column, default \code{"n_overflows"}.
#' @param valid_max Numeric. Validity threshold: \code{overflow_col <= valid_max}.
#' @param x Character. Name of the x-axis column (default \code{"mulde_area"}).
#' @param y Character. Name of the y-axis column (default \code{"mulde_height"}).
#' @param max_levels Integer. Parameter columns with more than \code{max_levels} distinct values
#'   are ignored in the tooltip list. Default 50.
#' @param alpha_invalid Numeric. Extra alpha multiplier for invalid solutions (background layer).
#' @param size Numeric. Base point size.
#' @param digits Integer. Significant digits for numeric tooltip values.
#' @param drop_overflow_gt_valid_max Logical. If \code{TRUE}, scenarios with \code{overflow_col > valid_max}
#'   are not plotted at all. Default \code{FALSE}.
#' @param jitter Logical. If \code{TRUE}, apply jitter (useful when many points share identical x/y).
#' @param jitter_width Numeric or \code{NULL}. Jitter width in data units; if \code{NULL} defaults from x-range.
#' @param jitter_height Numeric or \code{NULL}. Jitter height in data units; if \code{NULL} defaults from y-range.
#' @param jitter_factor Numeric. Factor to derive default jitter width/height from x/y ranges. Default 0.005.
#' @param alpha_mode Character. Either \code{"none"} (fixed alpha) or \code{"duplicates"} (vary alpha by
#'   number of scenarios sharing the same x/y). Default \code{"none"}.
#' @param alpha_min Numeric. Minimum alpha used when \code{alpha_mode="duplicates"}. Default 0.20.
#' @param alpha_max Numeric. Maximum alpha used when \code{alpha_mode="duplicates"}. Default 1.00.
#' @param keep_param_grid_limits Logical. If \code{TRUE} and
#'   \code{drop_overflow_gt_valid_max = TRUE}, the x/y axis limits are fixed to the
#'   full range (or full set of levels) found in \code{param_grid}, so the design-space
#'   axes do not shrink after filtering. Default \code{TRUE}.
#' @return A ggplot object. Tooltip text is mapped via \code{aes(text = ...)}.
#'
#' @importFrom dplyr %>% select all_of left_join mutate filter n_distinct case_when group_by ungroup n
#' @importFrom ggplot2 ggplot aes geom_point theme_bw labs scale_colour_manual 
#' position_jitter position_identity scale_alpha_identity guides coord_cartesian scale_x_discrete scale_y_discrete
#' @importFrom rlang .data
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
                                    keep_param_grid_limits = TRUE) {
  
  alpha_mode <- match.arg(alpha_mode)
  
  stopifnot(is.data.frame(param_grid), is.data.frame(sim_results))
  need_pg  <- c(id_col, x, y)
  need_res <- c(id_col, overflow_col)
  
  miss_pg  <- setdiff(need_pg,  names(param_grid))
  miss_res <- setdiff(need_res, names(sim_results))
  if (length(miss_pg)  > 0) stop("Missing columns in param_grid: ", paste(miss_pg, collapse = ", "))
  if (length(miss_res) > 0) stop("Missing columns in sim_results: ", paste(miss_res, collapse = ", "))
  
  # varied params = more than 1 distinct value; optionally drop those with too many levels
  cand <- setdiff(names(param_grid), id_col)
  lvl  <- sapply(param_grid[cand], function(v) dplyr::n_distinct(v, na.rm = TRUE))
  varied_params <- cand[lvl > 1 & lvl <= max_levels]
  
  # join (varied + x/y + outcome)
  keep_pg <- unique(c(id_col, x, y, varied_params))
  d <- dplyr::left_join(
    dplyr::select(param_grid, dplyr::all_of(keep_pg)),
    dplyr::select(sim_results, dplyr::all_of(c(id_col, overflow_col))),
    by = id_col
  )
  
  # validity + categories
  hi_lab <- paste0(">", valid_max)
  d <- dplyr::mutate(
    d,
    valid = .data[[overflow_col]] <= valid_max,
    overflow_cat = dplyr::case_when(
      .data[[overflow_col]] == 0 ~ "0",
      .data[[overflow_col]] == 1 ~ "1",
      TRUE ~ hi_lab
    )
  )
  
  # optionally drop all > valid_max
  if (isTRUE(drop_overflow_gt_valid_max)) {
    d <- dplyr::filter(d, .data[[overflow_col]] <= valid_max)
  }
  
  # tooltip: other varied params (excluding x/y)
  other_params <- setdiff(varied_params, c(x, y))
  
  fmt <- function(v) {
    if (is.numeric(v)) format(signif(v, digits), trim = TRUE, scientific = FALSE) else as.character(v)
  }
  
  other_block <- if (length(other_params) > 0) {
    m <- as.data.frame(d[other_params])
    m[] <- lapply(m, fmt)
    lines <- apply(m, 1, function(r) paste0(other_params, ": ", r, collapse = "<br>"))
    paste0("<br><br><b>Other parameters</b><br>", lines)
  } else {
    ""
  }
  
  d$hover <- paste0(
    "<b>", id_col, ":</b> ", d[[id_col]],
    "<br><b>", overflow_col, ":</b> ", fmt(d[[overflow_col]]),
    "<br><b>", x, ":</b> ", fmt(d[[x]]),
    "<br><b>", y, ":</b> ", fmt(d[[y]]),
    other_block
  )
  
  # color mapping: 0 darkgreen, 1 yellow, >1 red
  d <- dplyr::mutate(d, overflow_cat = factor(.data$overflow_cat, levels = c("0", "1", hi_lab)))
  pal <- c("0" = "darkgreen", "1" = "yellow", hi_lab = "red")
  
  # variable alpha by duplicates (same x/y)
  if (alpha_mode == "duplicates") {
    d <- d %>%
      dplyr::group_by(.data[[x]], .data[[y]]) %>%
      dplyr::mutate(dup_n = dplyr::n()) %>%
      dplyr::ungroup()
    
    # alpha decreases with duplicates; clamp to [alpha_min, alpha_max]
    alpha_dup <- alpha_max / sqrt(d$dup_n)
    alpha_dup <- pmax(alpha_min, pmin(alpha_max, alpha_dup))
    
    d$alpha_valid   <- alpha_dup
    d$alpha_invalid <- alpha_dup * alpha_invalid
  } else {
    d$alpha_valid   <- 0.95
    d$alpha_invalid <- alpha_invalid
  }
  
  # optional jitter
  if (isTRUE(jitter)) {
    xr <- range(d[[x]], na.rm = TRUE); yr <- range(d[[y]], na.rm = TRUE)
    dx <- diff(xr); dy <- diff(yr)
    if (is.null(jitter_width))  jitter_width  <- if (is.finite(dx) && dx > 0) jitter_factor * dx else 0
    if (is.null(jitter_height)) jitter_height <- if (is.finite(dy) && dy > 0) jitter_factor * dy else 0
    pos <- ggplot2::position_jitter(width = jitter_width, height = jitter_height)
  } else {
    pos <- ggplot2::position_identity()
  }
  
  if (isTRUE(drop_overflow_gt_valid_max)) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$overflow_cat, text = .data$hover, alpha = .data$alpha_valid),
        size = size + 0.6,
        position = pos
      ) +
      ggplot2::scale_colour_manual(values = pal, drop = TRUE) +
      ggplot2::scale_alpha_identity(guide = "none") +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = x, y = y, colour = overflow_col,
        title = paste0("Valid solutions (", overflow_col, " ≤ ", valid_max, ") in design space: ", x, " × ", y),
        subtitle = "Tooltip lists all other varied parameters"
      )
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(
        data = dplyr::filter(d, !.data$valid),
        ggplot2::aes(colour = .data$overflow_cat, text = .data$hover, alpha = .data$alpha_invalid),
        size = size,
        position = pos
      ) +
      ggplot2::geom_point(
        data = dplyr::filter(d, .data$valid),
        ggplot2::aes(colour = .data$overflow_cat, text = .data$hover, alpha = .data$alpha_valid),
        size = size + 0.8,
        position = pos
      ) +
      ggplot2::scale_colour_manual(values = pal, drop = FALSE) +
      ggplot2::scale_alpha_identity(guide = "none") +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = x, y = y, colour = overflow_col,
        title = paste0("Valid solutions (", overflow_col, " ≤ ", valid_max, ") in design space: ", x, " × ", y),
        subtitle = "Tooltip lists all other varied parameters"
      )
  }
  
  
  # keep full design-space limits even if >valid_max scenarios are dropped
  if (isTRUE(drop_overflow_gt_valid_max) && isTRUE(keep_param_grid_limits)) {
    
    # numeric axes -> fix via coord_cartesian using full param_grid range
    if (is.numeric(param_grid[[x]]) && is.numeric(param_grid[[y]])) {
      
      x_rng <- range(param_grid[[x]], na.rm = TRUE)
      y_rng <- range(param_grid[[y]], na.rm = TRUE)
      
      p <- p + ggplot2::coord_cartesian(xlim = x_rng, ylim = y_rng)
      
    } else {
      # discrete axes -> keep all levels
      p <- p +
        ggplot2::scale_x_discrete(limits = unique(param_grid[[x]]), drop = FALSE) +
        ggplot2::scale_y_discrete(limits = unique(param_grid[[y]]), drop = FALSE)
    }
  }
  
  p
}
