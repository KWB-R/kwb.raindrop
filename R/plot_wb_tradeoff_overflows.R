#' Trade-off-Plot: Infiltration vs. Evapotranspiration (farbcodiert nach Overflows)
#'
#' Erstellt einen interaktiven Scatter-Plot (plotly) aus Optimierungsergebnissen:
#' x = Element-Infiltration [%], y = Element-Evapotranspiration [%], Farbe = n_overflows (0/1),
#' Tooltip enthält zusätzlich die variierenden Parameter aus `param_grid`.
#'
#' @param simulation_results_optimisation Data frame mit Simulationsergebnissen.
#'   Benötigte Spalten (mindestens): `scenario_name`, `n_overflows`, `sum_overflows`,
#'   `element.WB_InfiltrationNetto_`, `element.WB_Evapotranspiration_`,
#'   `element.WB_Oberflaechenablauf_Ueberlauf_`.
#' @param param_grid Data frame mit Parametergrid; muss `scenario_name` enthalten.
#' @param filter_n_gt1 Logical; wenn TRUE werden Szenarien mit `n_overflows > 1` entfernt.
#' @param use_jitter Logical; wenn TRUE wird ein leichter Jitter gegen Overplotting genutzt.
#' @param jitter_width,jitter_height Numerisch; Jitter-Stärke in x/y (nur wenn `use_jitter = TRUE`).
#' @param jitter_seed Integer; Seed für reproduzierbaren Jitter.
#' @param digits Integer; Rundungsstellen im Tooltip für Wasserbilanzwerte.
#' @param digits_params Integer; Rundungsstellen im Tooltip für Parameterwerte (numerisch).
#'
#' @return Ein `plotly`-Objekt (interaktiv).
#'
#' @export
#'
#' @importFrom dplyr %>% select summarise across everything n_distinct
#' @importFrom dplyr filter pull mutate group_by left_join if_else case_when
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual labs theme_minimal position_jitter
#' @importFrom plotly ggplotly
#'
#' @examples
#' \dontrun{
#' p <- plot_wb_tradeoff_overflows(
#'   simulation_results_optimisation = simulation_results_optimisation,
#'   param_grid = param_grid,
#'   filter_n_gt1 = TRUE,
#'   use_jitter = TRUE
#' )
#' p
#' }
plot_wb_tradeoff_overflows <- function(simulation_results_optimisation,
                                       param_grid,
                                       filter_n_gt1 = TRUE,
                                       use_jitter = TRUE,
                                       jitter_width = 0.15,
                                       jitter_height = 0.15,
                                       jitter_seed = 1L,
                                       digits = 2L,
                                       digits_params = 4L) {
  
  # --- minimal checks ---------------------------------------------------------
  req_grid <- c("scenario_name")
  req_res  <- c(
    "scenario_name", "n_overflows", "sum_overflows",
    "element.WB_InfiltrationNetto_",
    "element.WB_Evapotranspiration_",
    "element.WB_Oberflaechenablauf_Ueberlauf_"
  )
  
  miss_grid <- setdiff(req_grid, names(param_grid))
  miss_res  <- setdiff(req_res,  names(simulation_results_optimisation))
  
  if (length(miss_grid) > 0) {
    stop("param_grid fehlt Spalte(n): ", paste(miss_grid, collapse = ", "))
  }
  if (length(miss_res) > 0) {
    stop("simulation_results_optimisation fehlt Spalte(n): ", paste(miss_res, collapse = ", "))
  }
  
  # --- Variierende Parameter im Grid ermitteln --------------------------------
  varying_params <- param_grid %>%
    select(-scenario_name) %>%
    summarise(across(everything(), ~ n_distinct(.) > 1)) %>%
    pivot_longer(everything(), names_to = "param", values_to = "vary") %>%
    filter(vary) %>%
    pull(param)
  
  # --- Tooltip-HTML für die variierenden Parameter bauen (robust) -------------
  if (length(varying_params) == 0) {
    param_tooltip <- param_grid %>%
      select(scenario_name) %>%
      mutate(params_html = "")
  } else {
    param_tooltip <- param_grid %>%
      select(scenario_name, dplyr::all_of(varying_params)) %>%
      pivot_longer(-scenario_name, names_to = "param", values_to = "val") %>%
      mutate(
        # robust: auch list-columns zu single string
        val_chr = map_chr(val, ~ paste(.x, collapse = ",")),
        val_num = suppressWarnings(as.numeric(val_chr)),
        val_fmt = ifelse(
          is.na(val_num),
          val_chr,
          format(round(val_num, digits_params), trim = TRUE)
        ),
        kv = paste0(param, "=", val_fmt)
      ) %>%
      group_by(scenario_name) %>%
      summarise(params_html = paste(kv, collapse = "<br>"), .groups = "drop")
  }
  
  # --- Results vorbereiten ----------------------------------------------------
  df <- simulation_results_optimisation %>%
    mutate(
      overflow_class = case_when(
        n_overflows <= 0 ~ "0",
        n_overflows == 1 ~ "1",
        TRUE ~ ">1"
      ),
      overflow_flag = if_else(n_overflows > 0, 1, 0),
      
      evap_pct     = element.WB_Evapotranspiration_,
      infil_pct    = element.WB_InfiltrationNetto_,
      overflow_pct = element.WB_Oberflaechenablauf_Ueberlauf_,
      
      evap_pos     = pmax(evap_pct, 0),
      infil_pos    = pmax(infil_pct, 0),
      overflow_pos = pmax(overflow_pct, 0),
      out_sum      = evap_pos + infil_pos + overflow_pos,
      
      evap_share     = if_else(out_sum > 0, evap_pos / out_sum, NA_real_),
      infil_share    = if_else(out_sum > 0, infil_pos / out_sum, NA_real_),
      overflow_share = if_else(out_sum > 0, overflow_pos / out_sum, NA_real_)
    )
  
  # nur 0/1 (optional)
  df_p1 <- df %>%
    filter(!filter_n_gt1 | n_overflows <= 1) %>%
    left_join(param_tooltip, by = "scenario_name") %>%
    mutate(
      overflow_class = factor(n_overflows, levels = c(0, 1), labels = c("0", "1"))
    )
  
  cols_valid <- c("0" = "darkgreen", "1" = "goldenrod2")
  pos <- if (isTRUE(use_jitter)) {
    position_jitter(width = jitter_width, height = jitter_height, seed = jitter_seed)
  } else {
    "identity"
  }
  
  # --- Plot -------------------------------------------------------------------
  p1 <- ggplot(df_p1, aes(
    x = element.WB_InfiltrationNetto_,
    y = element.WB_Evapotranspiration_,
    color = overflow_class,
    text = paste0(
      "scenario: ", scenario_name,
      "<br>n_overflows: ", n_overflows,
      "<br>infil%: ", round(element.WB_InfiltrationNetto_, digits),
      "<br>evap%: ", round(element.WB_Evapotranspiration_, digits),
      "<br>overflow%: ", round(element.WB_Oberflaechenablauf_Ueberlauf_, digits),
      "<br>sum_overflows: ", sum_overflows,
      "<br><br><b>varying params</b><br>", params_html
    )
  )) +
    geom_point(alpha = 0.7, position = pos) +
    scale_color_manual(values = cols_valid, drop = FALSE) +
    labs(
      title = "Overflows (Anzahl) vs Wasserbilanz (Element): Infiltration ↔ Verdunstung",
      x = "Element: Infiltration [%]",
      y = "Element: Evapotranspiration [%]",
      color = "n_overflows (0/1)"
    ) +
    theme_minimal()

}
