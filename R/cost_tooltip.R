#' German / English labels for optimisation parameter-grid columns
#'
#' Maps the raw `param_grid` column names produced by the case-study workflows
#' to human-readable, unit-carrying labels. Used to translate the
#' "varying parameters" block in the interactive tooltips of
#' [plot_cost_vs_overflow_volume()] and [plot_cost_overflow_boxplot()], so a
#' hovered point shows e.g. `Muldenflaeche [m2]=125` instead of the raw
#' `mulde_area=125`.
#'
#' Unknown columns fall back to their raw name, so a grid gaining a new column
#' still renders (just untranslated). Override individual entries or pass your
#' own named vector via the `param_labels` argument of the plot functions.
#'
#' @param lang Character. `"de"` or `"en"`.
#'
#' @return A named `character` vector: names are `param_grid` column names,
#'   values are the display labels.
#'
#' @export
#'
#' @examples
#' default_param_labels("de")[["mulde_area"]]
#' default_param_labels("en")[["storage_height"]]
default_param_labels <- function(lang = c("de", "en")) {
  lang <- match.arg(lang)
  switch(
    lang,
    de = c(
      connected_area               = "Angeschlossene Fl\u00e4che [m\u00b2]",
      mulde_area                   = "Muldenfl\u00e4che [m\u00b2]",
      mulde_height                 = "Muldenh\u00f6he [mm]",
      filter_hydraulicconductivity = "Filter-Leitf\u00e4higkeit kf [mm/h]",
      filter_height                = "Filterh\u00f6he [mm]",
      storage_height               = "Speicherh\u00f6he [mm]",
      bottom_hydraulicconductivity = "Sohl-Leitf\u00e4higkeit kf [mm/h]",
      rain_factor                  = "Regenfaktor [-]",
      lai                          = "Blattfl\u00e4chenindex LAI [-]",
      storage_type                 = "Speichertyp"
    ),
    en = c(
      connected_area               = "Connected area [m\u00b2]",
      mulde_area                   = "Swale area [m\u00b2]",
      mulde_height                 = "Swale depth [mm]",
      filter_hydraulicconductivity = "Filter conductivity kf [mm/h]",
      filter_height                = "Filter thickness [mm]",
      storage_height               = "Storage thickness [mm]",
      bottom_hydraulicconductivity = "Subsoil conductivity kf [mm/h]",
      rain_factor                  = "Rain factor [-]",
      lai                          = "Leaf area index LAI [-]",
      storage_type                 = "Storage type"
    )
  )
}

#' Per-scenario HTML of the varying parameter-grid entries (translated)
#'
#' Detects the `param_grid` columns that vary across scenarios (excluding
#' `scenario_name`), formats their values, translates the parameter names via
#' `param_labels`, and collapses them into one `<br>`-separated HTML string per
#' scenario for use in a plotly tooltip.
#'
#' @param param_grid Data frame with a `scenario_name` column.
#' @param lang Character. `"de"` or `"en"`.
#' @param param_labels Named character vector mapping columns to labels, or
#'   `NULL` to use [default_param_labels()].
#' @param digits_params Integer. Rounding for numeric parameter values.
#'
#' @return A tibble with columns `scenario_name` and `params_html`.
#'
#' @importFrom dplyr %>% select summarise across everything n_distinct filter
#' @importFrom dplyr pull mutate group_by all_of coalesce
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom rlang .data
#' @noRd
build_varying_param_html <- function(param_grid, lang = c("de", "en"),
                                     param_labels = NULL, digits_params = 4L) {
  lang <- match.arg(lang)
  if (is.null(param_labels)) param_labels <- default_param_labels(lang)

  varying_params <- param_grid %>%
    dplyr::select(-"scenario_name") %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~ dplyr::n_distinct(.) > 1)) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "param",
                        values_to = "vary") %>%
    dplyr::filter(.data$vary) %>%
    dplyr::pull("param")

  if (length(varying_params) == 0) {
    return(
      param_grid %>%
        dplyr::select("scenario_name") %>%
        dplyr::mutate(params_html = "")
    )
  }

  param_grid %>%
    dplyr::select("scenario_name", dplyr::all_of(varying_params)) %>%
    tidyr::pivot_longer(-"scenario_name",
                        names_to = "param",
                        values_to = "val") %>%
    dplyr::mutate(
      val_chr = purrr::map_chr(.data$val, ~ paste(.x, collapse = ",")),
      val_num = suppressWarnings(as.numeric(.data$val_chr)),
      val_fmt = ifelse(
        is.na(.data$val_num),
        .data$val_chr,
        format(round(.data$val_num, digits_params), trim = TRUE)
      ),
      param_label = dplyr::coalesce(unname(param_labels[.data$param]),
                                    .data$param),
      kv = paste0(.data$param_label, "=", .data$val_fmt)
    ) %>%
    dplyr::group_by(.data$scenario_name) %>%
    dplyr::summarise(params_html = paste(.data$kv, collapse = "<br>"),
                     .groups = "drop")
}

#' Shared tooltip labels for the cost plots
#'
#' The `tt_*` label set used to assemble the (identical) plotly tooltip of
#' [plot_cost_vs_overflow_volume()] and [plot_cost_overflow_boxplot()].
#'
#' @param lang Character. `"de"` or `"en"`.
#' @return Named list of label strings.
#' @noRd
cost_tooltip_labels <- function(lang = c("de", "en")) {
  lang <- match.arg(lang)
  switch(
    lang,
    de = list(
      tt_scenario         = "Szenario",
      tt_n_overflows      = "Anzahl \u00dcberlaufereignisse",
      tt_sum_overflows_mm = "Summe \u00dcberl\u00e4ufe [mm]",
      tt_overflow_volume  = "\u00dcberlaufvolumen [m\u00b3]",
      tt_wb_header        = "Wasserhaushalt [%]",
      tt_wb_evap          = "Verdunstung",
      tt_wb_infil         = "Versickerung",
      tt_wb_overflow      = "\u00dcberlauf",
      tt_cost_total       = "Gesamtkosten",
      tt_cost_excavation  = "Aushub",
      tt_cost_profiling   = "Profilierung + Begr\u00fcnung",
      tt_cost_filter      = "Bodenfilter",
      tt_cost_storage     = "Speicherschicht",
      tt_storage_type     = "Speichertyp",
      st_infiltration_box = "Sickerbox / Infiltration box",
      st_gravel_trench    = "Schotterrigol / Gravel trench",
      tt_costs_header     = "Kostenaufteilung [\u20ac]",
      tt_params           = "Variierende Parameter"
    ),
    en = list(
      tt_scenario         = "Scenario",
      tt_n_overflows      = "Number of overflow events",
      tt_sum_overflows_mm = "Sum of overflows [mm]",
      tt_overflow_volume  = "Overflow volume [m\u00b3]",
      tt_wb_header        = "Water balance [%]",
      tt_wb_evap          = "Evapotranspiration",
      tt_wb_infil         = "Infiltration",
      tt_wb_overflow      = "Overflow",
      tt_cost_total       = "Total cost",
      tt_cost_excavation  = "Excavation",
      tt_cost_profiling   = "Profiling + greening",
      tt_cost_filter      = "Soil filter",
      tt_cost_storage     = "Storage layer",
      tt_storage_type     = "Storage type",
      st_infiltration_box = "Infiltration box / Sickerbox",
      st_gravel_trench    = "Gravel trench / Schotterrigol",
      tt_costs_header     = "Cost breakdown [\u20ac]",
      tt_params           = "Varying parameters"
    )
  )
}

#' Assemble the shared cost-plot tooltip HTML for each row of `df`
#'
#' `df` must carry `scenario_name`, `n_overflows`, `sum_overflows`,
#' `overflow_volume_m3`, the three `element.WB_*` shares, the five `cost_*`
#' columns, `storage_type` and `params_html`. Returns one HTML string per row.
#' Both cost plots call this so their tooltips are byte-identical.
#'
#' @param df Data frame with the columns listed above.
#' @param tt Label list from `cost_tooltip_labels()`.
#' @param digits Integer. Rounding for the numeric tooltip values.
#' @return Character vector, length `nrow(df)`.
#' @noRd
cost_tooltip_text <- function(df, tt, digits = 2L) {
  st_raw <- if ("storage_type" %in% names(df)) {
    as.character(df$storage_type)
  } else {
    rep(NA_character_, nrow(df))
  }
  st_disp <- ifelse(!is.na(st_raw) & st_raw == "gravel_trench",
                    tt$st_gravel_trench, tt$st_infiltration_box)
  paste0(
    tt$tt_scenario, ": ", df$scenario_name,
    "<br>", tt$tt_n_overflows, ": ", df$n_overflows,
    "<br>", tt$tt_sum_overflows_mm, ": ", round(df$sum_overflows, digits),
    "<br>", tt$tt_overflow_volume, ": ", round(df$overflow_volume_m3, digits),
    "<br><br><b>", tt$tt_wb_header, "</b>",
    "<br>", tt$tt_wb_evap, ": ",
    round(df[["element.WB_Evapotranspiration_"]], digits),
    "<br>", tt$tt_wb_infil, ": ",
    round(df[["element.WB_InfiltrationNetto_"]], digits),
    "<br>", tt$tt_wb_overflow, ": ",
    round(df[["element.WB_Oberflaechenablauf_Ueberlauf_"]], digits),
    "<br><br><b>", tt$tt_storage_type, ": ", st_disp, "</b>",
    "<br><br><b>", tt$tt_costs_header, "</b>",
    "<br>", tt$tt_cost_excavation, ": ",
    format(round(df$cost_excavation, 0), big.mark = " ", trim = TRUE),
    "<br>", tt$tt_cost_profiling, ": ",
    format(round(df$cost_profiling, 0), big.mark = " ", trim = TRUE),
    "<br>", tt$tt_cost_filter, ": ",
    format(round(df$cost_filter, 0), big.mark = " ", trim = TRUE),
    "<br>", tt$tt_cost_storage, ": ",
    format(round(df$cost_storage, 0), big.mark = " ", trim = TRUE),
    "<br><b>", tt$tt_cost_total, ": ",
    format(round(df$cost_total, 0), big.mark = " ", trim = TRUE), "</b>",
    "<br><br><b>", tt$tt_params, "</b><br>", df$params_html
  )
}
