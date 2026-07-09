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
      storage_type                 = "Speichertyp",
      storage_volume_m3            = "Nutzbares Speichervolumen [m\u00b3]"
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
      storage_type                 = "Storage type",
      storage_volume_m3            = "Usable storage volume [m\u00b3]"
    )
  )
}

#' Short, language-specific display names for the storage_type values
#'
#' Used wherever the raw `storage_type` values appear as compact text: the
#' "varying parameters" tooltip block (`Speichertyp=Schotterrigol` instead of
#' `Speichertyp=gravel_trench`) and the x-axis of the `plot_main_effects()`
#' storage-type panel. The bold storage-type tooltip line keeps the longer
#' bilingual names from `cost_tooltip_labels()`.
#'
#' @param lang Character. `"de"` or `"en"`.
#' @return Named character vector (names = raw values).
#' @noRd
storage_type_value_labels <- function(lang = c("de", "en")) {
  lang <- match.arg(lang)
  switch(
    lang,
    de = c(infiltration_box = "Sickerbox",
           gravel_trench = "Schotterrigol"),
    en = c(infiltration_box = "Infiltration box",
           gravel_trench = "Gravel trench")
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
    # values_transform: numeric and character parameters (e.g. storage_type)
    # cannot share one `val` column otherwise.
    tidyr::pivot_longer(-"scenario_name",
                        names_to = "param",
                        values_to = "val",
                        values_transform = list(val = as.character)) %>%
    dplyr::mutate(
      val_chr = purrr::map_chr(.data$val, ~ paste(.x, collapse = ",")),
      val_num = suppressWarnings(as.numeric(.data$val_chr)),
      # format element-wise: a vectorised format() would pad every parameter
      # to the maximum number of decimals in the column (e.g. "100.00"
      # because another parameter has value 0.95).
      val_fmt = ifelse(
        is.na(.data$val_num),
        .data$val_chr,
        vapply(.data$val_num,
               function(v) format(round(v, digits_params), trim = TRUE,
                                  scientific = FALSE),
               character(1))
      ),
      # storage_type values get their short display names
      # (Speichertyp=Schotterrigol instead of =gravel_trench)
      val_fmt = ifelse(
        .data$param == "storage_type" &
          .data$val_chr %in% names(storage_type_value_labels(lang)),
        unname(storage_type_value_labels(lang)[.data$val_chr]),
        .data$val_fmt
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
      tt_cost_per_evap    = "Kosten je % Verdunstung [\u20ac/%]",
      tt_cost_excavation  = "Aushub",
      tt_cost_profiling   = "Profilierung + Begr\u00fcnung",
      tt_cost_filter      = "Bodenfilter",
      tt_cost_storage     = "Speicherschicht",
      tt_storage_type     = "Speichertyp",
      tt_storage_volume   = "Nutzbares Speichervolumen [m\u00b3]",
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
      tt_cost_per_evap    = "Cost per % evapotranspiration [\u20ac/%]",
      tt_cost_excavation  = "Excavation",
      tt_cost_profiling   = "Profiling + greening",
      tt_cost_filter      = "Soil filter",
      tt_cost_storage     = "Storage layer",
      tt_storage_type     = "Storage type",
      tt_storage_volume   = "Usable storage volume [m\u00b3]",
      st_infiltration_box = "Infiltration box / Sickerbox",
      st_gravel_trench    = "Gravel trench / Schotterrigol",
      tt_costs_header     = "Cost breakdown [\u20ac]",
      tt_params           = "Varying parameters"
    )
  )
}

#' Storage-type display factor and marker shapes for the cost plots
#'
#' Maps the raw `storage_type` values to their bilingual display names (from
#' `cost_tooltip_labels()`) and to the fixed marker shapes shared by all cost
#' plots: **filled square (15) = infiltration box (Sickerbox)**, **filled
#' triangle (17) = gravel trench (Schotterrigol)**. Values that are `NA` or
#' unknown fall back to the infiltration box, mirroring `cost_tooltip_text()`.
#'
#' @param storage_type Character vector of raw values
#'   (`"infiltration_box"` / `"gravel_trench"`).
#' @param tt Label list from `cost_tooltip_labels()`.
#' @return List with `display` (factor, infiltration box level first) and
#'   `shape_values` (named vector for `ggplot2::scale_shape_manual()`).
#' @noRd
storage_type_shapes <- function(storage_type, tt) {
  raw <- as.character(storage_type)
  disp <- ifelse(!is.na(raw) & raw == "gravel_trench",
                 tt$st_gravel_trench, tt$st_infiltration_box)
  lvls <- c(tt$st_infiltration_box, tt$st_gravel_trench)
  list(
    display = factor(disp, levels = lvls),
    shape_values = stats::setNames(c(15, 17), lvls)
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
  # Derived cost efficiency: total cost per percentage point of element
  # evapotranspiration [EUR/%]; undefined ("-") when evapotranspiration is 0.
  evap <- df[["element.WB_Evapotranspiration_"]]
  cpe <- ifelse(!is.na(df$cost_total) & !is.na(evap) & evap > 0,
                df$cost_total / evap, NA_real_)
  cpe_fmt <- vapply(cpe, function(v) {
    if (is.na(v)) "-" else format(round(v, 0), big.mark = " ", trim = TRUE)
  }, character(1))
  # Usable storage volume of the storage layer [m3]: area x height x usable
  # porosity (thetaS - thetaFC) of the storage type. Taken from a precomputed
  # storage_volume_m3 column when available, otherwise derived from the theta
  # columns; the line is omitted for result sets carrying neither.
  storage_volume <- if ("storage_volume_m3" %in% names(df)) {
    df$storage_volume_m3
  } else if (all(c("mulde_area", "storage_height", "storage_thetaS",
                   "storage_thetaFC") %in% names(df))) {
    df$mulde_area * df$storage_height / 1000 *
      (df$storage_thetaS - df$storage_thetaFC)
  } else {
    NULL
  }
  storage_volume_line <- if (is.null(storage_volume)) {
    ""
  } else {
    paste0("<br>", tt$tt_storage_volume, ": ", round(storage_volume, digits))
  }
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
    storage_volume_line,
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
    "<br>", tt$tt_cost_per_evap, ": ", cpe_fmt,
    "<br><br><b>", tt$tt_params, "</b><br>", df$params_html
  )
}
