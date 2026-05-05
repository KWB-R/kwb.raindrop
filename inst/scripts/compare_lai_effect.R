# Compare the effect of LAI = 3.9 (grass) vs LAI = 8.5 (status quo) on the
# Mulde-Rigole water balance for the Wien / Bad Aussee scenario sweeps.
#
# Reads the CSV produced by the analyse_results chunk in
# vignettes/workflow_<modelname>.Rmd, prints aggregated and paired-difference
# statistics, and writes a 3-page PDF illustrating the LAI effect on the
# evapotranspiration share.
#
# Run from the project root, e.g.:
#   Rscript inst/scripts/compare_lai_effect.R
#   Rscript inst/scripts/compare_lai_effect.R BadAussee
#
# Expects the corresponding CSV in vignettes/.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
modelname <- if (length(args) >= 1) args[[1]] else "Wien"

csv_path <- sprintf("vignettes/simulation_results_optimisation_%s.csv", modelname)
out_pdf  <- sprintf("vignettes/simulation_results_optimisation_%s_lai-effect-ET.pdf",
                    modelname)

if (!file.exists(csv_path)) {
  stop("CSV not found: ", csv_path,
       "\nRun the analyse_results chunk in vignettes/workflow_",
       tolower(modelname), ".Rmd first.")
}

result <- readr::read_csv(csv_path, show_col_types = FALSE)
if (!"lai" %in% names(result)) {
  stop("CSV has no 'lai' column. Re-run the vignette with the LAI sweep enabled.")
}

cat("rows:", nrow(result), "  cols:", ncol(result), "\n")
cat("LAI distribution:\n"); print(table(result$lai))

# --- Aggregated effect ---
cat("\n=== Aggregated effect of LAI on water balance (medians per LAI level) ===\n")
agg <- result %>%
  group_by(lai) %>%
  summarise(
    n = n(),
    median_n_overflows = median(n_overflows, na.rm = TRUE),
    median_sum_overflows_mm = round(median(sum_overflows, na.rm = TRUE), 1),
    median_ET_pct = round(median(element.WB_Evapotranspiration_, na.rm = TRUE), 1),
    median_Inf_pct = round(median(element.WB_InfiltrationNetto_, na.rm = TRUE), 1),
    median_Ueberlauf_pct = round(median(element.WB_Oberflaechenablauf_Ueberlauf_, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(agg)

# --- Paired comparison: same physical params, different LAI ---
keys <- c("connected_area", "mulde_area", "mulde_height",
          "filter_hydraulicconductivity", "filter_height",
          "storage_height", "bottom_hydraulicconductivity", "rain_factor")

paired <- result %>%
  select(all_of(c(keys, "lai", "n_overflows", "sum_overflows",
                  "element.WB_Evapotranspiration_",
                  "element.WB_InfiltrationNetto_",
                  "element.WB_Oberflaechenablauf_Ueberlauf_"))) %>%
  pivot_wider(names_from = lai,
              values_from = c("n_overflows", "sum_overflows",
                              "element.WB_Evapotranspiration_",
                              "element.WB_InfiltrationNetto_",
                              "element.WB_Oberflaechenablauf_Ueberlauf_"),
              names_sep = "_lai") %>%
  mutate(
    delta_n_overflows = `n_overflows_lai8.5` - `n_overflows_lai3.9`,
    delta_ET_pct = round(`element.WB_Evapotranspiration__lai8.5` - `element.WB_Evapotranspiration__lai3.9`, 2),
    delta_Inf_pct = round(`element.WB_InfiltrationNetto__lai8.5` - `element.WB_InfiltrationNetto__lai3.9`, 2),
    delta_Ueberlauf_pct = round(`element.WB_Oberflaechenablauf_Ueberlauf__lai8.5` -
                                `element.WB_Oberflaechenablauf_Ueberlauf__lai3.9`, 2)
  )

cat("\n=== Paired delta (LAI 8.5 minus LAI 3.9), n =", nrow(paired), "pairs ===\n")
cat("delta_n_overflows:\n"); print(summary(paired$delta_n_overflows))
cat("delta_ET_pct:\n"); print(summary(paired$delta_ET_pct))
cat("delta_Inf_pct:\n"); print(summary(paired$delta_Inf_pct))
cat("delta_Ueberlauf_pct:\n"); print(summary(paired$delta_Ueberlauf_pct))

# --- Reference scenario after Daniel Wicke's WaBiLa cross-check
#     (mulde_area=75, mulde_height=200, filter_kf=36, storage_height=500) ---
cat("\n=== Reference scenario (mulde_area=75, mulde_height=200, filter_kf=36, storage_height=500) ===\n")
focus <- result %>%
  filter(mulde_area == 75, mulde_height == 200,
         filter_hydraulicconductivity == 36, storage_height == 500) %>%
  select(scenario_name, lai, n_overflows, sum_overflows,
         element.WB_Evapotranspiration_,
         element.WB_InfiltrationNetto_,
         element.WB_Oberflaechenablauf_Ueberlauf_)
print(as.data.frame(focus))

# --- Plots ---
plot_df <- result %>%
  mutate(lai_label = factor(sprintf("LAI = %g", lai),
                            levels = c("LAI = 3.9", "LAI = 8.5")))

plot_pairs <- result %>%
  select(all_of(c(keys, "lai", "element.WB_Evapotranspiration_"))) %>%
  group_by(across(all_of(keys))) %>%
  mutate(pair_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(lai_label = factor(sprintf("LAI = %g", lai),
                            levels = c("LAI = 3.9", "LAI = 8.5")))

p1 <- ggplot(plot_pairs,
             aes(x = lai_label, y = element.WB_Evapotranspiration_)) +
  geom_line(aes(group = pair_id), colour = "grey70", alpha = 0.25) +
  geom_violin(fill = NA, colour = "steelblue") +
  geom_boxplot(width = 0.18, outlier.alpha = 0, fill = "white") +
  geom_jitter(width = 0.06, alpha = 0.25, size = 0.9) +
  theme_bw() +
  labs(
    title = sprintf("Effekt des Blattflaechenindex (Mulde-Rigole) auf den Verdunstungsanteil (%s)",
                    modelname),
    subtitle = sprintf("%d gepaarte Szenarien (graue Linien verbinden gleiche Designpunkte)",
                       nrow(plot_pairs) / 2),
    x = NULL,
    y = "Verdunstung am Mulden-Element (% der Bilanz)"
  )

p2 <- ggplot(plot_df,
             aes(x = factor(mulde_area), y = element.WB_Evapotranspiration_,
                 fill = lai_label)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("LAI = 3.9" = "#74c476", "LAI = 8.5" = "#fd8d3c"),
                    name = NULL) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = sprintf("Verdunstungsanteil nach Muldenflaeche und LAI (%s)", modelname),
    x = "Muldenflaeche [m^2]",
    y = "Verdunstung am Mulden-Element (% der Bilanz)"
  )

p3 <- ggplot(paired,
             aes(x = `element.WB_Evapotranspiration__lai3.9`,
                 y = `element.WB_Evapotranspiration__lai8.5`,
                 colour = factor(mulde_area))) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_colour_viridis_d(name = "Muldenflaeche [m^2]") +
  theme_bw() +
  labs(
    title = sprintf("Verdunstungsanteil bei LAI 8.5 vs LAI 3.9 (%s)", modelname),
    subtitle = "Jeder Punkt = 1 Designpunkt; oberhalb der Diagonalen: hoeherer ET-Anteil bei LAI 8.5",
    x = "Verdunstung bei LAI = 3.9 (% der Bilanz)",
    y = "Verdunstung bei LAI = 8.5 (% der Bilanz)"
  )

pdf(out_pdf, width = 9, height = 5, onefile = TRUE)
print(p1); print(p2); print(p3)
invisible(dev.off())
cat("\nSaved plot:", out_pdf, "\n")
