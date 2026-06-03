# Eisenstadt 2005 — SWMM time series & ET0 units note

Two SWMM-5 external time-series files extracted from the Eisenstadt engine
template `../base.h5`, for cross-checking the Tandler calculation kernel
against SWMM-UrbanEVA:

| File                 | Content                         | Units in file                  | Rows   |
|----------------------|---------------------------------|--------------------------------|--------|
| `eisenstadt_rain.dat`| Rainfall, year 2005, 10-min     | mm per 10-min interval (VOLUME)| 52 560 |
| `eisenstadt_et0.dat` | Reference ET0, year 2005, daily | mm/day                         | 365    |

Regenerate with:

```r
source(system.file("scripts/prepare_eisenstadt_swmm_timeseries.R",
                   package = "kwb.raindrop"))
export_eisenstadt_swmm_timeseries(out_dir = "<target>")
```

## The units question (mm/h vs mm/d) — short answer

**The engine curves `/Kurven/Regen` and `/Kurven/ET0` are RATES in mm/h on
an hour-based time axis** (the axis runs `0 .. 8759.833 h` = one year, step
1/6 h = 10 min). Evidence:

* **Rain** only makes physical sense as mm/h: `sum(rate) * (1/6 h) = 657 mm/a`
  for this template. Reading the same numbers as "mm per 10-min interval"
  gives `3943 mm/a` — impossible for eastern Austria.
* The **Wien / Bad Aussee vignettes explicitly convert** rain from mm-per-interval
  to mm/h before writing the curve
  (`workflow_wien.Rmd`: `### Convert rain from mm to mm/h; value <- value / period`).

So the calculation kernel **expects ET0 in mm/h as well**. This is exactly
the trap Daniel flagged: a value supplied in **mm/day** but written into the
mm/h curve is integrated by the engine as mm/h and ends up **24× too high**.

### Where this bites

* **Wien / Bad Aussee:** the vignettes read a *daily* ET0 CSV (`et.csv`,
  mm/day) and write it to `/Kurven/ET0` **without** the `/ 24` (or `/ period`)
  step that rain gets — see `workflow_wien.Rmd`, the `vals$.../Kurven/ET0 <- timeseries_et`
  line. If the kernel reads that curve as mm/h, Wien ET0 is ~24× too high,
  which is consistent with the implausibly high ~46 % evaporation share.
* **Eisenstadt:** the Eisenstadt vignettes do **not** inject any ET0/rain CSV
  (there is no `et.csv` for Eisenstadt). They run on this template's curves,
  i.e. a **constant placeholder ET0 = 0.2 mm/h (= 4.8 mm/d, 1752 mm/a)**.
  That modest constant — not a measured series — is why Eisenstadt's
  evaporation share (~17 %) lands lower than Wien's, while both stay too high.

### Conversion applied in these files

| Quantity | Engine curve | SWMM file | Conversion              |
|----------|--------------|-----------|-------------------------|
| Rain     | mm/h         | mm/10-min | `value[mm/h] * (10/60)` |
| ET0      | mm/h         | mm/day    | `value[mm/h] * 24`      |

> **Caveat — what these files represent:** all three bundled `base.h5`
> templates (Wien, Bad Aussee, Eisenstadt) are byte-identical, so the rain
> curve here is the template's 10-min year (657 mm/a) and the ET0 curve is
> the constant 0.2 mm/h placeholder. They reproduce exactly what the
> Eisenstadt *model template* feeds the kernel — **not** a measured
> Eisenstadt meteorology. For a real Eisenstadt run, point
> `export_eisenstadt_swmm_timeseries(path_h5 = ...)` at the actual server
> input `.h5`; the same mm/h → SWMM conversion then yields the correct CSVs.
