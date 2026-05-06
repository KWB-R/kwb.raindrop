# Vignette Input Data — Sources & Provenance

This directory ships small example input files used by the package
vignettes so they can be re-built locally and on CI without external
downloads.

## Layout

```
inst/extdata/models/
├── wien/
│   ├── base.h5    # HDF5 model template
│   ├── rain.csv   # 10-min precipitation, 2011-2025
│   └── et.csv     # Daily evapotranspiration (ET0), 2011-2025
├── badaussee/
│   ├── base.h5
│   ├── rain.csv
│   └── et.csv
└── eisenstadt-2005/
    └── base.h5
```

Access from R:

```r
system.file("extdata/models/wien/base.h5", package = "kwb.raindrop")
```

## Sources

### Precipitation (`rain.csv`) and evapotranspiration (`et.csv`)

Wien and Bad Aussee:
- **Provider**: Österreichischer Wetterdienst (GeoSphere Austria, formerly ZAMG)
- **Period**: 2011-2025
- **Original filenames**:
  - `<Station>_Zehnminutendaten_Niederschlag_v2_Datensatz_2011-2025.csv`
  - `<Station>_ET0_2011-2025.csv`
- **Renamed** to `rain.csv` / `et.csv` for canonical access via `system.file()`.

### Model templates (`base.h5`)

HDF5 input files produced with the Tandler "Regenwasserbewirtschaftung"
calculation engine. One template per case study, used as the starting
point for the vignette workflows.
