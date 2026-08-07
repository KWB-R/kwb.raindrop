# Default storage-type soil presets (Speicher layer)

Soil parameters of the storage (2nd Bodenschichtung) layer per storage
type, as used by the workflow vignettes: infiltration box ("Sickerbox",
thetaS 0.95) and gravel trench ("Schotterrigol", thetaS 0.3).

## Usage

``` r
default_storage_types()
```

## Value

Named list (per storage type) of lists with
`Startwerte_theta_ActualSoilMoisture`, `thetaWP_MoistureAtWiltingPoint`,
`thetaFC_MoistureAtFieldCapacity`, `thetaS_MoistureAtSaturation`.
