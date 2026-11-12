# H5 overrides that turn the base.h5 roof into a roof-type scenario

Returns a named list of `base.h5` parameter overrides (same layout as
[`h5_read_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_read_values.md)
output, merge via
[`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html)) that
parametrises the `Dach` element as one of three roof variants:

- `"conventional"` – the `base.h5` defaults, untouched. The sealed roof
  of the published status-quo runs, whose ET is exactly 0 even when the
  workflows switch `Evapotranspiration_aktiv` on, because (a)
  `Infiltration_aktiv = 0` keeps water out of the substrate while the
  Verschaltung empties the pond before pond evaporation acts, and (b)
  `SL = 0` plus a start moisture below the wilting point disable
  interception and transpiration (Tandler, 2026-10-08, answers 1-2).

- `"extensive"` – green roof, 80 mm substrate, `KC = 0.94`, `LAI = 3.9`.

- `"intensive"` – green roof, 200 mm substrate, `KC = 1.05`,
  `LAI = 5.0`.

## Usage

``` r
roof_type_values(
  roof_type = c("conventional", "extensive", "intensive"),
  hoernschemeyer_aktiv = 1L
)
```

## Arguments

- roof_type:

  One of `"conventional"`, `"extensive"`, `"intensive"`.

- hoernschemeyer_aktiv:

  Integer `0`/`1` (default `1`), or `NULL` to omit the global switch
  from the overrides.

## Value

Named list of `base.h5` overrides; empty except for the global switch
when `roof_type = "conventional"`.

## Details

Green-roof values were derived by Daniel Wicke (KWB) from SWMM-UrbanEVA
(Hoernschemeyer et al.), v2 of 2026-10-13: the LAI values of the two
variants were swapped relative to v1 (they were inconsistent with KC;
Tandler review, 2026-10-09). The substrate soil ("Bodenart Dach") is
deliberately identical for both green roofs – consistent with the KWB
KEYS parameterisation (`KWB-R/keys.lid`,
`swmm_lid-parameterisation.xlsx`, sheet `green_roof`), which also
differentiates extensive/intensive only by thickness. Differentiation
happens via substrate thickness and vegetation (KC/LAI).

Both green roofs set `Evapotranspiration_aktiv = 1` and
`Infiltration_aktiv = 1` for the roof (base.h5 ships both per-element
switches off; rain must enter the substrate to be stored and evaporated)
and `Endversickerungsrate = 0` (a roof is sealed underneath; the base.h5
value of 36 mm/h would silently drain the substrate out of the balance
once infiltration is active) – Tandler, 2026-10-08, answers 1-3, and
2026-10-09, point 3.

The global `Hoernschemeyer_aktiv` switch is part of the scenario frame:
it exists only at `//Berechnungsparameter/` (no per-element variant;
Tandler, 2026-10-08, answer 4) and gates the seasonal Growth/Shading
modulation, so it defaults to `1` here for every roof type – including
`"conventional"`, which keeps the swale comparable across all three
variants. Pass `hoernschemeyer_aktiv = NULL` to leave the base.h5 value
(0, legacy ET path) untouched, e.g. to replicate the published
status-quo runs bit for bit.

The growth curve itself is not part of these overrides: write the
monthly factors with
[`make_growth_curve()`](https://kwb-r.github.io/kwb.raindrop/reference/make_growth_curve.md)
into `//Kurven/Growth_1` (shared by Dach and Mulde_Rigole) or into an
additional named curve (e.g. `//Kurven/Growth_Dach`, created via
[`h5_ensure_datasets_from_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_ensure_datasets_from_values.md))
referenced by
`//Massnahmenelemente/Dach/Parameter_Evapotranspiration/Kurve_gfdoy`.

## Examples

``` r
str(roof_type_values("extensive"))
#> List of 14
#>  $ //Berechnungsparameter/Hoernschemeyer_aktiv                                     : int 1
#>  $ //Bodenarten/Dach/Ks_HydraulicConductivity                                      : num 90
#>  $ //Bodenarten/Dach/Psi_Saugspannung_CapillarySuction                             : num 10
#>  $ //Bodenarten/Dach/thetaFC_MoistureAtFieldCapacity                               : num 0.35
#>  $ //Bodenarten/Dach/thetaS_MoistureAtSaturation                                   : num 0.6
#>  $ //Bodenarten/Dach/thetaWP_MoistureAtWiltingPoint                                : num 0.05
#>  $ //Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv         : int 1
#>  $ //Massnahmenelemente/Dach/Berechnungsparameter/Infiltration_aktiv               : int 1
#>  $ //Massnahmenelemente/Dach/Allgemein/Endversickerungsrate                        : num 0
#>  $ //Massnahmenelemente/Dach/Bodenschichtung/Schichtdicken                         : num 80
#>  $ //Massnahmenelemente/Dach/Bodenschichtung/Startwerte_theta_ActualSoilMoisture   : num 0.1
#>  $ //Massnahmenelemente/Dach/Parameter_Evapotranspiration/KC_CropFactor            : num 0.94
#>  $ //Massnahmenelemente/Dach/Parameter_Evapotranspiration/LAI_LeafAreaIndex        : num 3.9
#>  $ //Massnahmenelemente/Dach/Parameter_Evapotranspiration/SL_LeafStorageCoefficient: num 0.29
roof_type_values("conventional", hoernschemeyer_aktiv = NULL)
#> list()
```
