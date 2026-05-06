# Package index

## All functions

- [`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md)
  : Add overflow-event metrics and water-balance shares (percent) to
  simulation results
- [`download_engine()`](https://kwb-r.github.io/kwb.raindrop/reference/download_engine.md)
  : Download the Tandler "Regenwasserbewirtschaftung" calculation engine
- [`find_single_param_variations()`](https://kwb-r.github.io/kwb.raindrop/reference/find_single_param_variations.md)
  : Find scenarios that differ from a reference in exactly one parameter
- [`get_simulation_results_all()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_all.md)
  : Read Raindrop optimisation simulation results (all) from HDF5
- [`get_simulation_results_optim()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim.md)
  : Read Raindrop optimisation simulation results from HDF5
- [`get_simulation_results_optim_parallel()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim_parallel.md)
  : Read Raindrop optimisation simulation results from HDF5 (parallel
  via future.apply + progress)
- [`h5_ensure_dataset()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_ensure_dataset.md)
  : Ensure that an HDF5 dataset exists (create if missing)
- [`h5_ensure_datasets_from_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_ensure_datasets_from_values.md)
  : Ensure that all datasets referenced by a values list exist
- [`h5_read_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_read_values.md)
  : Read values of all (or selected) datasets
- [`h5_validate_write()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_validate_write.md)
  : Validate what would be written where (pre-flight check)
- [`h5_write_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_write_values.md)
  : Write (updated) values back into existing HDF5 datasets (robust for
  your hdf5r build)
- [`list_h5_datasets()`](https://kwb-r.github.io/kwb.raindrop/reference/list_h5_datasets.md)
  : List all datasets (recursive)
- [`plot_hpond_vs_ref()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_hpond_vs_ref.md)
  : Plot the influence of single-parameter variations on a response
- [`plot_main_effects()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_main_effects.md)
  : Plot main effects of multiple parameters on an outcome
  (violin/box/jitter)
- [`plot_valid_design_space()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_valid_design_space.md)
  : Valid solutions in design space (x × y) with overflow-threshold
  discrete color scale
- [`plot_wb_tradeoff_overflows()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_wb_tradeoff_overflows.md)
  : Trade-off plot: Infiltration vs. Evapotranspiration (discrete colors
  by overflow threshold)
- [`read_hdf5_connections()`](https://kwb-r.github.io/kwb.raindrop/reference/read_hdf5_connections.md)
  : Read surface-water connections from an HDF5 results file
- [`read_hdf5_scalars()`](https://kwb-r.github.io/kwb.raindrop/reference/read_hdf5_scalars.md)
  : Read scalar datasets from an HDF5 group
- [`read_hdf5_timeseries()`](https://kwb-r.github.io/kwb.raindrop/reference/read_hdf5_timeseries.md)
  : Read HDF5 time series datasets from a group (supports deeperLayers)
- [`read_raindrop_errors()`](https://kwb-r.github.io/kwb.raindrop/reference/read_raindrop_errors.md)
  : Read RAINDROP error logs into a nested tibble
- [`run_model()`](https://kwb-r.github.io/kwb.raindrop/reference/run_model.md)
  : Run an rainwater management model executable with an input file
- [`run_scenarios()`](https://kwb-r.github.io/kwb.raindrop/reference/run_scenarios.md)
  : Run scenarios (parallel or sequential) with a user-supplied worker
  function
