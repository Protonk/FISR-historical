# Data Preparation

The scripts in this directory regenerate the data objects that ship with the
`visualfrsr` package. Raw CSV exports belong under `inst/extdata/`, while
compressed `.rda` objects are written to `data/`.

## Pipelines

Each pipeline lives under `data-raw/pipelines/` and exposes a single entry
function that returns tibbles ready to be serialized. Run the commands from the
project root so the helper scripts in `R/` are available. By default the
pipelines persist their outputs to `data/` with XZ compression; set the
`save_to_data` argument to `FALSE` if you only need the in-memory results.

- **Deconstruction** (`deconstruction.R`):
  ```sh
  Rscript -e "devtools::load_all(); source('data-raw/pipelines/deconstruction.R'); \
    run_deconstruction_pipeline()"
  ```
  This regenerates the Newton-Raphson exploration datasets and refreshes:
  `data/deconstructed.rda`, `data/widened.rda`, and `data/narrowed.rda`.

- **Cluster bands and samples** (`clusters.R`):
  ```sh
  Rscript -e "devtools::load_all(); source('data-raw/pipelines/clusters.R'); \
    run_cluster_pipeline()"
  ```
  Running the pipeline rewrites the clustering artifacts stored in
  `data/cluster_bands.rda` and `data/representative_clusters.rda`.

- **Binned sweeps** (`binned.R`):
  ```sh
  Rscript -e "devtools::load_all(); source('data-raw/pipelines/binned.R'); \
    run_binned_pipeline()"
  ```
  The binned pipeline reproduces the optimisation sweeps and out-of-bin error
  diagnostics, saving `data/bin_samples.rda`, `data/bucket_summary.rda`, and
  `data/oob_performance.rda`.

## Post-processing

Use `prepare_binned_data.R` to convert the CSV artifacts in `inst/extdata/`
into the compressed `.rda` files stored in `data/`. The script rewrites
`data/equal05_20bins.rda`, `data/equal_bins.rda`, `data/manybins.rda`,
`data/too_many_bins.rda`, and `data/varied_bins.rda`.

```sh
Rscript data-raw/prepare_binned_data.R
```
