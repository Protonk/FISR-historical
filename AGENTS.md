# AGENTS

This document orients autonomous collaborators around the `visualfrsr` R package,
its regeneration pipelines, and the points where action is usually required.
Treat it as the field manual for exploring, extending, or repairing the project.

---

## Mission Snapshot
- **Goal:** regenerate and visualize Fast Reciprocal Square Root (FRSR) behavior
  via reproducible R pipelines.
- **Runtime:** R ≥ 4.1 with `devtools`, `frsrr` (C++20), `tidyverse`, `Matrix`,
  `Rglpk`, and plotting libraries (`ggplot2`, `viridis`).
- **Entrypoint:** treat the repository root as an R package. Use
  `devtools::load_all()` to expose functions and datasets.
- **Environment:** `.Rprofile` pins custom `~/.R/Makevars` (`tools/Makevars.apple-arm64`)
  and `.Renviron.project` (OpenMP/OpenBLAS vars). Keep these files in sync if
  compiler flags change.

---

## Directory Map

| Path | Purpose | Notes |
|------|---------|-------|
| `R/` | Package source. `binned.R`, `clustered.R`, `examples.R`, `optimized.R`, `utils.R`, `visualfrsr-package.R` plus `transform-*` and `plots-*` modules. | `R/plots/utils.R` hosts internal geom helpers; `R/globals.R` suppresses NSE notes. |
| `data-raw/` | Repro pipelines & conversion scripts. | `pipelines/` contains `run_*_pipeline()` entrypoints; `prepare_binned_data.R` compresses CSV → `.rda`. |
| `data/` | Packaged `.rda` artifacts read by the transforms. | Ships lightweight placeholder `.rda` files so the package loads immediately; rerun pipelines to refresh them. |
| `inst/extdata/` | Raw CSV exports for binning/bucket experiments. | Read via `load_binned_csv()`/`load_bucket_sweep()`. |
| `man/` | Roxygen output for exported objects. | Keeps pkgdown in sync. |
| `plots/` | Rendered figures referenced in `README.md`. | Purely illustrative outputs. |
| `tests/testthat/` | QA focused on the binning pipeline helper layer. | Fixtures live in `helper-fixtures.R`. Coverage is currently sparse. |
| `tools/Makevars.apple-arm64` | Custom clang/OpenMP toolchain settings. | Activated by `.Rprofile`. |
| `.Renviron.project` | Thread caps + `DYLD_FALLBACK_LIBRARY_PATH`. | Prevents runaway OpenMP usage on macOS. |
| `R/ToTransform/` | Scratch scripts (currently untracked in git). | Treat as sandbox; do not rely on contents downstream. |

---

## Data → Transform → Plot Workflow

1. **Generate data** by running the appropriate `run_*_pipeline()` inside
   `data-raw/pipelines/`. These scripts call into `{frsrr}` samplers, perform
   heavy computations, and optionally persist refreshed `.rda` files under
   `data/`.
2. **Load/package data** through `R/data-*.R` documentation stubs. The `.rda`
   objects become available as package datasets (`deconstructed`, `bin_samples`,
   etc.) once pipelines serialize them.
3. **Transform** via `R/transform-*.R`. The exported `prep_*` helpers bridge
   raw tibbles to plot-ready structures. Keep data-massaging logic here.
4. **Plot** using the ggplot constructors under `R/plots-*.R` plus shared
   geometry utilities (`R/plots/utils.R`). Each module expects specific
   `prep_*` outputs to reduce duplication.

This separation lets agents refresh upstream data infrequently while iterating
rapidly on plotting layers. Remember that the committed `.rda` files are
illustrative; regenerate them locally before publishing quantitative results.

---

## Pipelines & Key Artifacts

### 1. Binned Sweeps (`data-raw/pipelines/binned.R`)
- **Entrypoint:** `run_binned_pipeline()` orchestrates `frsrr::frsr_bin()`
  sweeps across configurable bin counts (`bin_sizes`), calculates bucket
  coverage through `find_optimal_buckets()`/`bucket_selection()`, evaluates
  out-of-bin (OOB) error via `compute_oob_performance()`, and saves:
  - `data/bin_samples.rda`
  - `data/bucket_summary.rda`
  - `data/oob_performance.rda`
- **Supporting code:** `R/binned.R` (ILP helpers, CSV loaders, `norm_errorN`),
  `R/transform-binning.R` (multiple `prep_bucket_*` functions),
  `R/plots-binning.R` (13 ggplot constructors), and tests in
  `tests/testthat/test-binned.R`.
- **CSV hydration:** `inst/extdata/*.csv` plus
  `data-raw/prepare_binned_data.R` for the historical bin sets that ship as
  `data/equal*.rda`, `manybins`, `varied_bins`.
- **Invocation:**  
  ```sh
  Rscript -e "devtools::load_all(); source('data-raw/pipelines/binned.R'); run_binned_pipeline()"
  ```
  Reduce `bin_sizes`/`bucket_range` for quick smoke tests.

### 2. Deconstruction (`data-raw/pipelines/deconstruction.R`)
- **Entrypoint:** `run_deconstruction_pipeline()` samples via
  `frsrr::frsr_sample()` with multiple configurations (core, wide, narrow) and
  writes `data/deconstructed.rda`, `data/widened.rda`, `data/narrowed.rda`.
- **Transforms:** `R/transform-deconstruction.R` (`prep_deconstruction_*`
  family) enriches with relative errors, spans, zoom subsets.
- **Plots:** `R/plots-deconstruction.R` contains nine ggplot builders using
  palettes defined in `R/plots/utils.R`.
- **Historical comparisons:** `R/examples.R` + `R/transform-history.R` +
  `R/plots-history.R` reuse the same sampling machinery for classic FRSR
  variants and Newton-Raphson error narratives.
- **Invocation:**  
  ```sh
  Rscript -e "devtools::load_all(); source('data-raw/pipelines/deconstruction.R'); run_deconstruction_pipeline()"
  ```

### 3. Cluster Bands (`data-raw/pipelines/clusters.R`)
- **Entrypoint:** `run_cluster_pipeline()` clusters `frsrr` samples, builds
  representative bands, and emits `data/cluster_bands.rda` plus
  `data/representative_clusters.rda`.
- **Core sampler:** `R/clustered.R::sample_clusters()` is exported so agents
  can draw additional samples without re-running the whole pipeline.
- **Transforms & Plots:** `R/transform-clusters.R` for `prep_cluster_samples()`
  and `R/plots-clusters.R::plot_sampled_clusters()` for the visualization.
- **Invocation:**  
  ```sh
  Rscript -e "devtools::load_all(); source('data-raw/pipelines/clusters.R'); run_cluster_pipeline()"
  ```

### 4. Optimization Grid (`R/optimized.R`)
- **Purpose:** `compute_result_block()` performs a grid search over Newton
  parameters (`A`, `B`) for a fixed magic constant. Downstream pieces:
  - `R/transform-optimization.R::prep_optimized_results()`
  - `R/plots-optimization.R` (tile, polar path, scatter)
- **Note:** While not a `data-raw` pipeline, it still consumes `frsrr`
  samplers and can be long-running (`slices = 100000`). Gate heavy runs behind
  explicit function calls.

---

## Key Modules & Responsibilities

- `R/binned.R` — integer programming + CSV loaders backing binning utilities.
- `R/examples.R` — deterministic historical approximation samples.
- `R/clustered.R` — sample generator for cluster visuals.
- `R/optimized.R` — Newton parameter sweep.
- `R/utils.R` — sampling helpers (`logStratifiedSampler`, `create_slices`,
  `find_optimal_magic`) and palettes.
- `R/plots/utils.R` — shared ggplot annotations/palettes for the
  deconstruction suite.
- `R/globals.R` — registers NSE symbols for CRAN checks.
- `R/data-*.R` — document packaged datasets (`?bin_samples`, etc.). Keep in
  sync when adding/removing `.rda` files.
- `R/transform-*` & `R/plots-*` — maintain the data/visualization layering.
  When adding a new visualization, prefer creating a dedicated transform file
  over adding bespoke wrangling inside `R/plots-*`.

---

## Operational Playbooks

### Refresh a Pipeline & Plot
1. From repo root, run (example: binned)
   ```sh
   Rscript -e "devtools::load_all(); source('data-raw/pipelines/binned.R'); run_binned_pipeline()"
   ```
2. Verify `.rda` files updated under `data/`.
3. Back in an R session, `devtools::load_all()` and call the relevant transform
   (`prep_bucket_selection()`) + plot (`plot_bucket_widths()`).
4. Commit regenerated data only when necessary; note that large `.rda` files
   may bloat diffs.

### Add a New Plot
1. Confirm an upstream dataset already exists; otherwise add/refresh via a
   pipeline.
2. Create/extend a `prep_*` helper inside the relevant `R/transform-*.R`.
   Keep wrangling pure and testable.
3. Build the ggplot in `R/plots-*.R`, relying on the prep helper’s contract.
4. Document/export via roxygen and update `_pkgdown.yml` if the function should
   appear in reference docs.

### Introduce a New Dataset
1. Decide whether it should be generated dynamically (function in `R/`) or via
   a script in `data-raw/pipelines/`.
2. If pipeline-based, follow the pattern in existing scripts: declare helper
   functions inside the file, return both in-memory tibbles and (optionally)
   serialized `.rda` files, and gate the serialization via `save_to_data`.
3. Add documentation in `R/data-yourdataset.R` and roxygen comments describing
   columns/sources.
4. Reference the dataset in transforms or plots as needed.

---

## Testing & QA

- **Harness:** `tests/testthat/test-binned.R` ensures the ILP helpers behave
  on fixtures. Run via `devtools::test()` from the project root.
- **Coverage gaps:** Only the binning helpers are currently under test.
  Consider adding suites for:
  - Deconstruction transforms (`prep_deconstruction_errors()`).
  - Plot-specific contracts (e.g., ensuring factor levels, palettes).
  - CSV loader fallbacks (`load_binned_csv()` error handling).
- **Performance:** Long-running samplers (`run_*_pipeline`, `compute_result_block`)
  can take minutes. Avoid invoking them inside automated tests; instead,
  mock or use tiny fixtures.

---

## Build & Check Guidance

1. `Rscript -e "devtools::document()"` – regenerates `NAMESPACE`/Rd files; ensure every dataset in `data/` has a matching `R/data-*.R` doc stub (`equal_bins_data`, etc.) before running.
2. `Rscript -e "devtools::test()"` – validates binning helpers; regenerate placeholder `.rda` files first so datasets load.
3. `R CMD build .` then `R CMD check visualfrsr_0.0.0.9000.tar.gz` – expect:
   - **Repository warnings** because the sandbox blocks network access to CRAN/Bioconductor indexes.
   - **Installed size NOTE** due to ~126 MB under `inst/extdata/` plus ~4 MB in `data/`.
4. Keep `.Rprofile`/`.Renviron.project` committed so OpenMP threads stay capped (`OMP_NUM_THREADS=1`, etc.) and `devtools::load_all()` avoids shared-memory errors.
5. If `R CMD check` reports missing datasets, rerun the relevant pipeline (`run_*_pipeline(save_to_data = TRUE)`) or the CSV converter (`Rscript data-raw/prepare_binned_data.R`) so `.rda` files exist under `data/`.

---

## Observations & TODO Radar

1. **Data freshness:** The committed `.rda` files are placeholders; agents
   must rerun the pipelines described above before analyzing or plotting.
2. **Diagnostics folder:** `R/ToTransform/diagnostics.R` is untracked but
   present locally. Treat it as experimental—do not assume availability in CI.
3. **Pkgdown:** `_pkgdown.yml` covers only a subset of exported functions.
   Update it when new transforms/plots become public to keep site docs synced.
4. **Threading defaults:** `.Renviron.project` caps OpenMP/MKL threads at 1.
   If agents change sampler workloads, revisit these values to prevent
   artificial bottlenecks.
5. **frsrr dependency:** Building `{frsrr}` requires clang++ with C++20 and
   working OpenMP. Keep `tools/Makevars.apple-arm64` updated for target
   toolchains, and document non-Apple equivalents if cross-platform support is
   desired.

---

## Quick Reference Commands

```sh
# Inspect package description / dependencies
cat DESCRIPTION

# Load package + run tests inside R
devtools::load_all()
devtools::test()

# Regenerate specific pipeline dataset
Rscript -e "devtools::load_all(); source('data-raw/pipelines/deconstruction.R'); run_deconstruction_pipeline()"

# Convert CSV extdata to packaged .rda objects
Rscript data-raw/prepare_binned_data.R
```

---

## Contact Points for Future Agents

- **Pipelines:** `data-raw/pipelines/*.R` (edit when changing sampler logic).
- **Plot APIs:** `R/plots-*.R` (ensure exports + documentation).
- **Transforms:** `R/transform-*.R` (shared between pipelines and plots).
- **Data loaders:** `R/binned.R` (`load_bucket_sweep()`, etc.).
- **Configuration:** `.Rprofile`, `.Renviron.project`, `tools/Makevars.apple-arm64`.

When taking on new work, explicitly state which layer you are modifying
(pipeline vs. datasets vs. transforms vs. plots) so downstream agents can
quickly pick up where you left off.
