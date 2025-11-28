# PMScanR 1.0.1

**Date:** 2025-11-27

This release focuses on improving robustness on Windows systems, enhancing flexibility for external tools, and standardizing data structures.

### IMPROVEMENTS

* **Flexibility in `runPsScan()`:**
  * Added optional arguments (`database_path`, `ps_scan_path`, `pfscan_path`) to allow users to provide their own paths to PROSITE files. The function now prioritizes user-provided paths over the internal cache, facilitating usage in offline environments or with custom databases.

* **Vignette:**
  * Added a "Quick Start" section for immediate usage examples.
  * Clarified workflow descriptions to emphasize that visualization functions work interchangeably with data imported from GFF, PSA, or TXT formats.
  * Simplified data loading examples.

### BUG FIXES

* **Windows Path Handling:**
  * Fixed a critical issue in `runPsScan()` on Windows where backslashes in file paths caused the external Perl script to fail. The function now forces forward slashes (`/`) for all system calls.

* **Data Consistency in `readPsa()`:**
  * Fixed column data types in `readPsa()` (specifically ensuring character types for empty fields instead of logical `NA`).
  * Aligned the column order of `readPsa()` output to match the standard `GRanges`/`data.frame` structure returned by `rtracklayer::import.gff`. This ensures seamless compatibility between different input formats.

# PMScanR 0.99.4

**Date:** 2025-07-17

This version includes a major refactoring to align with Bioconductor submission standards, addressing all points from the initial review.

### SIGNIFICANT CHANGES

* The `runPsScan()` function has been completely refactored. It no longer downloads files to the working directory. Instead, it now uses `BiocFileCache` to download and cache the `ps_scan.pl` script, `prosite.dat`, and `pfscan` executables. This makes the function robust, reproducible, and compliant with Bioconductor guidelines for handling external data.

* All system calls have been updated to use `system2()` instead of `system()` or `shell()`, providing a safer and more portable way to run the external Perl script.

* The interactive OS confirmation prompt (`readline()`) has been removed from `runPsScan()` to make the function fully non-interactive and suitable for automated scripts.

### IMPROVEMENTS

* **Code Style:**

  * All function names have been standardized to use `camelCase` for consistency (e.g., `read.prosite` is now `readProsite`, `extract_protein_motifs` is now `extractProteinMotifs`, etc.).

  * Removed all `print()` and `cat()` statements from package functions. Status updates now correctly use `message()`.

  * Replaced `paste()` in `stop()` and `warning()` calls with `sprintf()`.

  * Removed all uses of `suppressWarnings()`, ensuring that important data coercion warnings are visible to the user.

* **Documentation:**

  * Added a package-level manual page, accessible via `?PMScanR`.

  * Added examples to all exported functions, including an `interactive()` example for `runPMScanRShiny()`.

* **Vignette:**

  * The vignette has been updated to include the standard Bioconductor installation instructions.

  * Removed `setwd()` and unnecessary `if (file.exists(...))` checks to make the code cleaner and more focused on workflow.

* **DESCRIPTION file:**

  * Added `URL` and `BugReports` fields pointing to the GitHub repository.

  * Added `Perl` to the `SystemRequirements` field.

  * Added `BiocFileCache` to `Imports`.

* **NAMESPACE:**

  * Removed all broad `import()` statements. The package now exclusively uses specific `importFrom()` tags for all dependencies (`bslib`, `bsicons`, `reshape2`, etc.), preventing namespace conflicts.

* **Unit Tests:**

  * Added a suite of unit tests using `testthat` for core data processing functions (`gff2matrix`, `extractProteinMotifs`, `freqPie`), increasing package robustness.

### BUG FIXES

* Fixed a bug in `gff2matrix()` where it would error on empty data frame input. It now correctly returns an empty matrix.

* Fixed a bug in `extractProteinMotifs()` where the regular expression failed to correctly parse motif headers, causing it to return an empty list. The parsing logic is now more robust.
