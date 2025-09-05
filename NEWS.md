# PMScanR 0.99.5
**Date:** 2025-09-05

Fixes a merge issue between branches without adding changes to the package.

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
