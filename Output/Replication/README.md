# Replication run record

The full paper-replication workflow completed successfully on 10 November 2026 using R 4.5.0 on Windows 11. The command exited with status 0:

```sh
R CMD BATCH --vanilla 00_Hypertension_Setup_ReplicatePaper.R Output/Replication/Replication_Run.log
```

Run this from the repository root, creating `Output/Replication/` first if needed. In Windows PowerShell, use `R.exe` instead of `R`. A rerun overwrites the log and generated outputs.

## Checks and results

- All 32 steps completed; the final validation checked syntax, 9,496 unique respondents, model samples and expected output files.
- All 204 recorded package versions matched `renv.lock`, and all 25 direct dependencies loaded.
- Elapsed time was 521.57 seconds (8.7 minutes), with the package environment prepared beforehand.
- Existing generated tables, figures and model files were moved aside before the run. No saved analysis outputs were reused from an earlier run.
- Compared with the outputs at Git commit `5b7cfc4588add492c707f27f40cc267d65c4efe1`, 22 of the 24 expected files were byte-identical. Table A2 differed only in line endings. The Figure A2 PDF had identical page-content streams and rendered identically at 100 dpi; its binary representation and creation date changed. All reported numerical results were unchanged.

## Files

| File | Contents |
| --- | --- |
| `Replication_Run.log` | R batch transcript, including console output, warnings, completion messages and total runtime; the local repository path is replaced with `<repository root>` and nothing else is edited |
| `Step_Timings.csv` | Status and elapsed seconds for each of the 32 steps |
| `Session_Info.txt` | R version, operating system, locale and loaded package versions |
| `Environment_Check.csv` | Installed and recorded versions for all 204 lockfile packages |
| `Input_Checksums.csv` | MD5 checksums of the 23 survey and auxiliary input files |
| `Code_Checksums.csv` | SHA-256 checksums of the analysis scripts, output manifest, package list and lockfile used for this run |

Tables written directly to files are supplied in `Output/Tables/`; figures are in `Output/Figures/`.

Checksums describe the files read on Windows. Text-file checksums may differ after Git converts line endings on another platform.

## Environment preparation and retained warnings

The project library was prepared before the recorded successful run. An initial source installation of `maps` failed on this machine. Matching installed versions of `maps`, `Ecfun`, `fields`, `ggalt` and `rworldmap`, together with their dependencies, were reused through `renv::hydrate()`. The master script then restored the remaining packages from the cache and checked every recorded version. This demonstrates a complete analysis rerun with the recorded environment, not an unattended installation on a machine without existing packages.

The log retains startup locale warnings, notices that some Windows binaries were built with R 4.5.3, package-registration messages and an existing data-recoding warning. The final session used the intended UTF-8 locale. The version checks, sample checks and output comparisons above passed. The recoding behaviour was not changed for this release.

The initial missing-package notice precedes restoration. The later `renv` out-of-sync notice concerns dependency inventory: four recorded packages (`commonmark`, `litedown`, `markdown`, `png`) were not detected as used, while the bootstrap package `renv` was used but not recorded in the lockfile. Its version, 1.2.4, is recorded in `Session_Info.txt`; the explicit check of all 204 lockfile versions passed.
