# educationsurveyindia

R interface to India's NSS 80th round Comprehensive Modular Survey on Education
(CMS-E, 2025) unit-level microdata, via DuckDB-backed Parquet files.

## Install

```r
install.packages("educationsurveyindia")
# downloading data also needs mospiR:
install.packages("mospiR", repos = c("https://saketlab.r-universe.dev", getOption("repos")))
```

## Build the data (one-time)

Requires a MOSPI API key (`MOSPI_KEY` in `~/.Renviron`, see <https://microdata.gov.in>)
and [mospiR](https://github.com/saketlab/mospiR):

```r
library(educationsurveyindia)
options(esi.data_dir = "~/data/education")  # optional; default is tools::R_user_dir()
esi_build()   # downloads into <data dir>/raw, writes Parquet to <data dir>
```

Raw files already downloaded are reused; pass `overwrite = TRUE` to fetch them again.
See `vignette("building-the-data")` for the full walkthrough.

### Command line

The package ships an `esi` script. Put it on your `PATH`:

```sh
ln -s "$(Rscript -e 'cat(system.file("exec", "esi", package = "educationsurveyindia"))')" ~/.local/bin/esi
```

```sh
esi build                          # download (if needed) + build into <data dir>
esi build --output-dir /data/cmse  # elsewhere
esi download --overwrite           # re-download raw files only
esi info                           # tables and row counts
esi help
```

### Every education round

`esi_surveys()` lists all seven NSS education surveys on microdata.gov.in
(42nd round 1986-87 through 80th round 2025). `esi_download_all()` fetches
every file of each (microdata in all formats, codebooks, questionnaires,
reports) into `<data dir>/rounds/<round>`, skipping files already there:

```r
esi_download_all()                  # all rounds
esi_download_all(rounds = c("71", "75"))
```

Only the 80th round is built into Parquet; earlier rounds are raw downloads.

## Use

```r
con <- esi_connect()                   # or esi_connect("~/data/education")

esi_tables(con)
esi_available_data(con)

esi_school_participation(con)          # weighted enrolment rate by state
esi_expenditure_by_state(con)          # mean school/coaching spend by state
esi_state_household_summary(con)       # household-level education spending

esi_find(con, "coaching")              # search variable labels
esi_decode(con, "household", "sector", c("1", "2"))  # -> "rural", "urban"

esi_disconnect(con)
```

## License

MIT
