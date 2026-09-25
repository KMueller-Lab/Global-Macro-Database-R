#!/usr/bin/env Rscript
# =====================================================================
# reproduce_pr9_issues.R
# Reproduces the issues found in the review of PR #9
# ("Fix cross-language inconsistencies; add print/fast parity").
#
# Self-contained, OFFLINE and deterministic: it sources the package's
# R/ files and mocks only the version lookup, so no network and no live
# GMD server are required. Your real on-disk cache is backed up and
# restored, so this script does not destroy anything.
#
# HOW TO RUN (with PR branch `20260628/GMD_test_R` checked out):
#   From the repo root:   Rscript reproduce_pr9_issues.R
#   Or in an R session:   source("reproduce_pr9_issues.R")
#
# Requires the `haven` package (it is in the project's Suggests).
# =====================================================================

repo <- getwd()   # <-- set this to the repo root if you run from elsewhere

## ---- preflight -------------------------------------------------------
need <- function(f) if (!file.exists(file.path(repo, f)))
  stop(sprintf("Cannot find '%s' under repo = '%s'.\n  Set `repo` at the top of this script to the repo root.", f, repo),
       call. = FALSE)
need("R/gmd.R"); need("R/helpers.R"); need("DESCRIPTION"); need("man/gmd.Rd")

if (!requireNamespace("haven", quietly = TRUE))
  stop("This reproduction needs the 'haven' package: install.packages('haven')", call. = FALSE)

hr <- function(t) cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")
ok <- function(...) cat("  [REPRODUCED]    ", ..., "\n", sep = "")
no <- function(...) cat("  [not reproduced]", ..., "\n", sep = "")

# Load the package code under test into the global env. The internal
# helpers `.gmd_safe_get` / `.gmd_load_versions_df` and `gmd` end up in
# globalenv(), so we can later swap the version lookup for a stub.
suppressWarnings(source(file.path(repo, "R/helpers.R")))
suppressWarnings(source(file.path(repo, "R/gmd.R")))


## =====================================================================
## Issue 3 — non-ASCII string literals in R/gmd.R  (R CMD check WARNING)
## =====================================================================
hr("Issue 3: non-ASCII characters in R source (CRAN check flags these)")
cat("`tools::showNonASCIIfile()` is the exact check R CMD check runs.\n",
    "The PR introduced raw 'u-umlaut' bytes inside message() at lines 118 & 122,\n",
    "while the rest of the file escapes them as \\u00fc (e.g. line 128).\n\n", sep = "")
flagged <- tools::showNonASCIIfile(file.path(repo, "R/gmd.R"))
cat("\n")
src <- readLines(file.path(repo, "R/gmd.R"), warn = FALSE)
non_ascii_lines <- which(grepl("[^\x01-\x7f]", src, perl = TRUE, useBytes = TRUE))
pr_offenders <- intersect(c(118, 122), non_ascii_lines)
line128_clean <- !(128 %in% non_ascii_lines)
if (length(pr_offenders) > 0 && line128_clean)
  ok("lines ", paste(pr_offenders, collapse = " & "),
     " carry raw non-ASCII bytes; line 128 uses the escaped \\u00fc form (clean).")
if (!line128_clean) no("line 128 unexpectedly also flagged - file layout may have shifted.")


## =====================================================================
## Issue 4 — `tools` used via :: but not declared in DESCRIPTION Imports
## =====================================================================
hr("Issue 4: tools::R_user_dir used but 'tools' not in DESCRIPTION Imports")
dcf  <- read.dcf(file.path(repo, "DESCRIPTION"))
imp  <- if ("Imports" %in% colnames(dcf)) dcf[, "Imports"] else ""
deps <- if ("Depends" %in% colnames(dcf)) dcf[, "Depends"] else ""
imp_pkgs <- trimws(gsub("\\s*\\(.*?\\)", "", strsplit(imp, ",")[[1]]))
uses_tools <- any(grepl("tools::", src, fixed = TRUE))
cat("DESCRIPTION Imports :", paste(imp_pkgs, collapse = ", "), "\n")
cat("DESCRIPTION Depends :", if (nzchar(deps)) deps else "(none)", "\n")
cat("R/gmd.R uses tools:::", uses_tools, "\n\n")
if (uses_tools && !("tools" %in% imp_pkgs))
  ok("tools::R_user_dir() is called but 'tools' is absent from Imports -> R CMD check NOTE.")
if (!grepl("R \\(>=", deps))
  ok("no 'R (>= 4.0)' floor declared, yet R_user_dir() needs R >= 4.0 (it runs on EVERY gmd() call).")


## =====================================================================
## Issue 2 — man/gmd.Rd not regenerated: new args undocumented
## =====================================================================
hr("Issue 2: man/gmd.Rd does not document the new args (R CMD check WARNING)")
sig <- names(formals(gmd))
rd  <- readLines(file.path(repo, "man/gmd.Rd"), warn = FALSE)
undocumented <- sig[!vapply(sig,
  function(a) any(grepl(paste0("\\b", a, "\\b"), rd)), logical(1))]
cat("gmd() formals     :", paste(sig, collapse = ", "), "\n")
cat("Missing from Rd   :", paste(undocumented, collapse = ", "), "\n\n")
if (length(undocumented) > 0)
  ok("man/gmd.Rd is stale; undocumented argument(s): ", paste(undocumented, collapse = ", "),
     " -> 'Undocumented arguments' WARNING.")


## =====================================================================
## Issues 1 & 5 — the cache READ is not gated on `fast`
##   #1: a planted cache is served even when fast = FALSE (stale data)
##   #5: a corrupt cache then breaks EVERY call, with no recovery path
## These run fully offline: we stub the version lookup and pre-seed the
## cache, so gmd() never touches the network.
## =====================================================================
hr("Issues 1 & 5: cache read ignores `fast` (headline bug)")

orig_versions <- .gmd_load_versions_df
# Stub: pretend the current version is 2026_03, no network.
assign(".gmd_load_versions_df",
       function() data.frame(versions = c("2026_03", "2026_02"),
                             stringsAsFactors = FALSE),
       envir = globalenv())

cache_dir  <- tools::R_user_dir("globalmacrodata", "cache")
cache_file <- file.path(cache_dir, "GMD_2026_03.dta")

backup <- NULL
restore <- function() {
  if (file.exists(cache_file)) file.remove(cache_file)
  if (!is.null(backup) && file.exists(backup)) file.rename(backup, cache_file)
  assign(".gmd_load_versions_df", orig_versions, envir = globalenv())
}

tryCatch({
  if (file.exists(cache_file)) {                 # protect a real cache
    backup <- paste0(cache_file, ".pr9bak")
    file.rename(cache_file, backup)
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  ## ---- Issue 1: plant a SENTINEL dataset as the cache ----
  sentinel <- data.frame(
    ISO3        = "ZZZ",
    year        = 1900L,
    countryname = "SENTINEL_FROM_CACHE",
    id          = 1L,
    rGDP        = 42,
    stringsAsFactors = FALSE
  )
  haven::write_dta(sentinel, cache_file)
  cat("Planted a fake cache at:\n  ", cache_file, "\n",
      "(ISO3 = 'ZZZ', rGDP = 42). A correct fresh download would never contain this.\n\n", sep = "")

  res1 <- suppressWarnings(suppressMessages(gmd()))            # DEFAULT: fast = FALSE
  res2 <- suppressWarnings(suppressMessages(gmd(fast = FALSE)))# explicit opt-out attempt

  hit1 <- "ZZZ" %in% res1$ISO3
  hit2 <- "ZZZ" %in% res2$ISO3
  cat("gmd()            -> ISO3 contains 'ZZZ'? ", hit1, "\n")
  cat("gmd(fast=FALSE)  -> ISO3 contains 'ZZZ'? ", hit2, "\n\n")
  if (hit1) ok("gmd() with the DEFAULT fast=FALSE served the planted cache instead of downloading.")
  if (hit2) ok("gmd(fast=FALSE) ALSO served the cache - there is no way to force a fresh download.")
  if (!hit1 && !hit2) no("default calls did not read the planted cache (bug not present).")

  ## ---- Issue 5: a corrupt cache then poisons every call ----
  cat("\n-- corrupt-cache consequence (Issue 5) --\n")
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03, 0x04)), cache_file)  # truncated/garbage .dta
  err <- tryCatch({
    suppressWarnings(suppressMessages(gmd())); NA_character_
  }, error = function(e) conditionMessage(e))
  if (!is.na(err))
    ok("a truncated cache makes plain gmd() error every time (no temp-write, no recovery):\n",
       "                   ", strtrim(gsub("\n", " ", err), 100))
  else
    no("corrupt cache did not surface an error.")

}, finally = restore())


## =====================================================================
hr("Summary")
cat(
"  Issue 1 (HIGH)  cache read not gated on `fast` -> stale data, no refresh path.\n",
"  Issue 2 (HIGH)  man/gmd.Rd stale -> R CMD check WARNING (undocumented args).\n",
"  Issue 3 (MED)   raw non-ASCII literals in R/gmd.R lines 118 & 122.\n",
"  Issue 4 (MED)   `tools` not in Imports / no 'R (>= 4.0)' floor.\n",
"  Issue 5 (LOW)   non-atomic cache write -> a corrupt cache breaks every call.\n",
"\n  (This script created only ", "reproduce_pr9_issues.R", " and a temporary cache file,\n",
"   which has been cleaned up / your original cache restored.)\n", sep = "")
