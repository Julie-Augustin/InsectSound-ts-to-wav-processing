# Convert InsectSound .ts files (timeseriesclassification.com) to raw WAV audio.
#
# Put this script in the same folder as InsectSound_TRAIN.ts and InsectSound_TEST.ts,
# then run it from R/RStudio with:  source("insectsound_to_wav.R")
# or from a terminal with:          Rscript insectsound_to_wav.R
#
# Output:
#   wav_raw/<label>/<label>_train_00001.wav   one WAV per recording
#   metadata.csv                              one row per WAV file
#   processing_log.txt                        settings, input checksums, software version
#
# The WAV files hold the values exactly as they are in the .ts files (to float32
# precision): no centering, no scaling, no resampling. They are 32-bit float WAVs,
# so values outside [-1, 1] are kept as they are. 

input_files <- c(train = "InsectSound_TRAIN.ts", test = "InsectSound_TEST.ts")
sample_rate <- 6000   # Hz, as stated in the dataset description
out_dir     <- "wav_raw"

# Minimal .ts reader for univariate, equal-length data with class labels
read_ts <- function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  start <- which(tolower(lines) == "@data")
  header <- lines[seq_len(start - 1)]
  lines <- lines[(start + 1):length(lines)]
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  labels <- trimws(sub(".*:", "", lines))
  values <- lapply(strsplit(sub(":[^:]*$", "", lines), ","), as.numeric)
  list(series = values, labels = labels, header = header)
}

# Write a mono 32-bit float WAV file, values unchanged
write_wav_float <- function(x, sr, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  n <- length(x)
  u32 <- function(v) writeBin(as.integer(v), con, size = 4, endian = "little")
  u16 <- function(v) writeBin(as.integer(v), con, size = 2, endian = "little")
  writeChar("RIFF", con, eos = NULL); u32(4 + 26 + 12 + 8 + 4 * n)
  writeChar("WAVE", con, eos = NULL)
  writeChar("fmt ", con, eos = NULL); u32(18)
  u16(3); u16(1); u32(sr); u32(sr * 4); u16(4); u16(32); u16(0)  # IEEE float, mono
  writeChar("fact", con, eos = NULL); u32(4); u32(n)
  writeChar("data", con, eos = NULL); u32(4 * n)
  writeBin(as.double(x), con, size = 4, endian = "little")
}

metadata <- list()
headers  <- list()

for (split in names(input_files)) {
  d <- read_ts(input_files[[split]])
  headers[[split]] <- d$header
  cat(input_files[[split]], ":", length(d$series), "instances\n")
  print(table(d$labels))
  
  counters <- list()
  files <- character(length(d$series))
  for (i in seq_along(d$series)) {
    label <- d$labels[i]
    counters[[label]] <- if (is.null(counters[[label]])) 1 else counters[[label]] + 1
    dir.create(file.path(out_dir, label), recursive = TRUE, showWarnings = FALSE)
    file_name <- sprintf("%s_%s_%05d.wav", label, split, counters[[label]])
    files[i] <- file.path(out_dir, label, file_name)
    write_wav_float(d$series[[i]], sample_rate, files[i])
  }
  
  n <- lengths(d$series)
  metadata[[split]] <- data.frame(
    file        = files,
    label       = d$labels,
    split       = split,
    source_file = input_files[[split]],
    source_row  = seq_along(d$series),   # position of the instance in the .ts file
    sample_rate = sample_rate,
    n_samples   = n,
    duration_s  = n / sample_rate,
    mean        = sapply(d$series, mean),
    sd          = sapply(d$series, sd),
    rms         = sapply(d$series, function(x) sqrt(mean(x^2))),
    min         = sapply(d$series, min),
    max         = sapply(d$series, max)
  )
}

metadata <- do.call(rbind, metadata)
write.csv(metadata, "metadata.csv", row.names = FALSE)

log <- c(
  "InsectSound .ts to WAV conversion",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  paste("Sample rate:", sample_rate, "Hz"),
  "Format: mono 32-bit IEEE float WAV",
  "Processing: none. Values are copied unchanged from the .ts files",
  "(no centering, scaling, resampling or filtering).",
  "",
  "Input files (MD5):",
  paste0("  ", input_files, "  ", unname(tools::md5sum(input_files))),
  "",
  "Files written per split and label:",
  paste0("  ", capture.output(print(table(metadata$split, metadata$label)))),
  paste("Total:", nrow(metadata), "files"),
  "",
  ".ts headers:",
  unlist(lapply(names(headers), function(s) c(paste0("  [", s, "]"), paste0("    ", headers[[s]])))),
  "",
  R.version.string
)
writeLines(log, "processing_log.txt")

cat("Done.\n")

#=============================================================================
# Rename the files according to the dataset structure
#=============================================================================
# A mapping table (new name <-> old name, SegmentID, Notes) is saved so no metadata is lost.

library(readxl)

# ---- Settings ---------------------------------------------------------------
xlsx_path  <- "C:/path/to/check.xlsx"   # the Excel file
source_dir <- "C:/path/to/all_wavs"     # folder where all the .wav files are now
dest_dir   <- "C:/path/to/renamed"      # where renamed files go (can be = source_dir)

prefix     <- "AAL"
start_num  <- 1          # first number -> AAL000001
digits     <- 6
dry_run    <- TRUE      # TRUE = only report; set FALSE to actually rename
copy_files <- TRUE       # TRUE = copy (originals kept); FALSE = rename/move
# -----------------------------------------------------------------------------

tab <- as.data.frame(read_excel(xlsx_path))
tab$Names <- trimws(tab$Names)

nums <- start_num + seq_len(nrow(tab)) - 1
stopifnot(max(nums) < 10^digits)
tab$NewID   <- sprintf("%s%0*d", prefix, digits, nums)
tab$NewName <- paste0(tab$NewID, ".", tools::file_ext(tab$Names))   # keeps .wav

src  <- file.path(source_dir, tab$Names)
dest <- file.path(dest_dir, tab$NewName)

# ---- Checks -----------------------------------------------------------------
stopifnot(!anyDuplicated(tab$Names), !anyDuplicated(tab$NewName))
missing <- !file.exists(src)
already <- file.exists(dest)
extra   <- setdiff(list.files(source_dir, pattern = "\\.wav$", ignore.case = TRUE), tab$Names)

cat("Rows in Excel:                ", nrow(tab), "\n")
cat("Files found:                  ", sum(!missing), "\n")
cat("Files missing:                ", sum(missing), "\n")
if (any(missing)) print(head(tab$Names[missing], 20))
cat("Files in folder not in Excel: ", length(extra), "\n")
cat("New names already existing:   ", sum(already), "(will be skipped)\n")
cat("Range:", tab$NewName[1], "->", tab$NewName[nrow(tab)], "\n")

todo <- !missing & !already

# ---- Do it ------------------------------------------------------------------
if (dry_run) {
  cat("\nDRY RUN - nothing changed. Examples:\n")
  print(head(data.frame(old = tab$Names, new = tab$NewName), 5))
  print(tail(data.frame(old = tab$Names, new = tab$NewName), 3))
  cat("Set dry_run <- FALSE to run for real.\n")
} else {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save the mapping FIRST, so you always have it
  map_file <- file.path(dest_dir, "rename_mapping.csv")
  write.csv(tab[, c("NewName", "NewID", "Names", "SegmentID", "Notes")],
            map_file, row.names = FALSE)
  cat("Mapping saved to:", map_file, "\n")
  
  ok <- if (copy_files) file.copy(src[todo], dest[todo]) else file.rename(src[todo], dest[todo])
  cat("Done:", sum(ok), "succeeded,", sum(!ok), "failed\n")
  if (any(!ok)) print(head(tab$Names[todo][!ok], 20))
  cat("All new files exist:", all(file.exists(dest)), "\n")
}
