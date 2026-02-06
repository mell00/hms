suppressPackageStartupMessages({
  library(ggplot2)
})

# Helper to read a dataset based on extension
read_hms_file <- function(path) {
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read .xlsx files.")
    }
    df <- readxl::read_excel(path)
    df <- as.data.frame(df)
  } else {
    df <- read.csv(path, stringsAsFactors = FALSE)
  }
  df
}

# Combine base + subcategory flags into a single binary vector
combine_dx <- function(df, base, sub_prefix) {
  base_vec <- NULL
  if (base %in% names(df)) {
    base_vec <- as.numeric(df[[base]] == 1)
  }

  sub_cols <- grep(paste0("^", sub_prefix, "\\d+"), names(df), value = TRUE)
  if (length(sub_cols) > 0) {
    sub_mat <- df[sub_cols]
    # Coerce to numeric where possible
    sub_mat[] <- lapply(sub_mat, function(x) as.numeric(x == 1))
    sub_any <- as.numeric(rowSums(sub_mat, na.rm = TRUE) > 0)
    if (is.null(base_vec)) {
      base_vec <- sub_any
    } else {
      base_vec <- as.numeric((base_vec == 1) | (sub_any == 1))
    }
  }

  if (is.null(base_vec)) {
    base_vec <- rep(NA_real_, nrow(df))
  }
  base_vec
}

# File list with labels in chronological order
files <- data.frame(
  label = c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020",
            "2020-2021", "2021-2022", "2022-2023", "2023-2024"),
  path = c(
    "2007-2024 datasets (.csv)/HMS_2015-2016_PUBLIC.xlsx",
    "2007-2024 datasets (.csv)/HMS_2016-2017_PUBLIC.csv",
    "2007-2024 datasets (.csv)/HMS_2017-2018_PUBLIC_instchars.csv",
    "2007-2024 datasets (.csv)/HMS_2018-2019_PUBLIC_instchars.csv",
    "2007-2024 datasets (.csv)/HMS_2019-2020_PUBLIC_instchars.csv",
    "2007-2024 datasets (.csv)/HMS_2020-2021_PUBLIC_instchars.csv",
    "2007-2024 datasets (.csv)/HMS_2021-2022_PUBLIC_instchars.csv",
    "2007-2024 datasets (.csv)/HMS_2022-2023_PUBLIC_instchars.csv",
    "2007-2024 datasets (.csv)/HMS_2023-2024_PUBLIC_instchars.csv"
  ),
  stringsAsFactors = FALSE
)

results <- lapply(seq_len(nrow(files)), function(i) {
  label <- files$label[i]
  path <- files$path[i]
  message("Reading ", label, " ...")
  df <- read_hms_file(path)

  dx_dep  <- combine_dx(df, "dx_dep",  "dx_dep_")
  dx_anx  <- combine_dx(df, "dx_anx",  "dx_ax_")
  dx_attl <- combine_dx(df, "dx_attl", "dx_att_")
  dx_ea   <- combine_dx(df, "dx_ea",   "dx_ea_")
  dx_psy  <- combine_dx(df, "dx_psy",  "dx_psy_")
  dx_pers <- combine_dx(df, "dx_pers", "dx_perso_")
  dx_sa   <- combine_dx(df, "dx_sa",   "dx_sa_")

  dx_full <- as.numeric(
    (dx_dep == 1) | (dx_anx == 1) | (dx_attl == 1) | (dx_ea == 1) |
      (dx_psy == 1) | (dx_pers == 1) | (dx_sa == 1)
  )

  count_dx_full <- sum(dx_full == 1, na.rm = TRUE)
  total_n <- nrow(df)
  data.frame(label = label, count_dx_full = count_dx_full, total_n = total_n,
             stringsAsFactors = FALSE)
})

summary_df <- do.call(rbind, results)
summary_df$start_year <- as.integer(sub("^(\\d{4}).*$", "\\1", summary_df$label))
summary_df <- summary_df[order(summary_df$start_year), ]
summary_df$label <- factor(summary_df$label, levels = summary_df$label)

# Make output dir
out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Plot
p <- ggplot(summary_df, aes(x = label, y = count_dx_full, group = 1)) +
  geom_line(color = "#2c7fb8", linewidth = 1) +
  geom_point(color = "#2c7fb8", size = 2.5) +
  labs(
    title = "Counts of dx_full (2015-2016 to 2023-2024)",
    x = "Survey Year Range",
    y = "Count of dx_full"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "dx_full_timeseries_2015_2024.png"), p, width = 9, height = 5, dpi = 150)

# Save summary data
write.csv(summary_df, file.path(out_dir, "dx_full_counts_2015_2024.csv"), row.names = FALSE)

message("Done. Outputs in ", out_dir)
