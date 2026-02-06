suppressPackageStartupMessages({
  library(ggplot2)
})

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

# codebook labels for personality disorders
perso_labels <- c(
  dx_perso_1  = "Antisocial personality disorder",
  dx_perso_2  = "Avoidant personality disorder",
  dx_perso_3  = "Borderline personality disorder",
  dx_perso_4  = "Dependent personality disorder",
  dx_perso_5  = "Histrionic personality disorder",
  dx_perso_6  = "Narcissistic personality disorder",
  dx_perso_7  = "Obsessive-Compulsive personality disorder",
  dx_perso_8  = "Paranoid personality disorder",
  dx_perso_9  = "Schizoid personality disorder",
  dx_perso_10 = "Schizotypal personality disorder",
  dx_perso_11 = "Other (please specify)"
)

results <- list()

for (i in seq_len(nrow(files))) {
  label <- files$label[i]
  path <- files$path[i]
  message("Reading ", label, " ...")
  df <- read_hms_file(path)

  for (col in names(perso_labels)) {
    if (col %in% names(df)) {
      count <- sum(as.numeric(df[[col]] == 1), na.rm = TRUE)
    } else {
      count <- NA_real_
    }
    results[[length(results) + 1]] <- data.frame(
      label = label,
      code = col,
      disorder = unname(perso_labels[col]),
      count = count,
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- do.call(rbind, results)
summary_df$start_year <- as.integer(sub("^(\\d{4}).*$", "\\1", summary_df$label))
summary_df <- summary_df[order(summary_df$start_year), ]
summary_df$label <- factor(summary_df$label, levels = unique(summary_df$label))

out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

# plot as faceted time series for readability
p <- ggplot(summary_df, aes(x = label, y = count, group = 1)) +
  geom_line(color = "#1b9e77", linewidth = 0.9) +
  geom_point(color = "#1b9e77", size = 2) +
  facet_wrap(~ disorder, scales = "free_y") +
  labs(
    title = "Personality Disorder Diagnosis Counts (2015-2016 to 2023-2024)",
    x = "Survey Year Range",
    y = "Count (dx_perso_*)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

out_plot <- file.path(out_dir, "dx_perso_timeseries_2015_2024.png")

ggsave(out_plot, p, width = 12, height = 8, dpi = 150)

# plot as single multi-colored line chart
p2 <- ggplot(summary_df, aes(x = label, y = count, color = disorder, group = disorder)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Personality Disorder Diagnosis Counts (2015-2016 to 2023-2024)",
    x = "Survey Year Range",
    y = "Count (dx_perso_*)",
    color = "Disorder"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

out_plot2 <- file.path(out_dir, "dx_perso_timeseries_2015_2024_multiline.png")
ggsave(out_plot2, p2, width = 12, height = 7, dpi = 150)

write.csv(summary_df, file.path(out_dir, "dx_perso_counts_2015_2024.csv"), row.names = FALSE)

message("Done. Outputs in ", out_dir)
