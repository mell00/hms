dfs_ordinal <- list(HMS_2007_2013_Aggregate, HMS_2013_2014_PUBLIC, HMS_2014_2015_PUBLIC)

common_cols_ordinal <- Reduce(intersect, lapply(dfs_ordinal, names))
combined_ordinal <- do.call(rbind, lapply(dfs, `[`, common_cols_ordinal))

dfs_numerical <- list(HMS_2015_2016_PUBLIC, HMS_2016_2017_PUBLIC, HMS_2017_2018_PUBLIC,
                      HMS_2018_2019_PUBLIC, HMS_2019_2020_PUBLIC,
                      HMS_2020_2021_PUBLIC, HMS_2021_2022_PUBLIC, HMS_2022_2023_PUBLIC,
                      HMS_2023_2024_PUBLIC)
common_cols_numerical <- Reduce(intersect, lapply(dfs_numerical, names))
combined_numerical <- do.call(rbind, lapply(dfs_numerical, `[`, common_cols_numerical))