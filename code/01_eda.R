# Secondary Mushroom Dataset - Data Loading & Cleaning
options(stringsAsFactors = FALSE)

# -------------------------------------------------------------------------
# 1. Confirm that R is running from the project root
# -------------------------------------------------------------------------

if (!dir.exists("data")) {
  stop(
    paste(
      "The data/ directory was not found.",
      "Open the project as an RStudio Project, or set the working directory",
      "to the project root before running this script."
    )
  )
}

# -------------------------------------------------------------------------
# 2. Locate the Secondary Mushroom data by its header
# -------------------------------------------------------------------------

expected_original_names <- c(
  "class",
  "cap-diameter",
  "cap-shape",
  "cap-surface",
  "cap-color",
  "does-bruise-or-bleed",
  "gill-attachment",
  "gill-spacing",
  "gill-color",
  "stem-height",
  "stem-width",
  "stem-root",
  "stem-surface",
  "stem-color",
  "veil-type",
  "veil-color",
  "has-ring",
  "ring-type",
  "spore-print-color",
  "habitat",
  "season"
)

candidate_files <- list.files(path = "data", full.names = TRUE, all.files = FALSE)

has_secondary_header <- function(path) {
  first_line <- tryCatch(
    readLines(path, n = 1, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
  if (length(first_line) == 0) return(FALSE)
  observed_names <- strsplit(first_line, ";", fixed = TRUE)[[1]]
  identical(observed_names, expected_original_names)
}

matching_files <- candidate_files[vapply(candidate_files, has_secondary_header, logical(1))]

if (length(matching_files) == 0) {
  stop(
    paste(
      "No Secondary Mushroom data file was found in data/.",
      "The correct file must begin with the 21-column header",
      "'class;cap-diameter;cap-shape;...;season'."
    )
  )
}

if (length(matching_files) > 1) {
  stop(
    paste(
      "More than one file in data/ has the expected Secondary Mushroom header:",
      paste(matching_files, collapse = ", "),
      "Keep one authoritative copy before continuing."
    )
  )
}

data_path <- matching_files[[1]]

# -------------------------------------------------------------------------
# 3. Read the semicolon-delimited file
# -------------------------------------------------------------------------

mushrooms_raw <- read.delim(
  file = data_path,
  header = TRUE,
  sep = ";",
  dec = ".",
  na.strings = c("", "NA"),
  strip.white = TRUE,
  check.names = FALSE,
  comment.char = "",
  quote = "\""
)

# -------------------------------------------------------------------------
# 4. Validate the structure before cleaning
# -------------------------------------------------------------------------

if (!identical(names(mushrooms_raw), expected_original_names)) {
  stop("The imported column names do not match the official metadata.")
}

if (ncol(mushrooms_raw) != 21) {
  stop(paste0("Expected 21 columns (1 target and 20 predictors), but found ", ncol(mushrooms_raw), "."))
}

if (nrow(mushrooms_raw) < 60000) {
  stop(paste0("The file contains only ", nrow(mushrooms_raw), " rows. Expected about 61,069 rows."))
}

if (!all(na.omit(unique(mushrooms_raw$class)) %in% c("e", "p"))) {
  stop("The class column contains values other than 'e' and 'p'.")
}

# -------------------------------------------------------------------------
# 5. Create analysis-friendly names and data types in memory
# -------------------------------------------------------------------------

mushrooms <- mushrooms_raw
names(mushrooms) <- gsub("-", "_", names(mushrooms), fixed = TRUE)

numeric_variables <- c("cap_diameter", "stem_height", "stem_width")

for (variable in numeric_variables) {
  mushrooms[[variable]] <- as.numeric(mushrooms[[variable]])
}

mushrooms$class <- factor(
  mushrooms$class,
  levels = c("e", "p"),
  labels = c("edible", "poisonous")
)

categorical_variables <- setdiff(names(mushrooms), c("class", numeric_variables))

mushrooms[categorical_variables] <- lapply(mushrooms[categorical_variables], factor)

# -------------------------------------------------------------------------
# 6. Optional quality-control check
# -------------------------------------------------------------------------

run_qc_check <- function(mushrooms, mushrooms_raw) {
  missing_summary <- data.frame(
    variable = names(mushrooms),
    missing_count = vapply(mushrooms, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  missing_summary$missing_percent <- round(100 * missing_summary$missing_count / nrow(mushrooms), 2)

  list(
    dimensions = c(rows = nrow(mushrooms), columns = ncol(mushrooms)),
    class_counts = table(mushrooms$class, useNA = "ifany"),
    class_proportions = round(prop.table(table(mushrooms$class)), 4),
    missing_summary = missing_summary[missing_summary$missing_count > 0, ],
    duplicate_rows = sum(duplicated(mushrooms)),
    numeric_summary = summary(mushrooms[numeric_variables]),
    levels_per_categorical = sort(vapply(mushrooms[categorical_variables], nlevels, integer(1)), decreasing = TRUE)
  )
}

if (!exists("RUN_QC")) {
  RUN_QC <- TRUE
}
if (RUN_QC) {
  print(run_qc_check(mushrooms, mushrooms_raw))
}

# -------------------------------------------------------------------------
# 7. Apply the agreed cleaning decisions
# -------------------------------------------------------------------------

mushrooms_clean <- mushrooms

# Preserve categorical missingness as an explicit level instead of deleting
# incomplete records or replacing missing values with the mode.
mushrooms_clean[categorical_variables] <- lapply(
  mushrooms_clean[categorical_variables],
  function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "unknown"
    factor(x)
  }
)

if (anyNA(mushrooms_clean[numeric_variables])) {
  stop(
    paste(
      "At least one numerical variable contains a missing value.",
      "Inspect it before proceeding; numerical imputation is not part of",
      "the current cleaning decision."
    )
  )
}

# Check whether identical predictor combinations ever have different labels.
# Such conflicts should be reported separately from exact duplicate rows.
predictor_variables <- setdiff(names(mushrooms_clean), "class")

predictor_key <- do.call(
  paste,
  c(
    lapply(
      mushrooms_clean[predictor_variables],
      function(x) as.character(x)
    ),
    sep = "\u001f"
  )
)

classes_per_predictor_pattern <- tapply(
  as.character(mushrooms_clean$class),
  predictor_key,
  function(x) length(unique(x))
)

conflicting_predictor_patterns <- sum(
  classes_per_predictor_pattern > 1
)

# Remove only rows that are exact duplicates across both predictors and class.
duplicate_rows <- duplicated(mushrooms_clean)
exact_duplicates_removed <- sum(duplicate_rows)
mushrooms_clean <- mushrooms_clean[!duplicate_rows, , drop = FALSE]
row.names(mushrooms_clean) <- NULL

cleaning_summary <- data.frame(
  item = c(
    "Rows before cleaning",
    "Exact duplicate rows removed",
    "Rows after cleaning",
    "Remaining missing values",
    "Conflicting predictor patterns"
  ),
  value = c(
    nrow(mushrooms),
    exact_duplicates_removed,
    nrow(mushrooms_clean),
    sum(is.na(mushrooms_clean)),
    conflicting_predictor_patterns
  ),
  row.names = NULL
)

cat("\n--- CLEANING SUMMARY ---\n")
print(cleaning_summary, row.names = FALSE)

if (conflicting_predictor_patterns > 0) {
  warning(
    paste(
      conflicting_predictor_patterns,
      "predictor patterns occur with more than one class label.",
      "Keep this fact in the report and do not remove them automatically."
    ),
    call. = FALSE
  )
}

# -------------------------------------------------------------------------
# 8. Prepare EDA tables
# -------------------------------------------------------------------------

class_summary <- as.data.frame(table(mushrooms_clean$class))
names(class_summary) <- c("class", "count")
class_summary$proportion <- class_summary$count / sum(class_summary$count)

missing_summary_plot <- data.frame(
  variable = names(mushrooms),
  missing_count = vapply(
    mushrooms,
    function(x) sum(is.na(x)),
    integer(1)
  ),
  row.names = NULL
)
missing_summary_plot$missing_percent <- (
  100 * missing_summary_plot$missing_count / nrow(mushrooms)
)
missing_summary_plot <- missing_summary_plot[
  missing_summary_plot$missing_count > 0,
  ,
  drop = FALSE
]
missing_summary_plot <- missing_summary_plot[
  order(missing_summary_plot$missing_percent),
  ,
  drop = FALSE
]
missing_summary_plot$variable <- factor(
  missing_summary_plot$variable,
  levels = missing_summary_plot$variable
)

numeric_long <- do.call(
  rbind,
  lapply(
    numeric_variables,
    function(variable) {
      data.frame(
        class = mushrooms_clean$class,
        variable = variable,
        value = mushrooms_clean[[variable]],
        row.names = NULL
      )
    }
  )
)
numeric_long$variable <- factor(
  numeric_long$variable,
  levels = numeric_variables
)

selected_categorical_variables <- c(
  "cap_shape",
  "gill_attachment",
  "habitat"
)

categorical_long <- do.call(
  rbind,
  lapply(
    selected_categorical_variables,
    function(variable) {
      data.frame(
        class = mushrooms_clean$class,
        feature = variable,
        level = as.character(mushrooms_clean[[variable]]),
        row.names = NULL
      )
    }
  )
)
categorical_long$feature <- factor(
  categorical_long$feature,
  levels = selected_categorical_variables
)

# -------------------------------------------------------------------------
# 9. Create the required EDA visualizations
# -------------------------------------------------------------------------

required_plot_packages <- c("ggplot2", "scales")
missing_plot_packages <- required_plot_packages[
  !vapply(required_plot_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_plot_packages) > 0) {
  stop(
    paste0(
      "EDA tables and cleaning completed, but plotting requires: ",
      paste(missing_plot_packages, collapse = ", "),
      ". Install them with install.packages(c(",
      paste(sprintf('"%s"', missing_plot_packages), collapse = ", "),
      ")) and source this script again."
    )
  )
}

class_distribution_plot <- ggplot2::ggplot(
  class_summary,
  ggplot2::aes(x = class, y = count, fill = class)
) +
  ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(
        scales::comma(count),
        "\n",
        scales::percent(proportion, accuracy = 0.1)
      )
    ),
    vjust = -0.25
  ) +
  ggplot2::scale_fill_manual(
    values = c("edible" = "#2E8B57", "poisonous" = "#B22222")
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma,
    expand = ggplot2::expansion(mult = c(0, 0.22))
  ) +
  ggplot2::labs(
    title = "Distribution of Mushroom Classes",
    subtitle = "The target is reasonably balanced for model comparison",
    x = "Class",
    y = "Number of records"
  ) +
  ggplot2::theme_minimal(base_size = 12)

missing_values_plot <- ggplot2::ggplot(
  missing_summary_plot,
  ggplot2::aes(x = missing_percent, y = variable)
) +
  ggplot2::geom_col(fill = "#3B6EA8", width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(round(missing_percent, 1), "%")
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 105),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Missing Values in Categorical Predictors",
    subtitle = "Missing categories will be retained as an explicit unknown level",
    x = "Missing records",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

numeric_distribution_plot <- ggplot2::ggplot(
  numeric_long,
  ggplot2::aes(x = value, fill = class)
) +
  ggplot2::geom_histogram(
    bins = 40,
    alpha = 0.55,
    position = "identity"
  ) +
  ggplot2::facet_wrap(
    ~ variable,
    scales = "free",
    ncol = 1
  ) +
  ggplot2::scale_fill_manual(
    values = c("edible" = "#2E8B57", "poisonous" = "#B22222")
  ) +
  ggplot2::labs(
    title = "Numerical Predictor Distributions by Class",
    x = "Observed value",
    y = "Number of records",
    fill = "Class"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top")

numeric_boxplot <- ggplot2::ggplot(
  numeric_long,
  ggplot2::aes(x = class, y = value, fill = class)
) +
  ggplot2::geom_boxplot(
    outlier.alpha = 0.15,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ variable,
    scales = "free_y",
    nrow = 1
  ) +
  ggplot2::scale_fill_manual(
    values = c("edible" = "#2E8B57", "poisonous" = "#B22222")
  ) +
  ggplot2::labs(
    title = "Numerical Predictors by Mushroom Class",
    x = "Class",
    y = "Observed value"
  ) +
  ggplot2::theme_minimal(base_size = 12)

categorical_class_plot <- ggplot2::ggplot(
  categorical_long,
  ggplot2::aes(x = level, fill = class)
) +
  ggplot2::geom_bar(position = "fill") +
  ggplot2::facet_wrap(
    ~ feature,
    scales = "free_x",
    ncol = 1
  ) +
  ggplot2::scale_fill_manual(
    values = c("edible" = "#2E8B57", "poisonous" = "#B22222")
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(
    title = "Class Proportions Across Selected Categorical Predictors",
    subtitle = "Each bar shows the edible/poisonous mix within one feature level",
    x = "Encoded feature level",
    y = "Class proportion",
    fill = "Class"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top")

eda_plots <- list(
  class_distribution = class_distribution_plot,
  missing_values = missing_values_plot,
  numeric_distributions = numeric_distribution_plot,
  numeric_boxplots = numeric_boxplot,
  categorical_class_proportions = categorical_class_plot
)

if (!exists("RUN_PLOTS")) {
  RUN_PLOTS <- TRUE
}
if (RUN_PLOTS) {
  for (plot_object in eda_plots) {
    print(plot_object)
  }
}

cat("\nEDA preparation completed successfully.\n")
cat("Use names(eda_plots) to list the available plot objects.\n")
