# =============================================================================
# PIMS ATLAS - FULL PIPELINE WITH DIAGNOSTIC MODE, JOB ZONE FILTER, AND RADAR CHART
# (Consolidated Mapping Files, Norm Groups, Radar Visualisation with Zero Axis)
# Developed by Gary C Townsend (2026)
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(stringr)
library(ggplot2)
library(gridExtra)
library(grid)
library(httr)
library(jsonlite)
library(tools)
library(fmsb)       # For radar chart
library(scales)     # For alpha() transparency

# ---- 1. CONFIGURATION ----
work_styles_folder <- "C:/Users/garyt/OneDrive/Documents/Box Sync Laptop/Emeris/Research/Project PIMS/PIMS Atlas Development/R-Folders"
norm_groups_folder <- "C:/Users/garyt/OneDrive/Documents/Box Sync Laptop/Emeris/Research/Project PIMS/PIMS Atlas Development/R-Folders/NormGroups"
graphs_folder   <- "C:/Users/garyt/OneDrive/Documents/Box Sync Laptop/Emeris/Research/Project PIMS/PIMS Atlas Development/R-Folders/Graphs"
reports_folder  <- "C:/Users/garyt/OneDrive/Documents/Box Sync Laptop/Emeris/Research/Project PIMS/PIMS Atlas Development/R-Folders/Reports"
userscores_folder <- "C:/Users/garyt/OneDrive/Documents/Box Sync Laptop/Emeris/Research/Project PIMS/PIMS Atlas Development/R-Folders/UserScores"

for (f in c(graphs_folder, reports_folder, userscores_folder)) {
  if (!dir.exists(f)) dir.create(f, recursive = TRUE)
}

min_job_zone <- 3
min_impact_threshold <- 90
congruence_threshold <- 0.2
top_n <- 3
z_diff_threshold <- 0.5

work_style_files <- c(
  "Attention to Detail.xlsx", "Cautiousness.xlsx", "Dependability.xlsx",
  "Integrity.xlsx", "Self-Control.xlsx", "Stress Tolerance.xlsx",
  "Cooperation.xlsx", "Empathy.xlsx", "Humility.xlsx", "Optimism.xlsx",
  "Sincerity.xlsx", "Social Orientation.xlsx", "Achievement Orientation.xlsx",
  "Adaptability.xlsx", "Initiative.xlsx", "Innovation.xlsx",
  "Intellectual Curiosity.xlsx", "Leadership Orientation.xlsx",
  "Perseverance.xlsx", "Self-Confidence.xlsx", "Tolerance for Ambiguity.xlsx"
)

mapping_file <- "PIMS Facet Mapping Matrix.xlsx"
anchors_file <- "PIMS Motivational Anchors.xlsx"

facet_names <- c(
  "Thrill-seeking", "Contracted", "Sociable", "Assertive",
  "Unguarded", "Engaging", "Compassionate", "Empathic",
  "Altruistic", "Collaborative", "Modest", "Candid",
  "Resilient", "Self-controlled", "Relaxed", "Optimistic",
  "Tempered", "Impervious", "Directed", "Competent",
  "Structured", "Self-disciplined", "Dutiful", "Cautious",
  "Creative", "Unconventional", "Complex", "Conceptual",
  "Conversant", "Composed"
)

fixed_columns <- c("Impact", "Job Zone", "Code", "Occupation")
required_work_columns <- c(fixed_columns, facet_names)

# ---- 2. LOAD NORMATIVE DATA ----
load_norm_data <- function(norm_groups_folder, facet_names) {
  cat("\n📄 Loading normative data from:", norm_groups_folder, "\n")
  csv_files <- list.files(norm_groups_folder, pattern = "\\.csv$", full.names = TRUE)
  excel_files <- list.files(norm_groups_folder, pattern = "\\.xlsx$", full.names = TRUE)
  all_files <- c(csv_files, excel_files)
  if (length(all_files) == 0) {
    cat("⚠️ No CSV or Excel files found. Using hardcoded stats.\n")
    return(NULL)
  }
  cat(glue("📂 Found {length(all_files)} file(s):\n"))
  for (f in all_files) cat(glue("  - {basename(f)}\n"))
  
  all_data <- list()
  for (file_path in all_files) {
    tryCatch({
      df <- NULL
      file_ext <- tolower(tools::file_ext(file_path))
      if (file_ext == "csv") {
        df <- read.csv(file_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
        cat(glue("  📄 Reading CSV: '{basename(file_path)}'..."))
      } else if (file_ext == "xlsx") {
        cat(glue("  📊 Reading Excel: '{basename(file_path)}'..."))
        sheet_names <- excel_sheets(file_path)
        cat(glue(" found sheets: {paste(sheet_names, collapse=', ')}"))
        selected_sheet <- NULL
        for (sheet in sheet_names) {
          test_df <- read_xlsx(file_path, sheet = sheet, n_max = 5)
          colnames(test_df) <- trimws(colnames(test_df))
          colnames(test_df) <- gsub("\\.", "-", colnames(test_df))
          present <- intersect(facet_names, colnames(test_df))
          if (length(present) >= 28) {
            selected_sheet <- sheet
            cat(glue("\n    ✅ Found norm data in sheet: '{sheet}'"))
            break
          }
        }
        if (is.null(selected_sheet)) selected_sheet <- sheet_names[1]
        df <- read_xlsx(file_path, sheet = selected_sheet)
        cat(glue("\n    📊 Read {nrow(df)} rows from sheet '{selected_sheet}'"))
      } else {
        cat(glue("  ⚠️ Unsupported file type: '{basename(file_path)}'. Skipping.\n"))
        next
      }
      colnames(df) <- trimws(colnames(df))
      colnames(df) <- gsub("\\.", "-", colnames(df))
      present <- intersect(facet_names, colnames(df))
      if (length(present) < 28) {
        cat(glue(" ⚠️ Has only {length(present)} of {length(facet_names)} required columns. Skipping.\n"))
        next
      }
      df_subset <- df[, facet_names, drop = FALSE]
      df_subset <- as.data.frame(lapply(df_subset, as.numeric))
      df_subset <- df_subset[rowSums(is.na(df_subset)) < length(facet_names), ]
      if (nrow(df_subset) > 0) {
        all_data[[basename(file_path)]] <- df_subset
        cat(glue(" ✅ Loaded {nrow(df_subset)} records\n"))
      } else {
        cat(glue(" ⚠️ No valid records found. Skipping.\n"))
      }
    }, error = function(e) {
      cat(glue("  ❌ Error reading '{basename(file_path)}': {e$message}\n"))
    })
  }
  if (length(all_data) == 0) {
    cat("⚠️ No valid normative data loaded. Using hardcoded stats.\n")
    return(NULL)
  }
  combined_norm <- bind_rows(all_data)
  cat(glue("\n📊 Total norm sample: {nrow(combined_norm)} records\n"))
  norm_stats <- data.frame(
    facet = facet_names,
    mean = sapply(facet_names, function(f) mean(combined_norm[[f]], na.rm = TRUE)),
    sd = sapply(facet_names, function(f) sd(combined_norm[[f]], na.rm = TRUE)),
    n = sapply(facet_names, function(f) sum(!is.na(combined_norm[[f]]))),
    stringsAsFactors = FALSE
  )
  cat("\n📊 Norm statistics summary:\n")
  cat(glue("  Sample size range: {min(norm_stats$n)} - {max(norm_stats$n)}\n"))
  cat(glue("  Mean range: {round(min(norm_stats$mean), 2)} - {round(max(norm_stats$mean), 2)}\n"))
  cat(glue("  SD range: {round(min(norm_stats$sd), 2)} - {round(max(norm_stats$sd), 2)}\n"))
  return(norm_stats)
}

norm_stats <- load_norm_data(norm_groups_folder, facet_names)
if (!is.null(norm_stats)) {
  training_means <- norm_stats$mean
  training_sds <- norm_stats$sd
  names(training_means) <- names(training_sds) <- facet_names
  cat("✅ Normative statistics loaded.\n")
} else {
  cat("⚠️ Using hardcoded placeholder statistics.\n")
  training_means <- c(2.60,2.51,2.47,3.02,2.75,2.62,2.82,3.04,2.49,2.91,
                      3.07,3.08,2.87,2.70,2.47,2.87,2.57,2.52,2.63,2.90,
                      3.02,2.88,2.85,3.10,2.86,2.90,3.04,2.84,2.94,2.50)
  training_sds <- c(0.64,0.58,0.56,0.81,0.73,0.61,0.58,0.73,0.66,0.74,
                    0.84,0.56,0.75,0.74,0.58,0.61,0.62,0.79,0.65,0.66,
                    0.82,0.76,0.74,0.85,0.75,0.76,0.56,0.61,0.71,0.59)
  names(training_means) <- names(training_sds) <- facet_names
}

# ---- 3. HELPER FUNCTIONS ----
symbol_to_demand <- function(symbol, facet_mean, facet_sd, z_threshold = 0.7) {
  if (is.na(symbol) || symbol == "" || symbol == "0") return(NA_real_)
  if (symbol %in% c("↑", "Δ")) return(facet_mean + z_threshold * facet_sd)
  if (symbol %in% c("↓", "Λ")) return(facet_mean - z_threshold * facet_sd)
  if (symbol == "↔") return(facet_mean)
  return(NA_real_)
}

standardize_user <- function(user_raw, means = training_means, sds = training_sds) {
  user_raw <- pmax(1, pmin(4, user_raw))
  return((user_raw - means) / sds)
}

# ---- 4. LOAD ALL WORK STYLES ----
load_all_work_styles <- function(folder_path) {
  if (!dir.exists(folder_path)) stop(glue("Folder not found: {folder_path}"))
  all_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  all_file_names <- basename(all_files)
  found_files <- intersect(all_file_names, work_style_files)
  missing_files <- setdiff(work_style_files, found_files)
  cat(glue("📂 Found {length(found_files)} of {length(work_style_files)} expected Work Style files.\n"))
  if (length(missing_files) > 0) cat(glue("⚠️ Missing: {paste(missing_files, collapse=', ')}\n"))
  
  all_data <- list()
  for (file_name in found_files) {
    file_path <- file.path(folder_path, file_name)
    cat(glue("\n  Loading: '{file_name}'..."))
    tryCatch({
      sheet_names <- excel_sheets(file_path)
      template_names <- c("All Occupations", "Template", "Sheet1", "All_Occupations")
      data_sheets <- sheet_names[!sheet_names %in% template_names]
      selected_sheet <- NULL
      for (s in data_sheets) {
        df_test <- read_xlsx(file_path, sheet = s, n_max = 5)
        if (all(required_work_columns %in% colnames(df_test))) { selected_sheet <- s; break }
      }
      if (is.null(selected_sheet)) {
        for (s in sheet_names) {
          df_test <- read_xlsx(file_path, sheet = s, n_max = 5)
          if (all(required_work_columns %in% colnames(df_test))) { selected_sheet <- s; break }
        }
      }
      if (is.null(selected_sheet)) selected_sheet <- sheet_names[1]
      
      df <- read_xlsx(file_path, sheet = selected_sheet)
      df <- df %>% mutate(across(where(is.character), ~ {
        x <- trimws(as.character(.))
        x <- gsub("[^[:print:]]", "", x)
        x <- gsub("\\u200B", "", x)
        x <- gsub("\\u00A0", " ", x)
        return(x)
      }))
      colnames(df) <- trimws(str_squish(colnames(df)))
      missing_cols <- setdiff(required_work_columns, colnames(df))
      if (length(missing_cols) > 0) { cat("❌ Missing columns.\n"); next }
      df <- df %>% select(all_of(required_work_columns))
      df$Impact <- as.numeric(df$Impact)
      if (nrow(df) == 0 || all(is.na(df$Impact))) { cat("❌ No data.\n"); next }
      df$Work_Style <- gsub("\\.xlsx$", "", file_name)
      cat(glue(" read {nrow(df)} rows. ✅"))
      all_data[[file_name]] <- df
    }, error = function(e) {
      cat(glue("❌ ERROR: {e$message}\n"))
    })
  }
  if (length(all_data) == 0) stop("No valid Work Style files loaded.")
  combined <- bind_rows(all_data)
  cat(glue("\n✅ Combined: {nrow(combined)} rows from {length(all_data)} files.\n"))
  return(combined)
}

# ---- 5. LOAD MAPPING DATA ----
cat("\n📄 Loading mapping file:", mapping_file, "\n")
expected_mapping_cols <- c(
  "O*NET Broad Work Style Element",
  "O*NET Work Style Element", 
  "PIMS Motivation Element",
  "TPQ Required Facet",
  "Motivation Symbol",
  "TPQ Facet Role",
  "TPQ Facet Psychological Function"
)
mapping_data <- read_xlsx(file.path(work_styles_folder, mapping_file))
colnames(mapping_data) <- trimws(colnames(mapping_data))
missing <- setdiff(expected_mapping_cols, colnames(mapping_data))
if (length(missing) > 0) stop("Column mismatch in mapping file.")
mapping_data <- mapping_data %>%
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>%
  mutate(across(where(is.character), ~ trimws(as.character(.)))) %>%
  distinct() %>%
  filter(!if_all(where(is.character), ~ is.na(.) | . == ""))
cat(glue("✅ Mapping data loaded with {nrow(mapping_data)} rows\n"))

# ---- 6. LOAD ANCHORS DATA ----
cat("\n📄 Loading PIMS Motivational Anchors...\n")
expected_anchors_cols <- c(
  "O*NET Broad Work Style Element",
  "O*NET Work Style Element",
  "Primary Motivator",
  "PIMS Motivation Element",
  "PIMS Motivation Intrinsic Driver (The \"How\" it feels)",
  "PIMS Motivation Extrinsic Driver (The \"What\" it gains)"
)
anchors_data <- read_xlsx(file.path(work_styles_folder, anchors_file))
colnames(anchors_data) <- trimws(colnames(anchors_data))
missing <- setdiff(expected_anchors_cols, colnames(anchors_data))
if (length(missing) > 0) stop("Column mismatch in anchors file.")
anchors_data <- anchors_data %>%
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>%
  mutate(across(where(is.character), ~ trimws(as.character(.)))) %>%
  distinct() %>%
  filter(!if_all(where(is.character), ~ is.na(.) | . == ""))
cat(glue("✅ Anchors data loaded with {nrow(anchors_data)} rows\n"))

# ---- 7. BUILD OCCUPATION VECTOR ----
build_occupation_vector <- function(soc_code, occ_data, means, sds, min_impact = 90) {
  occ_rows_all <- occ_data %>% filter(Code == soc_code)
  if (nrow(occ_rows_all) == 0) return(NULL)
  occ_rows <- occ_rows_all %>% filter(Impact >= min_impact)
  if (nrow(occ_rows) == 0) return(NULL)
  total_impact <- sum(occ_rows$Impact, na.rm = TRUE)
  if (total_impact == 0) return(NULL)
  occ_rows <- occ_rows %>%
    group_by(Work_Style) %>%
    mutate(element_weight = Impact / total_impact) %>%
    ungroup()
  
  aggregated_demand <- numeric(length(facet_names))
  aggregated_weight <- numeric(length(facet_names))
  names(aggregated_demand) <- names(aggregated_weight) <- facet_names
  
  for (f in facet_names) {
    symbols_found <- c()
    demands_raw <- c()
    for (i in 1:nrow(occ_rows)) {
      symbol <- occ_rows[[f]][i]
      if (!is.na(symbol) && symbol != "" && symbol != "0") {
        demand <- symbol_to_demand(symbol, means[f], sds[f])
        weighted_demand <- demand * occ_rows$element_weight[i]
        demands_raw <- c(demands_raw, weighted_demand)
        symbols_found <- c(symbols_found, symbol)
      }
    }
    if (length(demands_raw) > 0) {
      aggregated_demand[f] <- sum(demands_raw, na.rm = TRUE)
      freq_ratio <- length(demands_raw) / nrow(occ_rows)
      has_primary <- any(symbols_found %in% c("↑", "↓"))
      has_secondary <- any(symbols_found %in% c("Δ", "Λ"))
      has_modulation <- any(symbols_found == "↔")
      if (has_primary) {
        aggregated_weight[f] <- 1.0
      } else if (has_secondary) {
        aggregated_weight[f] <- 0.5 + 0.2 * freq_ratio
      } else if (has_modulation) {
        aggregated_weight[f] <- 0.3 + 0.2 * freq_ratio
      } else {
        aggregated_weight[f] <- 0.0
      }
    } else {
      aggregated_demand[f] <- 0
      aggregated_weight[f] <- 0.0
    }
  }
  aggregated_demand_z <- (aggregated_demand - means) / sds
  aggregated_demand_z <- pmax(-3, pmin(3, aggregated_demand_z))
  considered_workstyles <- unique(occ_rows_all$Work_Style)
  used_workstyles <- unique(occ_rows$Work_Style)
  return(list(
    soc_code = soc_code,
    occupation_name = occ_rows$Occupation[1],
    element_weights = occ_rows %>% select(Work_Style, element_weight) %>% distinct(),
    total_styles_available = nrow(occ_rows_all),
    styles_used = nrow(occ_rows),
    considered_workstyles = considered_workstyles,
    used_workstyles = used_workstyles,
    facet_vector = data.frame(
      facet = facet_names,
      demand_z = aggregated_demand_z,
      weight = aggregated_weight,
      stringsAsFactors = FALSE
    )
  ))
}

weighted_cosine <- function(user_z, occ_vector) {
  relevant <- occ_vector$facet_vector %>% filter(weight > 0)
  if (nrow(relevant) == 0) return(0)
  a <- user_z[relevant$facet]
  b <- relevant$demand_z
  w <- relevant$weight
  numerator <- sum(w * a * b, na.rm = TRUE)
  denom <- sqrt(sum(w * a^2, na.rm = TRUE) * sum(w * b^2, na.rm = TRUE))
  if (denom == 0 || is.nan(denom) || is.infinite(denom)) return(0)
  return(max(-1, min(1, numerator / denom)))
}

build_full_atlas <- function(occ_data, means, sds, min_impact = 90) {
  soc_codes <- unique(occ_data$Code)
  cat(glue("\n🏗️  Building PIMS Atlas with {length(soc_codes)} occupations (using Impact ≥ {min_impact})...\n"))
  atlas <- list()
  skipped <- 0
  for (i in seq_along(soc_codes)) {
    vec <- build_occupation_vector(soc_codes[i], occ_data, means, sds, min_impact)
    if (!is.null(vec)) atlas[[soc_codes[i]]] <- vec else skipped <- skipped + 1
    if (i %% 100 == 0) cat(glue("  Processed {i}/{length(soc_codes)}...\n"))
  }
  cat(glue("✅ Atlas built: {length(atlas)} occupations loaded ({skipped} skipped).\n"))
  return(atlas)
}

# ---- 8. MATCH USER TO ATLAS ----
match_user_to_atlas <- function(user_z, atlas) {
  cat(glue("\n🔍 Matching user to {length(atlas)} occupations...\n"))
  if (length(atlas) == 0) {
    cat("⚠️ Atlas is empty. No occupations to match.\n")
    return(data.frame(Code = character(), Occupation = character(), Congruence = numeric(), stringsAsFactors = FALSE))
  }
  results <- list()
  for (i in seq_along(atlas)) {
    occ_vec <- atlas[[i]]
    cs <- weighted_cosine(user_z, occ_vec)
    results[[i]] <- data.frame(
      Code = occ_vec$soc_code,
      Occupation = occ_vec$occupation_name,
      Congruence = cs,
      stringsAsFactors = FALSE
    )
    if (i %% 100 == 0) cat(glue("  Matched {i}/{length(atlas)}...\n"))
  }
  results <- bind_rows(results)
  results <- results %>% arrange(desc(Congruence))
  return(results)
}

# ---- 9. HELPER FUNCTIONS FOR MAPPING ----
get_motivation_elements_from_styles <- function(work_styles, mapping_data, anchors_data) {
  motivs <- c()
  for (ws in work_styles) {
    ws_clean <- trimws(ws)
    rows <- mapping_data %>% filter(`O*NET Work Style Element` == ws_clean)
    if (nrow(rows) > 0) {
      motiv <- rows$`PIMS Motivation Element`[1]
      if (!is.na(motiv) && motiv != "") motivs <- c(motivs, motiv)
    } else {
      anchor_row <- anchors_data %>% filter(`O*NET Work Style Element` == ws_clean)
      if (nrow(anchor_row) > 0) {
        motiv <- anchor_row$`PIMS Motivation Element`[1]
        if (!is.na(motiv) && motiv != "") motivs <- c(motivs, motiv)
      }
    }
  }
  return(unique(motivs))
}

get_facet_metadata <- function(facet, work_styles_df, mapping_data) {
  motivations <- c()
  psych_funcs <- c()
  roles <- c()
  directions <- c()
  for (ws in unique(work_styles_df$Work_Style)) {
    ws_clean <- trimws(ws)
    facet_clean <- trimws(facet)
    rows <- mapping_data %>%
      filter(`O*NET Work Style Element` == ws_clean & `TPQ Required Facet` == facet_clean)
    if (nrow(rows) > 0) {
      motiv <- rows$`PIMS Motivation Element`[1]
      if (!is.na(motiv) && motiv != "") motivations <- c(motivations, motiv)
      pfunc <- rows$`TPQ Facet Psychological Function`[1]
      if (!is.na(pfunc) && pfunc != "") psych_funcs <- c(psych_funcs, pfunc)
      role <- rows$`TPQ Facet Role`[1]
      if (!is.na(role) && role != "") roles <- c(roles, role)
      dir <- rows$`Motivation Symbol`[1]
      if (!is.na(dir) && dir != "") directions <- c(directions, dir)
    }
  }
  return(list(
    motivation = if (length(motivations) > 0) paste(unique(motivations), collapse = ", ") else "Not specified",
    psych_function = if (length(psych_funcs) > 0) paste(unique(psych_funcs), collapse = ", ") else "Not specified",
    role = if (length(roles) > 0) paste(unique(roles), collapse = ", ") else "Not specified",
    direction = if (length(directions) > 0) paste(unique(directions), collapse = ", ") else "Not specified"
  ))
}

# ---- 10. LOAD USER SCORES ----
user_file <- file.path(userscores_folder, "user_tpq_scores.csv")
if (file.exists(user_file)) {
  user_data <- read.csv(user_file, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  colnames(user_data) <- gsub("\\.", "-", colnames(user_data))
  present <- intersect(facet_names, colnames(user_data))
  missing <- setdiff(facet_names, present)
  if (length(missing) > 0) cat("⚠️ Missing columns in CSV: ", paste(missing, collapse=", "), "\n")
  user_raw <- numeric(length(facet_names))
  names(user_raw) <- facet_names
  user_raw[] <- 2.5
  for (f in present) {
    val <- as.numeric(user_data[1, f])
    if (!is.na(val)) user_raw[f] <- val
  }
} else {
  cat("\n⚠️ User CSV file not found. Using manual test profile.\n")
  user_raw <- c(2.5,2.3,2.5,3.5,3.0,3.5,3.8,3.8,2.5,3.0,
                4.0,2.5,4.0,3.8,2.5,2.3,3.0,3.0,3.5,4.0,
                3.8,4.0,3.8,4.0,3.8,2.8,3.3,4.0,4.0,2.3)
  names(user_raw) <- facet_names
}
user_raw <- pmax(1, pmin(4, user_raw))
user_z <- standardize_user(user_raw)

# ---- 11. RUN PIPELINE ----
cat("\n📂 Starting PIMS Atlas Pipeline\n")
cat(strrep("═", 70), "\n")
cat(glue("📁 Work Styles folder: {work_styles_folder}\n"))
cat(glue("📁 Norm Groups folder: {norm_groups_folder}\n"))
cat(glue("📁 Graphs folder:     {graphs_folder}\n"))
cat(glue("📁 Reports folder:    {reports_folder}\n"))
cat(glue("📁 User Scores folder: {userscores_folder}\n"))
cat(glue("⚙️  Impact threshold: {min_impact_threshold}\n"))
cat(glue("⚙️  Job Zone filter: ≥ {min_job_zone}\n"))

occupation_data_raw <- load_all_work_styles(work_styles_folder)
occupation_data <- occupation_data_raw %>% filter(`Job Zone` >= min_job_zone)
cat(glue("\n📊 Job Zone filter applied: kept {n_distinct(occupation_data$Code)} occupations.\n"))

atlas <- build_full_atlas(occupation_data, training_means, training_sds, min_impact_threshold)
ranked_results <- match_user_to_atlas(user_z, atlas)

if (nrow(ranked_results) == 0) {
  cat("\n⚠️ No occupations matched. Exiting.\n")
  stop("No matches found. Check filters and data.")
}

# ---- 12. DEFINE TRAIT CLUSTERS (Derived from O*NET Broad Work Styles) ----
clusters <- list(
  # 1. Conscientious & Rule Oriented (Precision, Security, Duty, Authenticity)
  Conscientious_Rule_Oriented = c(
    "Structured", "Self-disciplined", "Dutiful", "Cautious", 
    "Creative", "Thrill-seeking", "Unconventional", "Tempered", 
    "Resilient", "Directed", "Optimistic", "Candid", 
    "Altruistic", "Relaxed", "Assertive", "Unguarded", "Empathic"
  ),
  
  # 2. Emotionally Resilient (Mastery of self, Endurance)
  Emotionally_Resilient = c(
    "Self-controlled", "Tempered", "Composed", "Assertive", 
    "Relaxed", "Thrill-seeking", "Resilient", "Directed", 
    "Optimistic", "Impervious", "Dutiful"
  ),
  
  # 3. Interpersonally Oriented (Harmony, Attunement, Service, Hope, Truth, Belonging)
  Interpersonally_Oriented = c(
    "Collaborative", "Compassionate", "Modest", "Sociable", 
    "Assertive", "Self-disciplined", "Engaging", "Empathic", 
    "Altruistic", "Candid", "Unguarded", "Relaxed", "Competent", 
    "Optimistic", "Resilient", "Contracted"
  ),
  
  # 4. Proactive & Growth Oriented (Mastery of tasks, Fluidity, Agency, Discovery, Understanding, Influence, Grit, Assurance, Exploration)
  Proactive_Growth_Oriented = c(
    "Competent", "Self-disciplined", "Directed", "Assertive", 
    "Resilient", "Modest", "Unconventional", "Complex", 
    "Creative", "Thrill-seeking", "Structured", "Relaxed", 
    "Cautious", "Conceptual", "Conversant", "Sociable", 
    "Engaging", "Altruistic", "Compassionate", "Dutiful", 
    "Optimistic", "Impervious", "Composed"
  )
)

# ---- 13. API CONFIGURATION ----
api_key <- "AQ.Ab8RN6LwdO8dwQAbrDTDYPMw2tjuhEOm-8T9V-aSXHTIsBdGtw"  # <-- REPLACE WITH YOUR ACTUAL KEY
api_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-flash-latest:generateContent"
model <- "gemini-flash-latest"

# ---- 14. HELPER: CALL GEMINI API ----
generate_ai_narrative <- function(prompt, api_key, api_url, model) {
  if (api_key == "" || is.null(api_key) || api_key == "YOUR_GEMINI_API_KEY_HERE") {
    stop("API key not provided or still placeholder.")
  }
  headers <- add_headers(
    `X-goog-api-key` = api_key,
    `Content-Type` = "application/json"
  )
  body <- list(
    contents = list(
      parts = list(
        list(text = prompt)
      )
    ),
    generationConfig = list(
      temperature = 0.85,
      maxOutputTokens = 4096
    )
  )
  response <- POST(api_url, headers, body = toJSON(body, auto_unbox = TRUE))
  status <- status_code(response)
  content_text <- content(response, as = "text")
  if (status != 200) {
    error_msg <- tryCatch({
      parsed <- fromJSON(content_text)
      if (!is.null(parsed$error$message)) parsed$error$message else content_text
    }, error = function(e) content_text)
    stop(glue::glue("API returned status {status}: {error_msg}"))
  }
  result <- content(response, as = "parsed")
  return(result$candidates[[1]]$content$parts[[1]]$text)
}

# ---- 15. SAVE NARRATIVE AS PDF ----
save_narrative_as_pdf <- function(narrative, occ_name, congruence, output_file) {
  pdf(output_file, width = 8.27, height = 11.69, onefile = TRUE)
  pushViewport(viewport(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(7.27, "in"), height = unit(10.69, "in")
  ))
  y_pos <- 0.95
  line_height <- 0.045
  min_y <- 0.05
  new_page <- function() {
    grid.newpage()
    pushViewport(viewport(
      x = unit(0.5, "npc"), y = unit(0.5, "npc"),
      width = unit(7.27, "in"), height = unit(10.69, "in")
    ))
    y_pos <<- 0.95
    grid.text(paste(occ_name, "(continued)"), 
              x = 0.5, y = 0.98, gp = gpar(fontsize = 12, fontface = "bold"))
    y_pos <<- 0.93
  }
  grid.text(paste(occ_name, "– Congruence:", round(congruence, 3)), 
            x = 0.5, y = 0.98, gp = gpar(fontsize = 14, fontface = "bold"))
  y_pos <- 0.93
  lines <- strsplit(narrative, "\n")[[1]]
  for (line in lines) {
    is_heading <- grepl("^[0-9]+\\.", trimws(line)) || grepl("\\*\\*", line)
    is_bullet <- grepl("^\\s*[-•*]", line)
    if (trimws(line) == "") {
      y_pos <- y_pos - line_height * 0.5
      next
    }
    if (is_heading) {
      y_pos <- y_pos - line_height * 0.8
      clean_line <- gsub("\\*\\*", "", line)
      wrapped <- strwrap(clean_line, width = 100, simplify = TRUE)
      for (wline in wrapped) {
        if (y_pos < min_y) new_page()
        grid.text(wline, x = 0.05, y = y_pos, just = "left",
                  gp = gpar(fontsize = 11, fontface = "bold"))
        y_pos <- y_pos - line_height * 1.1
      }
    } else if (is_bullet) {
      clean_line <- gsub("^\\s*[-•*]\\s*", "• ", line)
      wrapped <- strwrap(clean_line, width = 95, simplify = TRUE)
      for (wline in wrapped) {
        if (y_pos < min_y) new_page()
        grid.text(wline, x = 0.08, y = y_pos, just = "left",
                  gp = gpar(fontsize = 10))
        y_pos <- y_pos - line_height * 1.05
      }
    } else {
      wrapped <- strwrap(line, width = 100, simplify = TRUE)
      for (wline in wrapped) {
        if (y_pos < min_y) new_page()
        grid.text(wline, x = 0.05, y = y_pos, just = "left",
                  gp = gpar(fontsize = 10))
        y_pos <- y_pos - line_height * 1.05
      }
    }
    y_pos <- y_pos - line_height * 0.2
  }
  grid.text(paste("Page", length(dev.list())), 
            x = 0.95, y = 0.02, just = "right", gp = gpar(fontsize = 8, col = "grey"))
  popViewport()
  dev.off()
}

# ---- 16. MAIN BRANCH: DIAGNOSTIC vs NORMAL MODE ----
max_congruence <- max(ranked_results$Congruence, na.rm = TRUE)

if (max_congruence < congruence_threshold) {
  # DIAGNOSTIC MODE
  cat("\n", strrep("═", 70), "\n")
  cat("🔍 DIAGNOSTIC MODE: No strong career matches found (max congruence =", round(max_congruence, 3), ").\n")
  cat(strrep("═", 70), "\n\n")
  
  cluster_scores <- sapply(clusters, function(cl) mean(user_z[cl], na.rm = TRUE))
  high_clusters <- names(sort(cluster_scores, decreasing = TRUE))[1:2]
  low_clusters <- names(sort(cluster_scores, decreasing = FALSE))[1:2]
  top_matches <- ranked_results %>%
    slice_head(n = 5) %>%
    mutate(entry = paste0(Occupation, " (", round(Congruence, 3), ")")) %>%
    pull(entry) %>%
    paste(collapse = "\n  • ")
  
  diag_prompt <- glue::glue(
    "You are a senior career psychologist and expert in the PIMS personality framework.

    **DIAGNOSTIC MODE**: The client's highest congruence score is {round(max_congruence, 3)}, 
    which is below the threshold for a positive career match.

    **Client's personality profile**:
    - Highest trait clusters: {paste(high_clusters, collapse = ' and ')} 
      (mean Z-scores: {paste(round(cluster_scores[high_clusters], 2), collapse = ' and ')})
    - Lowest trait clusters: {paste(low_clusters, collapse = ' and ')}
      (mean Z-scores: {paste(round(cluster_scores[low_clusters], 2), collapse = ' and ')})

    **The closest matches were**:
    {top_matches}

    Write a compassionate, insightful, and actionable diagnostic report (approx 350-450 words) with:
    1. Core Insight
    2. Why This Matters
    3. Hybrid / Custom Pathways
    4. Recommended Next Steps
    Use a professional, encouraging tone."
  )
  
  if (api_key != "" && !is.null(api_key) && api_key != "YOUR_GEMINI_API_KEY_HERE") {
    tryCatch({
      narrative <- generate_ai_narrative(diag_prompt, api_key, api_url, model)
      cat("\n📝 DIAGNOSTIC REPORT:\n", narrative, "\n")
      write(paste(strrep("═", 70), "\nDIAGNOSTIC REPORT\n", strrep("═", 70), "\n\n", narrative, "\n", sep=""), 
            file = file.path(reports_folder, "diagnostic_report.txt"), append = FALSE)
      pdf_file <- file.path(reports_folder, "diagnostic_report.pdf")
      save_narrative_as_pdf(narrative, "Diagnostic Report", max_congruence, pdf_file)
      cat(glue("📄 Diagnostic PDF saved to: {pdf_file}\n"))
    }, error = function(e) {
      cat("⚠️ AI generation failed. Using fallback text.\n")
    })
  } else {
    cat("⚠️ No valid API key. Using fallback diagnostic text.\n")
  }
  
} else {
  # NORMAL MODE
  cat("\n", strrep("═", 70), "\n")
  cat("✅ NORMAL MODE: Positive career matches found (max congruence =", round(max_congruence, 3), ").\n")
  cat(strrep("═", 70), "\n\n")
  
  # TOP 3 CAREERS
  cat("\n", strrep("═", 70), "\n🏆 TOP CAREER RECOMMENDATIONS\n", strrep("═", 70), "\n\n")
  top_n_print <- min(3, nrow(ranked_results))
  for (i in 1:top_n_print) {
    rank_icon <- if (i <= 3) c("🥇", "🥈", "🥉")[i] else paste0(i, ".")
    cs <- ranked_results$Congruence[i]
    level <- if (cs >= 0.7) "🌟 Excellent" else if (cs >= 0.5) "✅ Strong" else if (cs >= 0.3) "📊 Moderate" else "📈 Developing"
    cat(glue("{rank_icon} {ranked_results$Occupation[i]}"))
    cat(glue("\n     Code: {ranked_results$Code[i]}"))
    cat(glue("\n     Congruence: {round(cs, 4)} ({level})"))
    cat("\n\n")
  }
  cat(strrep("═", 70), "\n")
  
  top_occupations <- ranked_results %>% slice_head(n = top_n)
  
  # EXTRACT VECTORS
  occ_vectors <- list()
  occ_workstyles <- list()
  for (i in seq_len(nrow(top_occupations))) {
    code <- top_occupations$Code[i]
    occ <- atlas[[code]]
    if (is.null(occ)) next
    relevant <- occ$facet_vector %>% filter(weight > 0)
    if (nrow(relevant) == 0) { cat(glue("⚠️ No relevant facets for {code}; skipping.\n")); next }
    occ_vectors[[code]] <- relevant
    occ_workstyles[[code]] <- occ$element_weights
  }
  
  # ---- 16c. RADAR CHART - PERFECTLY LABELED & SPACED ----
  if (length(occ_vectors) > 0) {
    cat("\n📊 Generating radar chart for top", top_n, "occupations...\n")
    
    # ---- Prepare data ----
    all_facets <- facet_names
    
    # Build user profile with zero-masking: irrelevant facets = 0
    user_profile <- numeric(length(all_facets))
    names(user_profile) <- all_facets
    for (i in seq_along(all_facets)) {
      f <- all_facets[i]
      is_relevant <- any(sapply(occ_vectors, function(df) f %in% df$facet[df$weight > 0]))
      if (is_relevant) {
        val <- user_z[f]
        if (is.na(val) || !is.numeric(val)) val <- 0
        user_profile[i] <- val
      } else {
        user_profile[i] <- 0
      }
    }
    
    # Build occupation profiles with zero-masking
    occupation_profiles <- list()
    occ_labels <- c()
    considered_motivs_all <- c()
    core_motivs_all <- c()
    
    for (code in names(occ_vectors)) {
      occ_df <- occ_vectors[[code]]
      occ_profile <- numeric(length(all_facets))
      names(occ_profile) <- all_facets
      for (i in seq_along(all_facets)) {
        f <- all_facets[i]
        row <- occ_df[occ_df$facet == f, ]
        if (nrow(row) > 0 && !is.na(row$weight) && row$weight > 0) {
          val <- row$demand_z
          if (is.na(val) || !is.numeric(val)) val <- 0
          occ_profile[i] <- val
        } else {
          occ_profile[i] <- 0
        }
      }
      
      occ_name <- top_occupations %>% filter(Code == code) %>% pull(Occupation)
      congruence <- top_occupations %>% filter(Code == code) %>% pull(Congruence)
      occupation_profiles[[code]] <- occ_profile
      occ_labels <- c(occ_labels, paste0(occ_name, " (", round(congruence, 3), ")"))
      
      occ <- atlas[[code]]
      considered_motiv <- get_motivation_elements_from_styles(occ$considered_workstyles, mapping_data, anchors_data)
      core_motiv <- get_motivation_elements_from_styles(occ$used_workstyles, mapping_data, anchors_data)
      considered_motivs_all <- c(considered_motivs_all, considered_motiv)
      core_motivs_all <- c(core_motivs_all, core_motiv)
    }
    
    considered_motivs_unique <- unique(considered_motivs_all)
    core_motivs_unique <- unique(core_motivs_all)
    
    # ---- CRITICAL FIX: THE SHIFT METHOD ----
    SHIFT <- 3
    user_shifted <- user_profile + SHIFT
    
    occ_shifted <- list()
    for (code in names(occupation_profiles)) {
      occ_shifted[[code]] <- occupation_profiles[[code]] + SHIFT
    }
    
    # ---- Build radar_df with proper scaling ----
    n_profiles <- 2 + 1 + length(occ_shifted) 
    n_facets <- length(all_facets)
    
    radar_matrix <- matrix(0, nrow = n_profiles, ncol = n_facets)
    colnames(radar_matrix) <- all_facets
    rownames(radar_matrix) <- c("max", "min", "User Profile", occ_labels)
    
    radar_matrix[1, ] <- 6.0
    radar_matrix[2, ] <- 0.0
    radar_matrix[3, ] <- user_shifted
    
    for (i in seq_along(occ_labels)) {
      code <- names(occ_vectors)[i]
      radar_matrix[3 + i, ] <- occ_shifted[[code]]
    }
    
    radar_df <- as.data.frame(radar_matrix)
    for (col in colnames(radar_df)) {
      radar_df[[col]] <- as.numeric(radar_df[[col]])
    }
    
    # ---- Colours ----
    colors <- c("#2E86C1", "#E74C3C", "#27AE60", "#F39C12", "#8E44AD")
    colors <- colors[1:min(nrow(radar_df) - 2, length(colors))]
    
    # ---- Wrap text helpers ----
    wrap_text <- function(text, max_width = 50) {
      if (is.na(text) || text == "" || text == "NULL") return("")
      words <- strsplit(as.character(text), ", ")[[1]]
      if (length(words) == 0 || (length(words) == 1 && words[1] == "")) return("")
      lines <- c()
      current_line <- ""
      for (word in words) {
        if (nchar(current_line) + nchar(word) + 2 > max_width) {
          if (current_line != "") lines <- c(lines, current_line)
          current_line <- word
        } else {
          if (current_line == "") current_line <- word else current_line <- paste(current_line, word, sep = ", ")
        }
      }
      if (current_line != "") lines <- c(lines, current_line)
      return(lines)
    }
    
    # ---- Function to draw radar ----
    draw_radar <- function() {
      # Set up a clean layout with 3 rows
      layout(matrix(c(1, 2, 3, 4), nrow = 4), heights = c(1.2, 0.8, 10, 3.0))
      par(mar = c(0, 0, 0, 0))
      
      # ---- Row 1: Title ----
      plot.new()
      core_title_text <- paste(core_motivs_unique, collapse = ", ")
      if (core_title_text == "" || core_title_text == "NULL") core_title_text <- "No core motivations identified"
      text(0.5, 0.5, paste("Core Motivations:", core_title_text), 
           cex = 2.2, font = 2, col = "black")
      
      # ---- Row 2: Subtitle ----
      plot.new()
      subtitle_text <- glue("Impact threshold: {min_impact_threshold} | Job Zone: ≥ {min_job_zone} | Zero axis = centre ring")
      text(0.5, 0.5, subtitle_text, cex = 1.4, col = "black")
      
      # ---- Row 3: Radar Chart ----
      par(mar = c(1, 1, 2, 1))
      
      # IMPORTANT: Keep all labels exactly as they are (one line) so fmsb places them cleanly
      clean_labels <- all_facets
      
      radarchart(
        radar_df,
        axistype = 1,           
        pcol = colors,
        pfcol = alpha(colors, 0.1),
        plwd = 2.5,
        plty = 1,
        cglcol = "grey80",
        cglty = 1,
        cglwd = 0.8,
        axislabcol = "black",  
        vlcex = 1.1,             
        vlab = clean_labels,    # Pass the original one-line labels to fmsb
        caxislabels = c(-3, -2, 0, 1, 2, 3, 4), 
        calcex = 1.2,
        title = ""
      )
      
      # ---- Legend ----
      legend(
        x = 1.35, y = 1.0,
        legend = rownames(radar_df)[-c(1,2)],
        bty = "n",
        pch = 20,
        col = colors,
        text.col = "black",
        cex = 1.1,
        pt.cex = 1.8,
        title = "Profiles",
        title.col = "black"
      )
      
      # ---- Row 4: Considered Motivations ----
      par(mar = c(0, 0, 0, 0))
      plot.new()
      
      text(0.5, 0.85, "Considered Motivations (All Work Styles):", 
           font = 2, cex = 1.6, col = "black") 
      
      considered_text <- paste(considered_motivs_unique, collapse = ", ")
      if (considered_text == "" || considered_text == "NULL") considered_text <- "No considered motivations identified"
      
      # Force split into 3 lines with small gaps
      words <- considered_motivs_unique
      chunk_size <- ceiling(length(words) / 3)
      wrapped_considered <- c()
      for (i in seq(1, length(words), chunk_size)) {
        chunk <- words[i:min(i + chunk_size - 1, length(words))]
        wrapped_considered <- c(wrapped_considered, paste(chunk, collapse = ", "))
      }
      
      # Adjusted spacing for larger text
      line_height <- 0.16
      start_y <- 0.65
      for (j in seq_along(wrapped_considered)) {
        y_pos <- start_y - (j - 1) * line_height
        if (y_pos > 0.05) {
          text(0.5, y_pos, wrapped_considered[j], cex = 1.4, col = "black") 
        }
      }
      
      layout(1)
      par(mar = c(5,4,4,2) + 0.1)
    }
    
    # ---- Draw on screen ----
    draw_radar()
    
    # ---- Save PDF and PNG ----
    pdf_file <- file.path(graphs_folder, "pims_radar_comparison.pdf")
    if (file.exists(pdf_file)) {
      tryCatch({
        file.remove(pdf_file)
      }, error = function(e) {
        pdf_file <- file.path(graphs_folder, paste0("pims_radar_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))
        message(glue("Using alternative PDF filename: {basename(pdf_file)}"))
      })
    }
    pdf(pdf_file, width = 14, height = 11)
    draw_radar()
    dev.off()
    cat(glue("📊 Radar chart (PDF) saved to '{pdf_file}'\n"))
    
    png_file <- file.path(graphs_folder, "pims_radar_comparison.png")
    if (file.exists(png_file)) {
      tryCatch({
        file.remove(png_file)
      }, error = function(e) {
        png_file <- file.path(graphs_folder, paste0("pims_radar_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        message(glue("Using alternative PNG filename: {basename(png_file)}"))
      })
    }
    png(png_file, width = 1400, height = 1100, res = 150)
    draw_radar()
    dev.off()
    cat(glue("📊 Radar chart (PNG) saved to '{png_file}'\n"))
  }
  
  # ---- 16d. CONSOLE NARRATIVE ----
  cat("\n", strrep("═", 70), "\n")
  cat("📝 PIMS NARRATIVE REPORT (CONSOLE)\n")
  cat(strrep("═", 70), "\n\n")
  
  for (i in seq_len(nrow(top_occupations))) {
    code <- top_occupations$Code[i]
    occ_name <- top_occupations$Occupation[i]
    congruence <- top_occupations$Congruence[i]
    occ_vec <- occ_vectors[[code]]
    if (is.null(occ_vec)) next
    work_styles_df <- occ_workstyles[[code]]
    
    facet_details <- occ_vec %>%
      mutate(
        user_z = user_z[facet],
        demand_z = demand_z,
        weight = weight
      ) %>%
      rowwise() %>%
      mutate(
        meta = list(get_facet_metadata(facet, work_styles_df, mapping_data)),
        motivation = meta$motivation,
        psych_function = meta$psych_function,
        role = meta$role,
        direction = meta$direction
      ) %>%
      ungroup() %>%
      mutate(
        interpretation = case_when(
          user_z - demand_z > 0.5 ~ "above (strength)",
          user_z - demand_z < -0.5 ~ "below (development area)",
          TRUE ~ "aligned"
        )
      ) %>%
      filter(weight > 0) %>%
      select(facet, user_z, demand_z, weight, motivation, psych_function, role, direction, interpretation)
    
    cat(glue("\n{letters[i]}. {occ_name} (Code: {code})"))
    cat(glue("\n   Overall Congruence: {round(congruence, 3)}"))
    primary <- facet_details %>% filter(grepl("Core Driver", role, ignore.case=TRUE))
    if (nrow(primary) > 0) {
      cat("\n   • Core requirements:")
      for (f in primary$facet) cat(glue("\n      - {f}"))
    }
    strengths <- facet_details %>% filter(interpretation == "above (strength)") %>% pull(facet)
    dev_areas <- facet_details %>% filter(interpretation == "below (development area)") %>% pull(facet)
    if (length(strengths) > 0) cat(glue("\n   ✅ Strengths: {paste(strengths, collapse=', ')}"))
    if (length(dev_areas) > 0) cat(glue("\n   🔧 Development: {paste(dev_areas, collapse=', ')}"))
    cat("\n\n", strrep("─", 50), "\n")
  }
  
  # ---- 16e. Occupation-specific AI reports ----
  cat("\n", strrep("═", 70), "\n")
  cat("🤖 Generating AI-powered counsellor narratives for top occupations...\n")
  cat(strrep("═", 70), "\n\n")
  
  build_prompt <- function(occ_name, congruence, facet_details, strengths, dev_areas) {
    core_facets <- facet_details %>% filter(grepl("Core Driver", role, ignore.case = TRUE))
    suppressed_facets <- facet_details %>% filter(grepl("Suppression", role, ignore.case = TRUE))
    compensation_facets <- facet_details %>% filter(grepl("Compensation", role, ignore.case = TRUE))
    modulation_facets <- facet_details %>% filter(grepl("Modulation", role, ignore.case = TRUE))
    
    demand_summary <- paste(
      "Core drivers:", if(nrow(core_facets)>0) paste(core_facets$facet, collapse=", ") else "none",
      "\nSuppressed (must be low):", if(nrow(suppressed_facets)>0) paste(suppressed_facets$facet, collapse=", ") else "none",
      "\nCompensatory (can substitute):", if(nrow(compensation_facets)>0) paste(compensation_facets$facet, collapse=", ") else "none",
      "\nModulation (balanced):", if(nrow(modulation_facets)>0) paste(modulation_facets$facet, collapse=", ") else "none"
    )
    
    facet_lines <- c()
    for (j in 1:nrow(facet_details)) {
      row <- facet_details[j, ]
      line <- sprintf(
        "- %s (Role: %s, Direction: %s, Psych function: %s): score = %.2f, ideal = %.2f, weight = %.2f, motivation = %s, interpretation: %s",
        row$facet, row$role, row$direction, row$psych_function,
        row$user_z, row$demand_z, row$weight, row$motivation, row$interpretation
      )
      facet_lines <- c(facet_lines, line)
    }
    facet_text <- paste(facet_lines, collapse = "\n")
    strengths_text <- paste(strengths, collapse = ", ")
    dev_text <- paste(dev_areas, collapse = ", ")
    all_motivations <- unique(facet_details$motivation)
    all_motivations <- all_motivations[all_motivations != "Not specified"]
    motiv_text <- paste(all_motivations, collapse = ", ")
    
    prompt <- glue::glue(
      "You are a professional career counsellor with deep expertise in the PIMS framework. 
      Your task is to produce a concise, evidence-based report for a client based on their PIMS profile.

      **Occupation**: {occ_name}
      **Overall Fit Score**: {round(congruence, 3)} (on a scale from -1 to 1; positive values indicate alignment)
      **Core Motivational Drivers**: {motiv_text}

      **Occupational Demand Signature**: {demand_summary}

      **The client's detailed profile**: {facet_text}

      **Key Strengths**: {strengths_text}
      **Development Areas**: {dev_text}

      Please write a professional, balanced report (approximately 300-400 words) structured as follows:
      1. **Profile Summary**
      2. **Key Strengths** (bullet points)
      3. **Development Areas** (bullet points)
      4. **Conclusion**

      Use a professional, neutral tone."
    )
    return(prompt)
  }
  
  facet_details_list <- list()
  for (i in seq_len(nrow(top_occupations))) {
    code <- top_occupations$Code[i]
    occ_vec <- occ_vectors[[code]]
    if (is.null(occ_vec)) next
    work_styles_df <- occ_workstyles[[code]]
    
    details <- occ_vec %>%
      mutate(
        user_z = user_z[facet],
        demand_z = demand_z,
        weight = weight
      ) %>%
      rowwise() %>%
      mutate(
        meta = list(get_facet_metadata(facet, work_styles_df, mapping_data)),
        motivation = meta$motivation,
        psych_function = meta$psych_function,
        role = meta$role,
        direction = meta$direction
      ) %>%
      ungroup() %>%
      mutate(
        interpretation = case_when(
          user_z - demand_z > 0.5 ~ "above (strength)",
          user_z - demand_z < -0.5 ~ "below (development area)",
          TRUE ~ "aligned"
        )
      ) %>%
      filter(weight > 0) %>%
      select(facet, user_z, demand_z, weight, motivation, psych_function, role, direction, interpretation)
    
    facet_details_list[[code]] <- details
  }
  
  for (i in seq_len(nrow(top_occupations))) {
    code <- top_occupations$Code[i]
    occ_name <- top_occupations$Occupation[i]
    congruence <- top_occupations$Congruence[i]
    
    facet_details <- facet_details_list[[code]]
    if (is.null(facet_details)) next
    
    strengths <- facet_details %>% filter(interpretation == "above (strength)") %>% pull(facet)
    dev_areas <- facet_details %>% filter(interpretation == "below (development area)") %>% pull(facet)
    
    prompt <- build_prompt(occ_name, congruence, facet_details, strengths, dev_areas)
    
    cat(glue::glue("\n--- Report for {occ_name} ---\n"))
    cat(glue::glue("Prompt length: {nchar(prompt)} characters\n"))
    
    if (api_key != "" && !is.null(api_key) && api_key != "YOUR_GEMINI_API_KEY_HERE") {
      tryCatch({
        narrative <- generate_ai_narrative(prompt, api_key, api_url, model)
        cat("\n", strrep("─", 50), "\n")
        cat("📝 AI-GENERATED REPORT:\n")
        cat(strrep("─", 50), "\n")
        cat(narrative, "\n")
        cat(strrep("─", 50), "\n")
        
        write(paste(strrep("═", 70), "\nOCCUPATION: ", occ_name, "\nCONGRUENCE: ", round(congruence, 3), "\n", strrep("═", 70), "\n\n", narrative, "\n\n", strrep("═", 70), "\n", sep=""), 
              file = file.path(reports_folder, "counsellor_reports_full.txt"), append = TRUE)
        
        pdf_file <- file.path(reports_folder, paste0("report_", gsub(" ", "_", occ_name), ".pdf"))
        save_narrative_as_pdf(narrative, occ_name, congruence, pdf_file)
        cat(glue("📄 PDF report saved to: {pdf_file}\n"))
        
      }, error = function(e) {
        cat("⚠️ AI generation failed. Using fallback template.\n")
        cat(glue::glue("
        Based on your profile, you show a strong alignment with the {occ_name} role (congruence {round(congruence, 3)}).
        Your key strengths include {paste(strengths, collapse=', ')}.
        Areas to develop include {paste(dev_areas, collapse=', ')}.
        Your overall profile suggests a promising fit.
        "))
      })
    } else {
      cat("⚠️ No valid API key provided. Using fallback template.\n")
      cat(glue::glue("
      Based on your profile, you show a strong alignment with the {occ_name} role (congruence {round(congruence, 3)}).
      Your key strengths include {paste(strengths, collapse=', ')}.
      Areas to develop include {paste(dev_areas, collapse=', ')}.
      Your overall profile suggests a promising fit.
      "))
    }
    cat("\n", strrep("─", 50), "\n")
  }
}

# ---- 17. FINISH ----
cat("\n", strrep("═", 70), "\n")
cat("✅ Pipeline complete. Check your folders for output files.\n")
cat(glue("   Graphs: {graphs_folder}\n"))
cat(glue("   Reports: {reports_folder}\n"))
cat(strrep("═", 70), "\n")