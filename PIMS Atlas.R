# =============================================================================
# PIMS ATLAS - FULL PIPELINE WITH DIAGNOSTIC MODE, JOB ZONE FILTER, AND RADAR CHART
# (Consolidated Mapping Files, Norm Groups, Radar Visualisation with Zero Axis)
# Developed by Gary C Townsend (2026) v1.0 Publication Model
# FIX A: Radar chart limited to Top Match Only.
#        Core motivations now display contributing Work Style(s) in parentheses.
# FIX B: Radar chart title now includes the Top Match cosine value.
# FIX C: Profile legend moved to top of graph, below the Impact Threshold line.
# FIX D: Legend entries separated by "|" and perfectly centred via strwidth().
# FIX E: Radar chart outer facet labels no longer overlap adjacent panels
#        or each other. Achieved by enlarging the radar matrix "max" value
#        and giving the radar panel extra height + margins.
#
# -----------------------------------------------------------------------------
# INSTRUCTIONS FOR OTHER RESEARCHERS:
# -----------------------------------------------------------------------------
# 1. Place the entire "R-Folders" directory inside your "Documents" folder
#    (It works whether it's in OneDrive, Box Sync, or just plain Documents).
# 2. Ensure your WorkStyles subfolder contains your .xlsx files. The code will
#    automatically find them as long as they START with the correct name
#    (e.g., "Attention to Detail v1.0.xlsx" or "Attention to Detail v2.1.xlsx").
# 3. Ensure your "UserScores" folder contains a file named "user_tpq_scores.csv"
#    (this is the DEFAULT research-based profile that will be preferred).
#    Other files (e.g., "Portiauser_tpq_scores.csv") will be ignored unless
#    you rename them to "user_tpq_scores.csv" before running.
# 4. Ensure "FacetMappingMatrix/PIMS Facet Mapping Matrix*.xlsx" and
#    "MotivationalAnchors/PIMS Motivational Anchors*.xlsx" exist.
# 5. [OPTIONAL] Enter your Gemini API key in Section 13 to enable AI reports.
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
find_r_folders <- function() {
  candidate_paths <- c(
    file.path(Sys.getenv("USERPROFILE"), "Documents", "Box Sync Laptop", "Emeris", "Research", "Publication", "R-Folders"),
    file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documents", "Box Sync Laptop", "Emeris", "Research", "Publication", "R-Folders"),
    file.path(Sys.getenv("USERPROFILE"), "Documents", "R-Folders"),
    file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documents", "R-Folders"),
    file.path(Sys.getenv("USERPROFILE"), "Box", "R-Folders")
  )
  
  for (p in candidate_paths) {
    if (dir.exists(p)) {
      return(normalizePath(p, winslash = "/"))
    }
  }
  
  docs <- file.path(Sys.getenv("USERPROFILE"), "Documents")
  if (dir.exists(docs)) {
    found <- list.dirs(docs, recursive = TRUE, full.names = TRUE)
    found <- found[basename(found) == "R-Folders"]
    if (length(found) > 0) return(normalizePath(found[1], winslash = "/"))
  }
  
  stop("Could not automatically locate the 'R-Folders' directory. Please manually set the 'work_styles_folder' variable in Section 1.")
}

work_styles_folder <- find_r_folders()
cat(glue("📁 Detected R-Folders at: {work_styles_folder}\n"))

find_subfolder <- function(parent, name) {
  subs <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
  match <- subs[tolower(basename(subs)) == tolower(name)]
  if (length(match) > 0) return(match[1])
  return(file.path(parent, name))
}

norm_groups_folder <- find_subfolder(work_styles_folder, "NormGroups")
graphs_folder      <- find_subfolder(work_styles_folder, "Graphs")
reports_folder     <- find_subfolder(work_styles_folder, "Reports")
userscores_folder  <- find_subfolder(work_styles_folder, "UserScores")
work_styles_subfolder <- "WorkStyles"

for (f in c(graphs_folder, reports_folder)) {
  if (!dir.exists(f)) dir.create(f, recursive = TRUE)
}

min_job_zone <- 3
min_impact_threshold <- 90
congruence_threshold <- 0.2
top_n <- 3
z_diff_threshold <- 0.5

work_style_base_names <- c(
  "Attention to Detail", "Cautiousness", "Dependability",
  "Integrity", "Self-Control", "Stress Tolerance",
  "Cooperation", "Empathy", "Humility", "Optimism",
  "Sincerity", "Social Orientation", "Achievement Orientation",
  "Adaptability", "Initiative", "Innovation",
  "Intellectual Curiosity", "Leadership Orientation",
  "Perseverance", "Self-Confidence", "Tolerance for Ambiguity"
)

mapping_file_fragment <- "PIMS Facet Mapping Matrix"
anchors_file_fragment <- "PIMS Motivational Anchors"

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

TABLE1_MEANS <- c(
  Thrill.seeking = 2.787, Contracted = 2.251, Sociable = 2.487,
  Assertive = 2.899, Unguarded = 2.613, Engaging = 2.817,
  Compassionate = 2.824, Empathic = 2.504, Altruistic = 2.581,
  Collaborative = 2.763, Modest = 3.147, Candid = 2.569,
  Resilient = 2.786, Self.controlled = 2.372, Relaxed = 2.697,
  Optimistic = 2.591, Tempered = 2.393, Impervious = 2.565,
  Directed = 2.449, Competent = 3.075, Structured = 2.651,
  Self.disciplined = 2.536, Dutiful = 2.689, Cautious = 2.731,
  Creative = 2.757, Unconventional = 2.303, Complex = 2.839,
  Conceptual = 2.672, Conversant = 2.801, Composed = 2.270
)

TABLE1_SDS <- c(
  Thrill.seeking = 0.425, Contracted = 0.512, Sociable = 0.592,
  Assertive = 0.535, Unguarded = 0.420, Engaging = 0.452,
  Compassionate = 0.564, Empathic = 0.489, Altruistic = 0.499,
  Collaborative = 0.475, Modest = 0.403, Candid = 0.456,
  Resilient = 0.556, Self.controlled = 0.485, Relaxed = 0.551,
  Optimistic = 0.514, Tempered = 0.546, Impervious = 0.516,
  Directed = 0.458, Competent = 0.486, Structured = 0.580,
  Self.disciplined = 0.617, Dutiful = 0.435, Cautious = 0.461,
  Creative = 0.485, Unconventional = 0.425, Complex = 0.427,
  Conceptual = 0.449, Conversant = 0.525, Composed = 0.520
)

cat("\n📂 Verifying paths...\n")
cat(glue("  Work Styles folder exists: {dir.exists(work_styles_folder)}\n"))
cat(glue("  Norm Groups folder exists: {dir.exists(norm_groups_folder)}\n"))
cat(glue("  User Scores folder exists: {dir.exists(userscores_folder)}\n"))

# ---- 2. LOAD NORMATIVE DATA ----
load_norm_data <- function(norm_groups_folder, facet_names) {
  cat("\n📄 Loading normative data from:", norm_groups_folder, "\n")
  
  if (!dir.exists(norm_groups_folder)) {
    cat("⚠️ Norm Groups folder not found. Using hardcoded Table 1 stats.\n")
    return(NULL)
  }
  
  all_files <- list.files(norm_groups_folder, full.names = TRUE)
  all_files <- all_files[!grepl("~\\$", basename(all_files))]
  csv_files <- all_files[grepl("\\.csv$", all_files, ignore.case = TRUE)]
  excel_files <- all_files[grepl("\\.xlsx$", all_files, ignore.case = TRUE)]
  double_files <- all_files[grepl("\\.csv\\.xlsx$", all_files, ignore.case = TRUE)]
  all_data_files <- unique(c(csv_files, excel_files, double_files))
  
  if (length(all_data_files) == 0) {
    cat("⚠️ No data files found. Using hardcoded Table 1 stats.\n")
    return(NULL)
  }
  
  cat(glue("📂 Found {length(all_data_files)} file(s):\n"))
  for (f in all_data_files) cat(glue("  - {basename(f)}\n"))
  
  for (file_path in all_data_files) {
    tryCatch({
      df <- NULL
      file_lower <- tolower(file_path)
      
      if (grepl("\\.csv$", file_lower)) {
        df <- read.csv(file_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
        cat(glue("  📄 Read CSV: '{basename(file_path)}'..."))
      } else if (grepl("\\.xlsx$", file_lower)) {
        cat(glue("  📊 Reading Excel: '{basename(file_path)}'..."))
        sheet_names <- excel_sheets(file_path)
        cat(glue(" sheets: {paste(sheet_names, collapse=', ')}"))
        
        for (sheet in sheet_names) {
          test_df <- read_xlsx(file_path, sheet = sheet, n_max = 5)
          col_lower <- tolower(colnames(test_df))
          if (any(grepl("facet", col_lower)) && 
              any(grepl("mean|µ|μ|mu", col_lower)) && 
              any(grepl("sd|std|σ|sigma", col_lower))) {
            df <- read_xlsx(file_path, sheet = sheet)
            cat(glue(" loaded from sheet '{sheet}'"))
            break
          }
        }
        
        if (is.null(df)) {
          df <- read_xlsx(file_path, sheet = sheet_names[1])
          cat(glue(" using first sheet '{sheet_names[1]}'"))
        }
      } else {
        cat(glue("  ⚠️ Unsupported file type: '{basename(file_path)}'. Skipping.\n"))
        next
      }
      
      if (is.null(df) || nrow(df) == 0) {
        cat(" ⚠️ No data. Skipping.\n")
        next
      }
      
      colnames(df) <- trimws(colnames(df))
      colnames(df) <- gsub("\\.", "-", colnames(df))
      col_lower <- tolower(colnames(df))
      
      facet_idx <- which(grepl("facet", col_lower, ignore.case = TRUE))[1]
      mean_idx <- which(grepl("mean|µ|μ|mu", col_lower))[1]
      sd_idx <- which(grepl("sd|std|σ|sigma", col_lower, ignore.case = TRUE))[1]
      
      if (is.na(facet_idx) || is.na(mean_idx) || is.na(sd_idx)) {
        cat(" ⚠️ Could not find Facet, mean, and sd columns. Skipping.\n")
        cat(glue("     Found columns: {paste(colnames(df), collapse=', ')}\n"))
        next
      }
      
      norm_data <- data.frame(
        facet = as.character(df[[facet_idx]]),
        mean = as.numeric(df[[mean_idx]]),
        sd = as.numeric(df[[sd_idx]]),
        stringsAsFactors = FALSE
      )
      
      norm_data <- norm_data[!is.na(norm_data$facet) & !is.na(norm_data$mean) & !is.na(norm_data$sd), ]
      
      if (nrow(norm_data) == 0) {
        cat(" ⚠️ No valid rows. Skipping.\n")
        next
      }
      
      present <- intersect(facet_names, norm_data$facet)
      if (length(present) < 28) {
        cat(glue(" ⚠️ Only {length(present)} of {length(facet_names)} facets found. Skipping.\n"))
        next
      }
      
      norm_data <- norm_data[match(facet_names, norm_data$facet), ]
      norm_data <- norm_data[!is.na(norm_data$facet), ]
      
      cat(glue(" ✅ Loaded {nrow(norm_data)} facets\n"))
      return(norm_data)
      
    }, error = function(e) {
      cat(glue("  ❌ Error reading '{basename(file_path)}': {e$message}\n"))
    })
  }
  
  cat("⚠️ No valid data loaded. Using hardcoded Table 1 stats.\n")
  return(NULL)
}

norm_stats <- load_norm_data(norm_groups_folder, facet_names)

if (!is.null(norm_stats) && nrow(norm_stats) > 0) {
  training_means <- norm_stats$mean
  training_sds <- norm_stats$sd
  names(training_means) <- names(training_sds) <- norm_stats$facet
  
  for (f in facet_names) {
    if (is.na(training_means[f]) || is.na(training_sds[f])) {
      training_means[f] <- TABLE1_MEANS[f]
      training_sds[f] <- TABLE1_SDS[f]
      cat(glue("  ⚠️ Filled missing {f} with Table 1 values\n"))
    }
  }
  
  cat("✅ Normative statistics loaded from file.\n")
} else {
  cat("✅ Using hardcoded Table 1 statistics.\n")
  training_means <- TABLE1_MEANS
  training_sds <- TABLE1_SDS
}

names(training_means) <- names(training_sds) <- facet_names

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

find_matching_file <- function(folder_path, base_name, prefer_most_recent = TRUE) {
  if (!dir.exists(folder_path)) return(NULL)
  all_files <- list.files(folder_path, full.names = TRUE)
  all_files <- all_files[!grepl("^~\\$", basename(all_files))]
  pattern <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", base_name))
  matches <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE) & 
                         grepl("\\.(xlsx|csv)$", all_files, ignore.case = TRUE)]
  
  if (length(matches) == 0) return(NULL)
  
  if (length(matches) > 1 && prefer_most_recent) {
    file_times <- file.info(matches)$mtime
    matches <- matches[order(file_times, decreasing = TRUE)]
    cat(glue("  ⚠️ Multiple matches for '{base_name}'. Using most recent: {basename(matches[1])}\n"))
    for (m in matches) {
      cat(glue("     - {basename(m)} (modified: {file.info(m)$mtime})\n"))
    }
  }
  
  return(matches[1])
}

# ---- 4. LOAD ALL WORK STYLES ----
load_all_work_styles <- function(folder_path, work_styles_subfolder, base_names) {
  full_path <- file.path(folder_path, work_styles_subfolder)
  
  if (!dir.exists(full_path)) {
    subs <- list.dirs(folder_path, recursive = FALSE)
    match <- subs[tolower(basename(subs)) == tolower(work_styles_subfolder)]
    if (length(match) > 0) {
      full_path <- match[1]
    } else {
      stop(glue("WorkStyles subfolder not found in: {folder_path}"))
    }
  }
  
  cat(glue("📂 Searching for Work Style files in: {full_path}\n"))
  
  all_data <- list()
  found_count <- 0
  missing_names <- c()
  
  for (base_name in base_names) {
    file_path <- find_matching_file(full_path, base_name)
    
    if (is.null(file_path)) {
      missing_names <- c(missing_names, base_name)
      next
    }
    
    file_name <- basename(file_path)
    cat(glue("\n  Loading: '{file_name}'..."))
    found_count <- found_count + 1
    
    tryCatch({
      sheet_names <- excel_sheets(file_path)
      selected_sheet <- NULL
      df <- NULL
      
      for (s in sheet_names) {
        df_test <- read_xlsx(file_path, sheet = s, n_max = 5)
        clean_cols <- trimws(colnames(df_test))
        clean_cols <- gsub("[^[:print:]]", "", clean_cols)
        clean_cols <- gsub("\\u00A0", " ", clean_cols)
        
        has_fixed <- all(tolower(fixed_columns) %in% tolower(clean_cols))
        has_facets <- all(facet_names %in% clean_cols)
        
        if (has_fixed && has_facets) {
          selected_sheet <- s
          df <- read_xlsx(file_path, sheet = s)
          break
        }
      }
      
      if (is.null(selected_sheet)) {
        cat("\n  ❌ No sheet contained all required columns.\n")
        cat("  Required fixed columns:", paste(fixed_columns, collapse=", "), "\n")
        cat("  --- Sheets and columns found ---\n")
        for (s in sheet_names) {
          df_test <- read_xlsx(file_path, sheet = s, n_max = 1)
          cat(glue("    Sheet '{s}': {paste(head(colnames(df_test), 10), collapse=', ')}...\n"))
        }
        next
      }
      
      colnames(df) <- trimws(colnames(df))
      colnames(df) <- gsub("[^[:print:]]", "", colnames(df))
      colnames(df) <- gsub("\\u00A0", " ", colnames(df))
      
      for (fc in fixed_columns) {
        actual_col <- colnames(df)[tolower(colnames(df)) == tolower(fc)]
        if (length(actual_col) > 0) {
          colnames(df)[colnames(df) == actual_col[1]] <- fc
        }
      }
      
      df <- df %>% select(all_of(required_work_columns))
      df$Impact <- as.numeric(df$Impact)
      
      if (nrow(df) == 0 || all(is.na(df$Impact))) { 
        cat("❌ No valid Impact data.\n"); 
        next 
      }
      
      df$Work_Style <- base_name
      cat(glue(" read {nrow(df)} rows. ✅"))
      all_data[[base_name]] <- df
      
    }, error = function(e) {
      cat(glue("❌ ERROR: {e$message}\n"))
    })
  }
  
  cat(glue("\n\n📊 Summary: Found {found_count}/{length(base_names)} Work Style files.\n"))
  if (length(missing_names) > 0) {
    cat("⚠️ Missing files (no match found):\n")
    for (m in missing_names) cat(glue("   - {m} (expected any .xlsx starting with this name)\n"))
  }
  
  if (length(all_data) == 0) {
    stop("No valid Work Style files loaded. Please check that your Excel files contain the required columns.")
  }
  
  combined <- bind_rows(all_data)
  cat(glue("✅ Combined: {nrow(combined)} rows from {length(all_data)} files.\n"))
  return(combined)
}

# ---- 5. LOAD MAPPING DATA ----
cat("\n📄 Loading mapping file...\n")
mapping_path <- find_matching_file(file.path(work_styles_folder, "FacetMappingMatrix"), mapping_file_fragment)
if (is.null(mapping_path)) {
  mapping_path <- find_matching_file(work_styles_folder, mapping_file_fragment)
}
if (is.null(mapping_path)) {
  stop("Could not find a file starting with 'PIMS Facet Mapping Matrix' in the FacetMappingMatrix subfolder or R-Folders root.")
}
cat(glue("  Found: {basename(mapping_path)}\n"))

expected_mapping_cols <- c(
  "O*NET Broad Work Style Element",
  "O*NET Work Style Element", 
  "PIMS Motivation Element",
  "TPQ Required Facet",
  "Motivation Symbol",
  "TPQ Facet Role",
  "TPQ Facet Psychological Function"
)
mapping_data <- read_xlsx(mapping_path)
colnames(mapping_data) <- trimws(colnames(mapping_data))
missing <- setdiff(expected_mapping_cols, colnames(mapping_data))
if (length(missing) > 0) {
  cat("❌ Column mismatch in mapping file.\n")
  cat("Expected columns:\n")
  for (col in expected_mapping_cols) cat(glue("  - {col}\n"))
  cat("Found columns:\n")
  for (col in colnames(mapping_data)) cat(glue("  - {col}\n"))
  stop("Column mismatch in mapping file.")
}
mapping_data <- mapping_data %>%
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>%
  mutate(across(where(is.character), ~ trimws(as.character(.)))) %>%
  distinct() %>%
  filter(!if_all(where(is.character), ~ is.na(.) | . == ""))
cat(glue("✅ Mapping data loaded with {nrow(mapping_data)} rows\n"))

# ---- 6. LOAD ANCHORS DATA ----
cat("\n📄 Loading PIMS Motivational Anchors...\n")
anchors_path <- find_matching_file(file.path(work_styles_folder, "MotivationalAnchors"), anchors_file_fragment)
if (is.null(anchors_path)) {
  anchors_path <- find_matching_file(work_styles_folder, anchors_file_fragment)
}
if (is.null(anchors_path)) {
  stop("Could not find a file starting with 'PIMS Motivational Anchors' in the MotivationalAnchors subfolder or R-Folders root.")
}
cat(glue("  Found: {basename(anchors_path)}\n"))

expected_anchors_cols <- c(
  "O*NET Broad Work Style Element",
  "O*NET Work Style Element",
  "Primary Motivator",
  "PIMS Motivation Element",
  "PIMS Motivation Intrinsic Driver (The \"How\" it feels)",
  "PIMS Motivation Extrinsic Driver (The \"What\" it gains)"
)
anchors_data <- read_xlsx(anchors_path)
colnames(anchors_data) <- trimws(colnames(anchors_data))
missing <- setdiff(expected_anchors_cols, colnames(anchors_data))
if (length(missing) > 0) {
  cat("❌ Column mismatch in anchors file.\n")
  cat("Expected columns:\n")
  for (col in expected_anchors_cols) cat(glue("  - {col}\n"))
  cat("Found columns:\n")
  for (col in colnames(anchors_data)) cat(glue("  - {col}\n"))
  stop("Column mismatch in anchors file.")
}
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
cat("\n", strrep("─", 60), "\n")
cat("👤 LOADING USER SCORES\n")
cat(strrep("─", 60), "\n")
cat(glue("📂 UserScores folder: {userscores_folder}\n"))

if (dir.exists(userscores_folder)) {
  all_user_files <- list.files(userscores_folder, full.names = TRUE)
  all_user_files <- all_user_files[!grepl("^~\\$", basename(all_user_files))]
  if (length(all_user_files) > 0) {
    cat("📄 Files found in UserScores folder:\n")
    for (f in all_user_files) {
      cat(glue("   - {basename(f)} | {file.size(f)} bytes | Modified: {file.info(f)$mtime}\n"))
    }
  } else {
    cat("⚠️ UserScores folder is empty.\n")
  }
} else {
  cat("⚠️ UserScores folder does not exist.\n")
}

user_file <- NULL
candidate_patterns <- c("user_tpq_scores", "UserScores", "user_tpq")

for (pattern in candidate_patterns) {
  f <- find_matching_file(userscores_folder, pattern)
  if (!is.null(f)) {
    user_file <- f
    cat(glue("\n✅ Matched pattern '{pattern}' to file: {basename(f)}\n"))
    break
  }
}

if (is.null(user_file)) {
  cat("\n⚠️ No file matched standard patterns. Attempting auto-selection...\n")
  all_csvs <- list.files(userscores_folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  all_csvs <- all_csvs[!grepl("^~\\$", basename(all_csvs))]
  
  if (length(all_csvs) > 0) {
    exact_match <- all_csvs[basename(all_csvs) == "user_tpq_scores.csv"]
    if (length(exact_match) > 0) {
      user_file <- exact_match[1]
      cat(glue("   Auto-selected (exact match): {basename(user_file)}\n"))
    } else {
      all_csvs <- all_csvs[order(file.info(all_csvs)$mtime, decreasing = TRUE)]
      user_file <- all_csvs[1]
      cat(glue("   Auto-selected (most recent CSV): {basename(user_file)}\n"))
    }
  }
}

if (!is.null(user_file)) {
  cat(glue("\n✅ Loading user scores from: {basename(user_file)}\n"))
  cat(glue("   Full path: {user_file}\n"))
  cat(glue("   Last modified: {file.info(user_file)$mtime}\n\n"))
  
  if (grepl("\\.csv$", user_file, ignore.case = TRUE)) {
    user_data <- read.csv(user_file, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  } else {
    user_data <- read_xlsx(user_file)
  }
  
  colnames(user_data) <- gsub("\\.", "-", colnames(user_data))
  colnames(user_data) <- trimws(colnames(user_data))
  
  present <- intersect(facet_names, colnames(user_data))
  missing <- setdiff(facet_names, present)
  if (length(missing) > 0) cat("⚠️ Missing columns in UserScores file: ", paste(missing, collapse=", "), "\n")
  
  user_raw <- numeric(length(facet_names))
  names(user_raw) <- facet_names
  user_raw[] <- 2.5
  
  for (f in present) {
    val <- as.numeric(user_data[1, f])
    if (!is.na(val)) user_raw[f] <- val
  }
  
  cat("\n📊 Raw user values loaded from file (first 10 facets):\n")
  print(round(user_raw[1:10], 3))
  cat("\n📊 Any facets equal to default (2.5)? ", sum(user_raw == 2.5), " of ", length(user_raw), "\n")
  if (sum(user_raw == 2.5) > 5) {
    cat("⚠️ WARNING: Many facets are at the default 2.5 value.\n")
    cat("   This suggests your column names may not match the expected facet names.\n")
    cat("   Expected facet names (first 10):\n")
    cat("   ", paste(facet_names[1:10], collapse=", "), "...\n")
    cat("   Columns found in your file (first 10):\n")
    cat("   ", paste(colnames(user_data)[1:min(10, ncol(user_data))], collapse=", "), "...\n")
  }
} else {
  cat("\n⚠️ No UserScores file found. Using manual test profile.\n")
  cat("   Expected: A .csv or .xlsx file in the UserScores folder.\n")
  user_raw <- c(2.5,2.3,2.5,3.5,3.0,3.5,3.8,3.8,2.5,3.0,
                4.0,2.5,4.0,3.8,2.5,2.3,3.0,3.0,3.5,4.0,
                3.8,4.0,3.8,4.0,3.8,2.8,3.3,4.0,4.0,2.3)
  names(user_raw) <- facet_names
}
user_raw <- pmax(1, pmin(4, user_raw))
user_z <- standardize_user(user_raw)

cat("\n", strrep("─", 60), "\n")

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

occupation_data_raw <- load_all_work_styles(work_styles_folder, work_styles_subfolder, work_style_base_names)
occupation_data <- occupation_data_raw %>% filter(`Job Zone` >= min_job_zone)
cat(glue("\n📊 Job Zone filter applied: kept {n_distinct(occupation_data$Code)} occupations.\n"))

atlas <- build_full_atlas(occupation_data, training_means, training_sds, min_impact_threshold)
ranked_results <- match_user_to_atlas(user_z, atlas)

if (nrow(ranked_results) == 0) {
  cat("\n⚠️ No occupations matched. Exiting.\n")
  stop("No matches found. Check filters and data.")
}

# ---- 12. DEFINE TRAIT CLUSTERS ----
clusters <- list(
  Conscientious_Rule_Oriented = c(
    "Structured", "Self-disciplined", "Dutiful", "Cautious", 
    "Creative", "Thrill-seeking", "Unconventional", "Tempered", 
    "Resilient", "Directed", "Optimistic", "Candid", 
    "Altruistic", "Relaxed", "Assertive", "Unguarded", "Empathic"
  ),
  Emotionally_Resilient = c(
    "Self-controlled", "Tempered", "Composed", "Assertive", 
    "Relaxed", "Thrill-seeking", "Resilient", "Directed", 
    "Optimistic", "Impervious", "Dutiful"
  ),
  Interpersonally_Oriented = c(
    "Collaborative", "Compassionate", "Modest", "Sociable", 
    "Assertive", "Self-disciplined", "Engaging", "Empathic", 
    "Altruistic", "Candid", "Unguarded", "Relaxed", "Competent", 
    "Optimistic", "Resilient", "Contracted"
  ),
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
# ⚠️ ENTER YOUR GEMINI API KEY BELOW ⚠️
api_key <- ""  # <-- PASTE YOUR API KEY HERE
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
  cat("\n", strrep("═", 70), "\n")
  cat("✅ NORMAL MODE: Positive career matches found (max congruence =", round(max_congruence, 3), ").\n")
  cat(strrep("═", 70), "\n\n")
  
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
  
  # ---- 16c. RADAR CHART (FIX A + B + C + D + E) ----
  if (length(occ_vectors) > 0) {
    cat("\n📊 Generating radar chart for top 1 occupation (Fix A + B + C + D + E)...\n")
    
    all_facets <- facet_names
    
    top_code <- names(occ_vectors)[1]
    top_occ_df <- occ_vectors[[top_code]]
    top_occ_name <- top_occupations %>% filter(Code == top_code) %>% pull(Occupation)
    top_congruence <- top_occupations %>% filter(Code == top_code) %>% pull(Congruence)
    
    user_profile <- numeric(length(all_facets))
    names(user_profile) <- all_facets
    for (i in seq_along(all_facets)) {
      f <- all_facets[i]
      is_relevant <- f %in% top_occ_df$facet[top_occ_df$weight > 0]
      if (is_relevant) {
        val <- user_z[f]
        if (is.na(val) || !is.numeric(val)) val <- 0
        user_profile[i] <- val
      } else {
        user_profile[i] <- 0
      }
    }
    
    occ_profile <- numeric(length(all_facets))
    names(occ_profile) <- all_facets
    for (i in seq_along(all_facets)) {
      f <- all_facets[i]
      row <- top_occ_df[top_occ_df$facet == f, ]
      if (nrow(row) > 0 && !is.na(row$weight) && row$weight > 0) {
        val <- row$demand_z
        if (is.na(val) || !is.numeric(val)) val <- 0
        occ_profile[i] <- val
      } else {
        occ_profile[i] <- 0
      }
    }
    
    occ <- atlas[[top_code]]
    considered_motiv <- get_motivation_elements_from_styles(occ$considered_workstyles, mapping_data, anchors_data)
    core_motiv <- get_motivation_elements_from_styles(occ$used_workstyles, mapping_data, anchors_data)
    considered_motivs_unique <- unique(considered_motiv)
    core_motivs_unique <- unique(core_motiv)
    
    ws_to_motiv <- mapping_data %>%
      select(`O*NET Work Style Element`, `PIMS Motivation Element`) %>%
      distinct()
    
    anchors_ws_to_motiv <- anchors_data %>%
      select(`O*NET Work Style Element`, `PIMS Motivation Element`) %>%
      distinct()
    
    style_to_motiv <- bind_rows(ws_to_motiv, anchors_ws_to_motiv) %>%
      distinct() %>%
      filter(!is.na(`PIMS Motivation Element`) & `PIMS Motivation Element` != "")
    
    used_styles <- occ$used_workstyles
    
    motiv_to_styles <- list()
    for (style in used_styles) {
      motiv_row <- style_to_motiv %>% filter(`O*NET Work Style Element` == style)
      if (nrow(motiv_row) > 0) {
        motiv <- motiv_row$`PIMS Motivation Element`[1]
        if (!is.null(motiv) && motiv != "" && motiv != "Not specified") {
          if (!motiv %in% names(motiv_to_styles)) {
            motiv_to_styles[[motiv]] <- c()
          }
          motiv_to_styles[[motiv]] <- c(motiv_to_styles[[motiv]], style)
        }
      }
    }
    
    core_parts <- c()
    for (motiv in core_motivs_unique) {
      styles_for_motiv <- motiv_to_styles[[motiv]]
      if (length(styles_for_motiv) > 0) {
        core_parts <- c(core_parts, paste0(motiv, " (", paste(styles_for_motiv, collapse = ", "), ")"))
      } else {
        core_parts <- c(core_parts, motiv)
      }
    }
    core_title_text <- paste(core_parts, collapse = ", ")
    
    SHIFT <- 3
    user_shifted <- user_profile + SHIFT
    occ_shifted <- occ_profile + SHIFT
    
    radar_matrix <- matrix(0, nrow = 4, ncol = length(all_facets))
    colnames(radar_matrix) <- all_facets
    rownames(radar_matrix) <- c(
      "max", 
      "min", 
      "User Profile", 
      paste0(top_occ_name, " (", round(top_congruence, 3), ")")
    )
    
    # FIX E: enlarged "max" pulls the outer ring inward so the built-in
    # facet labels drawn by radarchart() sit inside the panel and don't
    # overlap the panels above and below.
    radar_matrix[1, ] <- 8.0
    radar_matrix[2, ] <- 0.0
    radar_matrix[3, ] <- user_shifted
    radar_matrix[4, ] <- occ_shifted
    
    radar_df <- as.data.frame(radar_matrix)
    for (col in colnames(radar_df)) {
      radar_df[[col]] <- as.numeric(radar_df[[col]])
    }
    
    colors <- c("#2E86C1", "#E74C3C")
    
    draw_radar <- function() {
      # Layout (top to bottom):
      #   1. Title Panel
      #   2. Subtitle Panel
      #   3. Legend Panel
      #   4. Radar Chart Panel (taller so crowded labels have room)
      #   5. Footer Panel
      layout(matrix(c(1, 2, 3, 4, 5), nrow = 5),
             heights = c(1.2, 0.7, 0.7, 12, 3.0))
      par(mar = c(0, 0, 0, 0))
      
      # ---- Panel 1: Title ----
      plot.new()
      if (core_title_text == "" || core_title_text == "NULL") core_title_text <- "No core motivations identified"
      title_with_cosine <- paste0(
        "Core Motivations: ", core_title_text,
        "  [Cosine: ", sprintf("%.4f", top_congruence), "]"
      )
      text(0.5, 0.5, title_with_cosine, 
           cex = 2.0, font = 2, col = "black")
      
      # ---- Panel 2: Subtitle ----
      plot.new()
      subtitle_text <- glue("Impact threshold: {min_impact_threshold} | Job Zone: ≥ {min_job_zone} | Zero axis = centre ring | Top Match Only")
      text(0.5, 0.5, subtitle_text, cex = 1.4, col = "black")
      
      # ---- Panel 3: Legend ----
      plot.new()
      legend_items <- rownames(radar_df)[-c(1, 2)]
      
      if (length(legend_items) == 2) {
        cex_val <- 1.3
        dot_cex <- 2.0
        gap_dot_to_label <- 0.008
        gap_label_to_sep <- 0.025
        
        w1 <- strwidth(legend_items[1], cex = cex_val, units = "user")
        w2 <- strwidth(legend_items[2], cex = cex_val, units = "user")
        
        sep_x   <- 0.5
        y_ctr   <- 0.5
        
        label1_right <- sep_x - gap_label_to_sep
        label1_left  <- label1_right - w1
        dot1_x       <- label1_left - gap_dot_to_label
        
        dot2_x        <- sep_x + gap_label_to_sep
        label2_left   <- dot2_x + gap_dot_to_label
        label2_right  <- label2_left + w2
        
        points(dot1_x, y_ctr, pch = 20, col = colors[1], cex = dot_cex)
        text(label1_left, y_ctr, legend_items[1],
             cex = cex_val, col = "black", adj = c(0, 0.5))
        
        text(sep_x, y_ctr, "|", cex = 1.7, col = "grey40")
        
        points(dot2_x, y_ctr, pch = 20, col = colors[2], cex = dot_cex)
        text(label2_left, y_ctr, legend_items[2],
             cex = cex_val, col = "black", adj = c(0, 0.5))
        
      } else {
        legend(
          x = 0.5, y = 0.5,
          legend = legend_items,
          bty = "n",
          pch = 20,
          col = colors,
          text.col = "black",
          cex = 1.3,
          pt.cex = 2.0,
          horiz = TRUE,
          xjust = 0.5,
          yjust = 0.5
        )
      }
      
      # ---- Panel 4: Radar Chart (with built-in labels) ----
      # Slightly reduced margins + slightly reduced label size (vlcex)
      # so crowded facets at top and bottom have room without overlapping.
      par(mar = c(1, 1, 1, 1))
      
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
        vlcex = 0.95,                       # slightly smaller facet labels
        vlab = all_facets,
        caxislabels = c(-3, -2, 0, 1, 2, 3, 4),
        calcex = 1.2,
        title = ""
      )
      
      # ---- Panel 5: Footer ----
      par(mar = c(0, 0, 0, 0))
      plot.new()
      
      text(0.5, 0.85, "Considered Motivations (All Work Styles):", 
           font = 2, cex = 1.6, col = "black") 
      
      words <- considered_motivs_unique
      chunk_size <- ceiling(length(words) / 3)
      wrapped_considered <- c()
      for (i in seq(1, length(words), chunk_size)) {
        chunk <- words[i:min(i + chunk_size - 1, length(words))]
        wrapped_considered <- c(wrapped_considered, paste(chunk, collapse = ", "))
      }
      
      line_height <- 0.16
      start_y <- 0.65
      for (j in seq_along(wrapped_considered)) {
        y_pos <- start_y - (j - 1) * line_height
        if (y_pos > 0.05) {
          text(0.5, y_pos, wrapped_considered[j], cex = 1.4, col = "black") 
        }
      }
      
      layout(1)
      par(mar = c(5, 4, 4, 2) + 0.1)
    }
    
    draw_radar()
    
    # ---- Save PDF ----
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
    
    # ---- Save PNG ----
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

# ---- 17. VERIFICATION SECTION ----
cat("\n\n", strrep("═", 80), "\n")
cat("📊 PIMS ATLAS VERIFICATION REPORT: Top Match Worked Example\n")
cat(strrep("═", 80), "\n")
cat(glue("Timestamp: {Sys.time()}\n"))
cat(glue("Normative database: {ifelse(exists('norm_stats') && !is.null(norm_stats), 'Loaded from file', 'Table 1 hardcoded')}\n"))
cat(glue("User: {ifelse(exists('user_file') && !is.null(user_file), basename(user_file), 'Test Profile')}\n"))
cat(strrep("═", 80), "\n\n")

cat("⚙️  CONFIGURATION\n")
cat(strrep("─", 60), "\n")
cat(glue("  Impact threshold: {min_impact_threshold}\n"))
cat(glue("  Job Zone filter: ≥ {min_job_zone}\n"))
cat(glue("  Congruence threshold: {congruence_threshold}\n"))
cat(glue("  Top N: {top_n}\n"))
cat(strrep("─", 60), "\n\n")

cat("📊 NORMATIVE STATISTICS (N = 850)\n")
cat(strrep("─", 60), "\n")

if (exists("training_means") && exists("training_sds")) {
  norm_table <- data.frame(
    Facet = names(training_means),
    μ = round(training_means, 3),
    σ = round(training_sds, 3)
  )
  print(norm_table, row.names = FALSE)
} else {
  cat("⚠️ Normative statistics not found.\n")
}
cat(strrep("─", 60), "\n\n")

cat("🏆 TOP MATCH IDENTIFICATION\n")
cat(strrep("─", 60), "\n")

if (exists("ranked_results") && nrow(ranked_results) > 0) {
  top_match <- ranked_results %>% slice_head(n = 1)
  top_code <- top_match$Code[1]
  top_occupation <- top_match$Occupation[1]
  top_congruence <- top_match$Congruence[1]
  
  cat(glue("  Top Match: {top_occupation} (Code: {top_code})\n"))
  cat(glue("  Congruence Score: {round(top_congruence, 4)}\n"))
  
  if (exists("atlas") && top_code %in% names(atlas)) {
    occ_vec <- atlas[[top_code]]
    cat(glue("  Total styles available: {occ_vec$total_styles_available}\n"))
    cat(glue("  Styles used (Impact ≥ {min_impact_threshold}): {occ_vec$styles_used}\n"))
    cat(glue("  Used styles: {paste(occ_vec$used_workstyles, collapse=', ')}\n"))
  } else {
    cat("⚠️ Occupation vector not found in atlas.\n")
  }
} else {
  cat("⚠️ No ranked results found.\n")
  stop("Cannot proceed with verification.")
}
cat(strrep("─", 60), "\n\n")

cat(glue("✈️  SOURCE DATA: {top_occupation} (Code: {top_code})\n"))
cat(strrep("─", 60), "\n")

if (exists("occupation_data")) {
  occ_raw <- occupation_data %>% filter(Code == top_code)
  
  if (nrow(occ_raw) > 0) {
    cat(glue("  Found {nrow(occ_raw)} rows for {top_occupation}\n\n"))
    
    occ_summary <- occ_raw %>%
      select(Work_Style, Impact, JobZone = `Job Zone`, Occupation) %>%
      distinct()
    cat("  All Work Style Elements:\n")
    print(occ_summary, row.names = FALSE)
    
    occ_selected <- occ_raw %>% filter(Impact >= min_impact_threshold)
    cat(glue("\n  Selected rows (Impact ≥ {min_impact_threshold}): {nrow(occ_selected)} rows\n"))
    selected_summary <- occ_selected %>%
      select(Work_Style, Impact) %>%
      distinct() %>%
      arrange(desc(Impact))
    print(selected_summary, row.names = FALSE)
    
    occ_excluded <- occ_raw %>% filter(Impact < min_impact_threshold)
    if (nrow(occ_excluded) > 0) {
      cat("\n  Excluded rows (Impact < {min_impact_threshold}):\n")
      excluded_summary <- occ_excluded %>%
        select(Work_Style, Impact) %>%
        distinct() %>%
        arrange(desc(Impact))
      print(excluded_summary, row.names = FALSE)
    }
    
    cat("\n  Symbol Matrix (Selected Rows):\n")
    symbol_cols <- intersect(facet_names, colnames(occ_selected))
    non_empty_cols <- c("Work_Style", "Impact")
    for (col in symbol_cols) {
      if (any(!is.na(occ_selected[[col]]) & occ_selected[[col]] != "" & occ_selected[[col]] != "0")) {
        non_empty_cols <- c(non_empty_cols, col)
      }
    }
    print(occ_selected[, non_empty_cols], row.names = FALSE)
    
  } else {
    cat(glue("❌ {top_occupation} (Code: {top_code}) not found.\n"))
  }
}
cat(strrep("─", 60), "\n\n")

cat("📊 STEP 3: IMPACT NORMALISATION\n")
cat(strrep("─", 60), "\n")

if (exists("occupation_data")) {
  occ_selected <- occupation_data %>% 
    filter(Code == top_code, Impact >= min_impact_threshold)
  
  if (nrow(occ_selected) > 0) {
    total_impact <- sum(occ_selected$Impact, na.rm = TRUE)
    cat(glue("  Total Impact = {paste(occ_selected$Impact, collapse=' + ')} = {total_impact}\n\n"))
    
    impact_table <- occ_selected %>%
      select(Work_Style, Impact) %>%
      distinct() %>%
      mutate(
        Normalised_Weight = round(Impact / total_impact, 4)
      ) %>%
      arrange(desc(Impact))
    
    cat("  Work Style | Impact | Normalised Weight\n")
    cat("  ", strrep("─", 45), "\n")
    for (i in 1:nrow(impact_table)) {
      cat(glue("  {impact_table$Work_Style[i]} | {impact_table$Impact[i]} | {impact_table$Normalised_Weight[i]}\n"))
    }
  }
}
cat(strrep("─", 60), "\n\n")

cat("📊 STEP 4: SYMBOL PARSING (±0.7σ RULE)\n")
cat(strrep("─", 60), "\n")

if (exists("occupation_data") && exists("training_means") && exists("training_sds")) {
  occ_selected <- occupation_data %>% 
    filter(Code == top_code, Impact >= min_impact_threshold)
  
  if (nrow(occ_selected) > 0) {
    cat("  Symbol-to-Demand Conversions:\n")
    for (f in facet_names) {
      symbols <- occ_selected[[f]]
      symbols <- symbols[!is.na(symbols) & symbols != "" & symbols != "0"]
      if (length(symbols) > 0) {
        mu <- training_means[f]
        sigma <- training_sds[f]
        for (sym in unique(symbols)) {
          demand <- switch(sym,
                           "↑" = mu + 0.7 * sigma,
                           "Δ" = mu + 0.7 * sigma,
                           "↓" = mu - 0.7 * sigma,
                           "Λ" = mu - 0.7 * sigma,
                           "↔" = mu,
                           NA
          )
          if (!is.na(demand)) {
            cat(glue("    {f} ({sym}): μ {ifelse(sym %in% c('↑','Δ'), '+', ifelse(sym %in% c('↓','Λ'), '-', ''))} 0.7σ = {round(demand, 3)}\n"))
          }
        }
      }
    }
  }
}
cat(strrep("─", 60), "\n\n")

cat("📊 STEP 5: FACET AGGREGATIONS (Demand_raw)\n")
cat(strrep("─", 60), "\n")

if (exists("atlas") && top_code %in% names(atlas)) {
  occ_vec <- atlas[[top_code]]
  used_styles <- occ_vec$used_workstyles
  
  cat(glue("  Total styles used: {length(used_styles)}\n"))
  cat(glue("  Styles: {paste(used_styles, collapse=', ')}\n\n"))
  
  facet_vector <- occ_vec$facet_vector
  relevant <- facet_vector %>% filter(weight > 0) %>% arrange(desc(weight))
  
  cat("  Aggregated Raw Demands (Demand_raw):\n")
  for (i in 1:nrow(relevant)) {
    f <- relevant$facet[i]
    dz <- relevant$demand_z[i]
    mu <- training_means[f]
    sigma <- training_sds[f]
    raw <- dz * sigma + mu
    cat(glue("    {f}: {round(raw, 3)}\n"))
  }
}
cat(strrep("─", 60), "\n\n")

cat("📊 STEP 6: ROLE-BASED WEIGHTING\n")
cat(strrep("─", 60), "\n")

if (exists("atlas") && top_code %in% names(atlas)) {
  occ_vec <- atlas[[top_code]]
  facet_vector <- occ_vec$facet_vector
  relevant <- facet_vector %>% filter(weight > 0) %>% arrange(desc(weight))
  
  cat("  Facet | Weight | Role\n")
  cat("  ", strrep("─", 40), "\n")
  for (i in 1:nrow(relevant)) {
    f <- relevant$facet[i]
    w <- relevant$weight[i]
    role <- if (w == 1.0) "Core" else if (w >= 0.5) "Secondary" else "Modulation"
    cat(glue("  {f} | {round(w, 2)} | {role}\n"))
  }
}
cat(strrep("─", 60), "\n\n")

cat("📊 STEP 7: DEMAND Z-SCORES (Standardisation)\n")
cat(strrep("─", 60), "\n")

if (exists("atlas") && top_code %in% names(atlas)) {
  occ_vec <- atlas[[top_code]]
  facet_vector <- occ_vec$facet_vector
  relevant <- facet_vector %>% filter(weight > 0) %>% arrange(desc(weight))
  
  cat("  Facet | Demand_z\n")
  cat("  ", strrep("─", 30), "\n")
  for (i in 1:nrow(relevant)) {
    f <- relevant$facet[i]
    dz <- relevant$demand_z[i]
    cat(glue("  {f} | {round(dz, 3)}\n"))
  }
}
cat(strrep("─", 60), "\n\n")

cat("👤 STEP 8: USER STANDARDISATION\n")
cat(strrep("─", 60), "\n")

if (exists("user_z") && exists("atlas") && top_code %in% names(atlas)) {
  occ_vec <- atlas[[top_code]]
  facet_vector <- occ_vec$facet_vector
  relevant <- facet_vector %>% filter(weight > 0)
  
  cat("  Relevant Facets (Top Match):\n")
  user_table <- data.frame(
    Facet = relevant$facet,
    Raw_Score = round(user_raw[relevant$facet], 1),
    μ = round(training_means[relevant$facet], 3),
    σ = round(training_sds[relevant$facet], 3),
    Z_user = round(user_z[relevant$facet], 3)
  )
  print(user_table, row.names = FALSE)
}
cat(strrep("─", 60), "\n\n")

cat("📊 STEP 9 & 10: WEIGHTED COSINE SIMILARITY\n")
cat(strrep("─", 60), "\n")

if (exists("atlas") && exists("user_z") && top_code %in% names(atlas)) {
  occ_vec <- atlas[[top_code]]
  facet_vector <- occ_vec$facet_vector
  relevant <- facet_vector %>% filter(weight > 0)
  
  a <- user_z[relevant$facet]
  b <- relevant$demand_z
  w <- relevant$weight
  
  cat("  Facet | Weight (w) | Demand Z (B) | User Z (A) | w·A·B | w·A² | w·B²\n")
  cat("  ", strrep("─", 95), "\n")
  
  numerator <- sum(w * a * b, na.rm = TRUE)
  total_wA2 <- sum(w * a^2, na.rm = TRUE)
  total_wB2 <- sum(w * b^2, na.rm = TRUE)
  denom <- sqrt(total_wA2) * sqrt(total_wB2)
  cs <- if (denom == 0 || is.nan(denom) || is.infinite(denom)) 0 else numerator / denom
  
  for (i in 1:nrow(relevant)) {
    f <- relevant$facet[i]
    cat(glue("  {f} | {round(w[i],2)} | {round(b[i],3)} | {round(a[i],3)} | {round(w[i]*a[i]*b[i],3)} | {round(w[i]*a[i]^2,3)} | {round(w[i]*b[i]^2,3)}\n"))
  }
  
  cat("  ", strrep("─", 95), "\n")
  cat(glue("  Sum: | | | | {round(numerator,3)} | {round(total_wA2,3)} | {round(total_wB2,3)}\n"))
  cat(strrep("─", 95), "\n")
  
  cat(glue("\n  Numerator = {round(numerator, 3)}\n"))
  cat(glue("  √Σ(w·A²) = √{round(total_wA2,3)} = {round(sqrt(total_wA2),3)}\n"))
  cat(glue("  √Σ(w·B²) = √{round(total_wB2,3)} = {round(sqrt(total_wB2),3)}\n"))
  cat(glue("  Denominator = {round(sqrt(total_wA2),3)} × {round(sqrt(total_wB2),3)} = {round(denom,3)}\n"))
  cat(glue("  Congruence Score (CS) = {round(numerator,3)} / {round(denom,3)} = {round(cs,3)}\n"))
}
cat(strrep("─", 60), "\n\n")

cat("🏆 TOP 10 OCCUPATION MATCHES\n")
cat(strrep("─", 60), "\n")

if (exists("ranked_results") && nrow(ranked_results) > 0) {
  top10 <- ranked_results %>% slice_head(n = 10)
  for (i in 1:nrow(top10)) {
    rank_icon <- if (i <= 3) c("🥇", "🥈", "🥉")[i] else paste0(i, ".")
    cs <- top10$Congruence[i]
    level <- if (cs >= 0.7) "🌟 Excellent" else if (cs >= 0.5) "✅ Strong" else if (cs >= 0.3) "📊 Moderate" else "📈 Developing"
    cat(glue("  {rank_icon} {top10$Occupation[i]} (Code: {top10$Code[i]}) -> {round(cs, 4)} ({level})\n"))
  }
}
cat(strrep("─", 60), "\n\n")

cat("🔍 DIAGNOSTIC MODE CHECK\n")
cat(strrep("─", 60), "\n")

if (exists("max_congruence")) {
  cat(glue("  Max congruence: {round(max_congruence, 3)}\n"))
  cat(glue("  Threshold: {congruence_threshold}\n"))
  if (max_congruence < congruence_threshold) {
    cat("  ✅ Mode: DIAGNOSTIC (no strong matches)\n")
  } else {
    cat("  ✅ Mode: NORMAL (positive matches found)\n")
  }
}
cat(strrep("─", 60), "\n\n")

# ---- 18. FINISH ----
cat("\n", strrep("═", 70), "\n")
cat("✅ Pipeline complete. Check your folders for output files.\n")
cat(glue("   Graphs: {graphs_folder}\n"))
cat(glue("   Reports: {reports_folder}\n"))
cat(strrep("═", 70), "\n")
