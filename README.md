PIMS Atlas
Personality-Interest Motivational Sequences Atlas - A Generative Career Pathwaying Tool

The PIMS Atlas is an R-based career guidance tool that maps personality architectures to occupational psychological demand signatures using weighted cosine similarity.

📁 Repository Structure
text
PimsAtlas/
├── PIMS Atlas.R                          # Main R script
├── README.md                             # This file
└── R-Folders/                            # Data folder (ALL data files go here)
    ├── NormGroups/                       # Normative personality data
    │   └── pims_atlas_norm_data.csv      # TPQ normative statistics (μ, σ for 30 facets)
    ├── UserScores/                       # User personality scores
    │   └── user_tpq_scores.csv           # Individual's TPQ facet scores
    ├── Graphs/                           # Output: radar charts (auto-created)
    ├── Reports/                          # Output: PDF/TXT reports (auto-created)
    ├── PIMS Facet Mapping Matrix.xlsx    # O*NET Work Styles → TPQ facets mapping
    ├── PIMS Motivational Anchors.xlsx    # Motivational driver framework
    └── [21 Work Style .xlsx files]       # O*NET Work Style data files
🚀 Getting Started
Prerequisites
R (version 4.0 or later)

RStudio (recommended)

Required R packages (install via R console):

r
install.packages(c(
  "readxl", "dplyr", "tidyr", "purrr", "glue", "stringr",
  "ggplot2", "gridExtra", "grid", "httr", "jsonlite",
  "tools", "fmsb", "scales"
))
⚙️ Configuration: Setting Your Local File Path
IMPORTANT: The script comes with a hardcoded path that will NOT work on your machine. You MUST update the work_styles_folder variable to point to your local R-Folders folder.

Step 1: Locate Your R-Folders Folder
After cloning the repository, your R-Folders folder will be located at:

text
[Your-Clone-Path]/PimsAtlas/R-Folders/
For example:

Windows: C:/Users/YourName/Documents/GitHub/PimsAtlas/R-Folders

Mac: /Users/YourName/Documents/GitHub/PimsAtlas/R-Folders

Linux: /home/YourName/GitHub/PimsAtlas/R-Folders

Step 2: Edit the Script
Open PIMS Atlas.R in RStudio and find the CONFIGURATION section (around line 30):

r
# ---- 1. CONFIGURATION ----
# HARDCODED PATH TO R-Folders (CHANGE THIS TO YOUR ACTUAL FOLDER)
work_styles_folder <- "C:/Users/garyt/OneDrive/Documents/Box Sync Laptop/Emeris/Research/Project PIMS/PIMS Atlas Development/PimsAtlas/R-Folders"
CHANGE THIS LINE to your actual path:

r
# Windows example:
work_styles_folder <- "C:/Users/YourName/Documents/GitHub/PimsAtlas/R-Folders"

# Mac example:
work_styles_folder <- "/Users/YourName/Documents/GitHub/PimsAtlas/R-Folders"

# Linux example:
work_styles_folder <- "/home/YourName/GitHub/PimsAtlas/R-Folders"
Step 3: Verify Your Data Files
Ensure the following files and folders exist inside your R-Folders folder:

Required Item	Description
PIMS Facet Mapping Matrix.xlsx	Mapping file (MUST exist)
PIMS Motivational Anchors.xlsx	Motivational anchors (MUST exist)
NormGroups/pims_atlas_norm_data.csv	Normative data (MUST exist)
UserScores/user_tpq_scores.csv	User scores (MUST exist)
Attention to Detail.xlsx	Work Style data (ALL 21 files required)
Cautiousness.xlsx	(and all other Work Style .xlsx files)
Step 4: Run the Script
In RStudio:

Open PIMS Atlas.R

Click the Source button (or press Ctrl+Shift+S / Cmd+Shift+S)

Or run in the console:

r
source("PIMS Atlas.R")
🔍 Verification: Did It Work?
When you run the script, you should see output like this:

text
📂 Verifying paths...
  Work Styles folder exists: TRUE
  Mapping file exists: TRUE
  Anchors file exists: TRUE
  Norm Groups folder exists: TRUE
  User Scores folder exists: TRUE
✅ All critical files found. Continuing...
If you see FALSE for any item, check:

That the path in work_styles_folder is correct

That all required files are in the R-Folders folder

That folder names are spelled correctly (case-sensitive on Mac/Linux)

📊 Output Files
After running successfully, the script generates:

Output	Location	Description
pims_radar_comparison.pdf	R-Folders/Graphs/	Radar chart comparing user vs top occupations
pims_radar_comparison.png	R-Folders/Graphs/	Radar chart (PNG format)
report_[Occupation].pdf	R-Folders/Reports/	AI-generated career report (per occupation)
counsellor_reports_full.txt	R-Folders/Reports/	All reports combined in text format
diagnostic_report.pdf	R-Folders/Reports/	Diagnostic report (if no positive matches found)
🛠️ Troubleshooting
Error: path does not exist: ‘./R-Folders/PIMS Facet Mapping Matrix.xlsx’
Cause: The hardcoded path doesn't match your local folder structure.

Solution: Update the work_styles_folder path as described above.

Error: NormGroups folder is empty or missing
Cause: The NormGroups folder is missing from R-Folders or empty.

Solution: Ensure R-Folders/NormGroups/pims_atlas_norm_data.csv exists.

Error: UserScores folder is empty or missing
Cause: The UserScores folder is missing or empty.

Solution: Ensure R-Folders/UserScores/user_tpq_scores.csv exists.

Warning: Using hardcoded placeholder statistics
Cause: The normative data file was not found.

Solution: Check that pims_atlas_norm_data.csv is in R-Folders/NormGroups/.

📝 Alternative: Using a Configuration File (Advanced)
For advanced users, you can create a config.R file instead of editing the main script:

config.R:

r
# config.R - Custom configuration for PIMS Atlas
work_styles_folder <- "C:/Users/YourName/Documents/GitHub/PimsAtlas/R-Folders"
norm_groups_folder <- file.path(work_styles_folder, "NormGroups")
graphs_folder      <- file.path(work_styles_folder, "Graphs")
reports_folder     <- file.path(work_styles_folder, "Reports")
userscores_folder  <- file.path(work_styles_folder, "UserScores")
Then in PIMS Atlas.R, replace the configuration section with:

r
# ---- 1. CONFIGURATION ----
if (file.exists("config.R")) {
  source("config.R")
} else {
  # Fallback to hardcoded path
  work_styles_folder <- "C:/Path/To/Your/R-Folders"
  # ...
}
📄 License
[Specify your license here - e.g., MIT, GPL-3, etc.]

👥 Authors
Gary Clifford Townsend (gary.townsend43@gmail.com)

Portia Webb

📚 Citation
If you use the PIMS Atlas in your research, please cite:

Townsend, G. C., & Webb, P. (2026). The PIMS Atlas: Operationalising Generative Career Pathwaying Through Personality-Interest Motivational Sequences. [Journal/Preprint details].

❓ Questions?
For questions, issues, or contributions:

Open an issue on GitHub

Contact the corresponding author: gary.townsend43@gmail.com

Note: All participant data in the worked examples has been fully de-identified to protect participant confidentiality and anonymity.

