# PIMS Atlas Software Pipeline

An automated data science pipeline developed in R to map, analyze, and visualize client personality profile configurations using the PIMS framework. This software cross-references individual scores against O*NET work style demand profiles and normative reference data to generate structured career guidance outputs.

## 🗂️ Project Structure

The repository is organized to run right out of the box with the following file arrangement:
* `PIMS Atlas.R` — The primary execution script containing data loading, data transformation, matching logic, and reporting modules.
* `R-Folders/` — Main data subdirectory containing core asset dependencies:
  * `PIMS Facet Mapping Matrix.xlsx` & `PIMS Motivational Anchors.xlsx` — Base matrix configurations.
  * `NormGroups/` — Subdirectory containing baseline normative statistics.
  * `UserScores/user_tpq_scores.csv` — File input structure for staging active client profile metrics.

## 🚀 Getting Started (How to Run)

Follow these steps to run the pipeline on your local system:

1. **Download the Repository**
   Click the green **Code** button at the top right of this page and select **Download ZIP**. Extract the contents to any folder on your machine.

2. **Launch the Workspace**
   Open the extracted project folder and open **`PIMS Atlas.R`** in **RStudio**. 

3. **Verify Local Directory Configuration**
   The project is pre-configured with relative directory paths. Ensure your script paths reflect the localized structure:
   ```R
   work_styles_folder <- "./R-Folders"
   norm_groups_folder <- "./R-Folders/NormGroups"
   graphs_folder      <- "./R-Folders/Graphs"
   reports_folder     <- "./R-Folders/Reports"
   userscores_folder  <- "./R-Folders/UserScores"
   ```

4. **Execute the Script**
   Run the full pipeline. The script automatically verifies directory paths, checks for your user-profile score staging matrix, processes the underlying alignment matrices, and outputs analytical deliverables directly into your local generation folders.

## 📊 Pipeline Outputs

Once execution finishes, look inside your auto-generated directories for your results:
* `/Graphs` — Contains the composite **PIMS Radar Chart Comparison Visualization** (`.pdf` and `.png` versions).
* `/Reports` — Contains the deep-dive, narrative career counselor reports generated via automated framework alignment.

---
*Developed by Gary C. Townsend (2026)*
