PIMS Atlas Pipeline — README
Version: 1.0 (Publication Model)
Author: Gary C Townsend (2026)
Purpose: Match a user's TPQ personality profile against O*NET work styles to identify the strongest occupational fits, using the PIMS (Personality-Informed Matching System) framework.

1. Overview
This R script automates the full PIMS Atlas pipeline:
Loads normative data (means/SDs per facet) from your NormGroups folder — falls back to hardcoded Table 1 stats if unavailable.
Loads and combines all Work Style definition files (.xlsx) from the WorkStyles subfolder.
Loads the Facet Mapping Matrix and Motivational Anchors files.
Applies a Job Zone filter and Impact threshold to reduce noise.
Builds a "demand signature" (facet vector) for every occupation using symbol parsing (±0.7σ rule) and role-based weighting.
Loads the user's TPQ scores from UserScores.
Computes a weighted cosine congruence between the user and every occupation.
Enters one of two branches:
Normal Mode — if max congruence ≥ threshold (0.2). Prints top matches, generates a radar chart (PDF + PNG), console narrative, and optional AI-generated counsellor reports.
Diagnostic Mode — if max congruence < threshold. Produces a compassionate "no strong match" report, highlights highest/lowest trait clusters, and suggests hybrid pathways.
Outputs a detailed verification report in the console (step-by-step calculation of the top match) for audit purposes.

2. Prerequisites
R packages
r
install.packages(c(
  "readxl", "dplyr", "tidyr", "purrr", "glue", "stringr",
  "ggplot2", "gridExtra", "grid", "httr", "jsonlite",
  "tools", "fmsb", "scales"
))
Folder structure
Place the R-Folders directory inside your Documents folder. The script auto-detects it in common locations (OneDrive, Box, plain Documents) and will fall back to a recursive search.
text
Documents/
└── R-Folders/
    ├── WorkStyles/
    │   ├── Attention to Detail v1.0.xlsx
    │   ├── Cautiousness v1.0.xlsx
    │   └── ... (one file per work style)
    ├── NormGroups/
    │   └── (optional) facet stats CSV or XLSX
    ├── FacetMappingMatrix/
    │   └── PIMS Facet Mapping Matrix*.xlsx
    ├── MotivationalAnchors/
    │   └── PIMS Motivational Anchors*.xlsx
    ├── UserScores/
    │   └── user_tpq_scores.csv
    ├── Graphs/      ← auto-created
    └── Reports/     ← auto-created
File-name rules (important)
Folder	What is required
WorkStyles/	One .xlsx per work style. The filename must start with the exact work-style name (e.g. Attention to Detail v1.0.xlsx, Cautiousness v2.1.xlsx). Version suffix is free-form. If multiple versions exist, the most recently modified one wins and a warning is printed.
NormGroups/	A .csv or .xlsx with columns containing facet, mean (or µ/mu), and sd (or std/σ). If absent, hardcoded Table 1 values are used.
FacetMappingMatrix/	An .xlsx starting with PIMS Facet Mapping Matrix.
MotivationalAnchors/	An .xlsx starting with PIMS Motivational Anchors.
UserScores/	A .csv named user_tpq_scores.csv (preferred). Other CSVs are ignored unless renamed. Column names must match the facet names exactly (e.g. Thrill-seeking, Self-controlled). The script auto-detects <facet> columns with hyphens (it converts dots to hyphens).

3. Required Columns
Work Style .xlsx files
Must contain a sheet with all of these columns:
Fixed columns:
Impact, Job Zone, Code, Occupation
Facet columns (30):
Thrill-seeking, Contracted, Sociable, Assertive, Unguarded, Engaging, Compassionate, Empathic, Altruistic, Collaborative, Modest, Candid, Resilient, Self-controlled, Relaxed, Optimistic, Tempered, Impervious, Directed, Competent, Structured, Self-disciplined, Dutiful, Cautious, Creative, Unconventional, Complex, Conceptual, Conversant, Composed
Facet cells contain a motivation symbol:
Symbol	Meaning	Demand computed as
↑ or Δ	High demand (primary)	μ + 0.7σ
↓ or Λ	Low demand / suppression	μ − 0.7σ
↔	Balance required (modulation)	μ
0 or blank	Not required	ignored
user_tpq_scores.csv
Must contain one row (the user) and one column per facet, with values on the 1–4 Likert scale. Values are clamped to [1, 4] before z-standardisation.
PIMS Facet Mapping Matrix*.xlsx
Required columns (exact spelling):
O*NET Broad Work Style Element
O*NET Work Style Element
PIMS Motivation Element
TPQ Required Facet
Motivation Symbol
TPQ Facet Role
TPQ Facet Psychological Function
PIMS Motivational Anchors*.xlsx
Required columns:
O*NET Broad Work Style Element
O*NET Work Style Element
Primary Motivator
PIMS Motivation Element
PIMS Motivation Intrinsic Driver (The "How" it feels)
PIMS Motivation Extrinsic Driver (The "What" it gains)
Norm file (optional)
Any of: Facet + Mean + SD (case-insensitive; µ, mu, μ, σ, std also accepted).

4. Tunable Parameters
Located in Section 1 (Configuration) near the top of the script:
Variable	Default	What it does
min_job_zone	3	Drops occupations whose O*NET Job Zone is below this.
min_impact_threshold	90	Only Work Style rows with Impact ≥ 90 contribute to the occupation signature.
congruence_threshold	0.2	Below this max congruence, Diagnostic Mode fires.
top_n	3	Number of top occupations analysed in Normal Mode.
z_diff_threshold	0.5	Used to classify a facet as a strength / development area / aligned.
z_threshold (inside symbol_to_demand)	0.7	σ-multiplier for symbol → demand conversion.
api_key (Section 13)	""	Gemini API key. Leave blank to disable AI reports (fallback templates are used instead).

5. Running the Script
Fill in the API key in Section 13 if you want AI-generated reports (optional).
Place user_tpq_scores.csv in UserScores/.
Source the script in RStudio (Ctrl+Shift+S or source("PIMS_Atlas.R")).
Watch the console — it prints progress at every step (found files, filters applied, occupations processed, etc.).
Check:
Graphs/pims_radar_comparison.pdf and .png — top-match radar chart.
Reports/report_<Occupation>.pdf — one per top-N occupation (Normal Mode).
Reports/counsellor_reports_full.txt — concatenated AI narratives.
Reports/diagnostic_report.{txt,pdf} — Diagnostic Mode only.

6. Outputs
Normal Mode
Console: Top 3 matches with congruence and level (Excellent ≥ 0.7, Strong ≥ 0.5, Moderate ≥ 0.3, Developing < 0.3), then per-occupation strengths/development areas, then the AI narrative.
Radar chart (PDF + PNG): Single-occupation comparison showing the user profile vs the top match, with:
Title containing Core Motivations (each followed by the contributing Work Styles in parentheses) and the cosine value in brackets.
Subtitle showing the Impact threshold, Job Zone filter, and zero-axis note.
Centred horizontal legend (User | Occupation) below the subtitle.
Footer listing "Considered Motivations (All Work Styles)".
PDF reports: One narrative per top-N occupation.
Diagnostic Mode
Console: Closest matches (with scores), cluster scores, and an AI-generated diagnostic narrative (or fallback text).
Files: Reports/diagnostic_report.txt and Reports/diagnostic_report.pdf.
Verification Report (always printed at end)
Full worked example for the #1 match: source rows → Impact normalisation → symbol parsing → aggregation → weighting → z-scores → user standardisation → the weighted-cosine arithmetic table with the numerator, both norm terms, and the final CS. Useful for auditing or debugging unexpected matches.

7. Key Algorithmic Notes
Facet standardisation: All z-scores use the loaded norm means/SDs (training_means, training_sds). User raw scores are clamped to [1, 4] before standardising.
Weighted cosine: Facets with weight 0 (not required by the occupation) are excluded. The weight reflects the symbol role: 1.0 for primary, 0.5 + 0.2·freq for secondary, 0.3 + 0.2·freq for modulation.
Job Zone filter applies before atlas construction — occupations below min_job_zone never enter the ranking.
Normal vs Diagnostic branch is decided by the maximum congruence across all ranked occupations.

8. Troubleshooting
Symptom	Likely cause
Could not automatically locate the 'R-Folders' directory	Set work_styles_folder manually in Section 1.
No sheet contained all required columns	One of the .xlsx files is missing a facet column or a fixed column — the script lists what it found.
Many user facets at default 2.5	Column names in user_tpq_scores.csv do not match facet names (e.g. dots vs hyphens, or typos).
Column mismatch in mapping file	The mapping .xlsx header text is not exactly as specified in §3.
Multiple WorkStyles files matched	Older versions in the folder. The newest by mtime is used; the script prints which one. Delete or move stale versions to silence warnings.
Radar labels cramped	Increase vlcex upward or raise the radar_matrix[1, ] "max" value (currently 8.0) to push the outer ring inward.
AI step prints "No valid API key"	Add your Gemini key in Section 13 or accept the fallback template.

9. Version History
v1.0 — Consolidated mapping files, norm groups, radar visualisation with zero axis.
Fix A: Radar limited to top match only; core motivations list contributing Work Styles in parentheses.
Fix B: Radar title includes top-match cosine.
Fix C: Legend moved to top of graph, below the impact-threshold line.
Fix D: Legend entries separated by | and centred via strwidth().
Fix E: Enlarged radar "max" + taller radar panel — outer facet labels no longer overlap adjacent panels.

10. License / Attribution
This project is licensed under the MIT License - see the LICENSE file for details.

