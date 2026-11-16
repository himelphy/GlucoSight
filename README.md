# GlucoSight
Shiny application for survillance accuracy test of glucose monitors
GLUCOSIGHT QUICK START GUIDE

1. SETUP & PACKAGES

   - Open the R script in RStudio and run it. Missing packages install automatically.


2. INPUT DATA

   - Prepare a CSV file with two columns: "Reference" and "Meter" (in mg/dL).

   - Use the "Download Example CSV" button in the sidebar if you need a template.


3. HOW TO USE

   - Click "Choose CSV File" to upload your data.

   - Select the PEGA Diabetes Type (Type 1 or Type 2).

   - Click the blue "Analyze" button.


4. TABS OVERVIEW

   - Numerical Statistics: ISO, FDA, ADA, and CLIA compliance results.

   - Raw Data Analysis: Color-coded table and Excel export with pass/fail highlights.

   - MARD & Paired Analysis: Descriptive stats, MARD, paired t-test, and slope plots.

   - Error Grid Analysis: Clarke (CEGA) and Parkes (PEGA) tables and plots with export options.

   - Modified Bland-Altman: Difference plot with ISO acceptance limit lines.

   - Credit: Developer citation and contact information.


5. DEVELOPER

   - Dr. Himel Mondal, Department of Physiology, AIIMS Deoghar, India.
