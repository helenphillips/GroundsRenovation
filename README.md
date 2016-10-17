# GroundsRenovation
This github repository contains all code (including Latex) for recent analysis that investigated the biodiversity value of the Natural History Museum's grounds before and after proposed renovations.

Paper was submitted to PeerJ for review.

## Details
===========
### MetaAnalysis/Scripts folder

This folder contains all scripts related to the meta-analysis.
#### 02_main.R
Main analysis script for the meta-analysis. *Should* be able to be re-run if entire repository has been downloaded and working directory has been set.

#### 03_PlantsDatabase.R
Not nessecary to re-run for re-analysis. Output has already been added to the main dataset.
This scripts uses the data from the Wildlife Garden's database (available elsewhere), to find relevant data from each habitat area to calculate the species richness. Output is saved as a .csv file, and has already been added to main dataset.

#### 04_SensitivityAnalysis.R
Code to run the sensitivity analysis that investigated how the overall biodiversity value might change due to uncertainty around the model coefficients.

#### /Functions
All functions that the above R code used. Loaded within the scripts.


### PlantsDatabase/Scripts
Analysis relating to the Wildlife Garden's database on plant species. Will need original data (not provided here) to run. Not written to be run by 

#### 01_main.R
Code would need to be adapted to run on other machines (sorry). Would also require the Wildlife Garden database (available elsewhere).  Code calculates the similarlity in plant species between different habitats within the grounds.

### Latex/
Main latex files for the write up and submission to PeerJ.
#### peerJ/
Latex files that apply all formatting, abstract and acknowledgements to the article.

#### wlg-main/
Text only for the main article and then the supplementary file
