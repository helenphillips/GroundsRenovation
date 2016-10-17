# GroundsRenovation
This github repository contains all code (including Latex) for recent analysis that investigated the biodiversity value of the Natural History Museum's grounds before and after proposed renovations.

Paper was submitted to PeerJ for review.

## Details
===========
### R/
This folder contains all functions related to all the analysis. Loaded within individual scripts.

### analysis/
Scripts for the main analysis.

#### MetaAnalysis.R
Main analysis script for the meta-analysis. *Should* be able to be re-run if entire repository has been downloaded and working directory has been set.

#### SpeciesSimilarity.R
Code calculates the similarity in plant species between different habitats within the grounds. *Should* work on anymore

#### SensitivityAnalysis.R
Code to run the sensitivity analysis that investigated how the overall biodiversity value might change due to uncertainty around the model coefficients.



### data/
All data used in the analysis.  Also include bibliography for papers used in the analysis.


### Latex/
Main latex files for the write up and submission to PeerJ.
#### peerJ/
Latex files that apply all formatting, abstract and acknowledgements to the article.

#### wlg-main/
Text only for the main article and then the supplementary file
