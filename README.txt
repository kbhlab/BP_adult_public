This repo contains the data and code for "Pushing boundaries: Bilinguals’ phoneme perception when cued by real words." 
https://osf.io/xf9ug/

Code was written by Lena V. Kremin (lena.kremin@mail.concordia.ca) and Andrea Sander-Montant.

Scripts must be run sequentially according to the number of their file (i.e., 01_load must be run before 02_models_mixed-effect, etc.).

The following packages are required to run the scripts:
library(tidyverse)
library(here)
library(Hmisc)
library(lme4)
library(lmerTest)
library(afex)
library(janitor)
library(data.table)
library(beepr)
library(arm)
library(boot)
library(separationplot)
library(performance)
library(DescTools)

The 05_visualizations script contains different versions of visualizations that were used for conferences as work on this project progressed (as saved in figures folder). Visualizations as seen in the final manuscript are in 06_manuscript


The data folder contains the data in different forms.
- partial data: a subset of the data used to check sample size during data collection to determine when to stop data collection in accordance with the pre-registration (https://osf.io/k3m2w/)
- raw data: data downloaded directly from online testing platform and LHQ survey; used in scripts where possible
- data not in subfolder: data that was either pre-processed by hand to add in start times to match meta data to response data, or LHQ responses coded for common errors (as described in the LHQ Data Dictionary), and a subject tracking log

