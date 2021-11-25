This repository contains the required R code and data files to produce most of the analyses and plots in:  
‘Humans are disrupting a longstanding trophic-size structure in vertebrates’   
Robert S. C. Cooke and William Gearty, Abbie S. A. Chapman, Jillian Dunic, Graham J. Edgar, Jonathan S. Lefcheck, Gil Rilov, Craig R. McClain, Rick D. Stuart-Smith, S. Kathleen Lyons, and Amanda E. Bates  
Nature Ecology & Evolution (in revision)
  
Before running these R scripts, we suggest that you download this folder and set the `R` folder as your R working directory. Required data files should then load when called in each script, and plot files will save within the `figures` folder.

## Extant analyses (main text figures 2-3; supplementary figures 1-2 and 7):  
Run `v_plots_rob.R` to produce all plots and analyses.

## Fossil analyses (main text figures 1 and 4; supplementary figures 3-6):  
Run `mammal_diet.R` to produce all plots and analyses. 

## Future analyses (main text figure 5):  
Run `future_v_short.R` to produce all plots and analyses. 

## To replicate the analyses and plots presented here, the following R packages are required:
cowplot  
deeptime  
dplyr  
ggplot2  
grImport  
gtable  
gtools  
Hmisc  
lemon  
nlme  
pacman  
plyr  
stringr  
tidyr  
tidyverse  
vectoR  
viridis  

The vectoR package is currently only available on GitHub and will need to be installed from there to reproduce
these analyses and plots. This can be achieved by running the following commands in your R console (ignore the first line 
if you already have devtools installed).  
```r
install.packages("devtools")  
devtools::install_github("richfitz/vectoR")
```

  
All other packages can be installed from CRAN. These scripts have been tested using R version 4.1.0 - 
Copyright (C) 2021 The R Foundation for Statistical Computing.
