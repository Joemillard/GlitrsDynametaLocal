# GLiTRS Dynameta

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6354502.svg)](https://doi.org/10.5281/zenodo.16785811) 
[![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Joemillard/GlitrsDynametaLocal/blob/master/LICENSE.md)
<!-- badges: end -->

***

### Background

GLiTRS Dynameta is a package for the visualisation of a set of effect sizes corresponding to the effect of anthropogenic activities on insects, published by Diversity and Distributions under the title “A multi-threat meta-analytic database for understanding insect biodiversity change”. GLiTRS Dynameta was developed as part of the GLiTRS project, a cross-institutional consortium aiming to build global threat-response models to better understand and predict insect biodiversity change.

Both current (i.e. meta-analyses carried out by a GLiTRS contributor) and prior (i.e. collected from previously published literature) effect sizes can be downloaded from the view effect sizes page. If using the effect sizes, please cite the original Dynameta publication (Skinner et al. 2023) and the Methods publication describing the full effect size database (Millard et al. 2025, Diversity & Distributions):

- Skinner, G., Cooke, R., Keum, J., Purvis, A., Raw, C., Woodcock, B.A. and Millard, J., 2023. Dynameta: A dynamic platform for ecological meta-analyses in R Shiny. SoftwareX, 23, p.101439.
- Millard, J., Skinner, G., Bladon, A., Cooke, R., Outhwaite, C., Rodger, J. G., Barnes, L. A., Isip, J., Keum J., Raw, C., Wenban-Smith, E., Dicks, L. V., Hui, C., Jones, I. J., Woodcock, B., Isaac, N. J. B., 2025. Purvis, A. A multi-threat meta-analytic database for understanding insect biodiversity change. Diversity and Distributions

### File structure and variable names
GLiTRS Dynameta is a built as an interactive Shiny R package, meaning it is a code and data release together, designed to by installed and run in R. If you don’t want to use the package, you can use our new website hosted at the Natural History Museum in London (https://shiny.nhm.ac.uk/glitrs-dynameta/). You can also find all effect sizes in the files ‘inst/shiny_data/current_data.rds’ and ‘inst/shiny_data/prior_data.rds’. ’current_data.rds’ contains all effect sizes collected by meta-analyses carried out by a contributor to the GLiTRS project, whereas ‘prior_data.rds’ contains all effect sizes collected from previously published papers. Given there are many variables in both these data files, we do not describe these in detail here. Please see our pre-registration on the Open Science Framework for more details.

### Sharing/Access information
The data in GLiTRS Dynameta were derived from 7 meta-analyses, four of which are available in the previously published literature, and three of which are currently in preparation for publication. Please see the below four citations for the previously published papers. When using this database users should also cite these 4 papers:

- Gallego‐Zamorano, J., de Jonge, M.M., Runge, K., Huls, S.H., Wang, J., Huijbregts, M.A. and Schipper, A.M., 2023. Context‐dependent responses of terrestrial invertebrates to anthropogenic nitrogen enrichment: A meta‐analysis. Global Change Biology, 29(14), pp.4161-4173.
- Liang, H., He, Y.D., Theodorou, P. and Yang, C.F., 2023. The effects of urbanization on pollinators and pollination: A meta‐analysis. Ecology Letters, 26(9), pp.1629-1642.
- Nessel, M.P., Konnovitch, T., Romero, G.Q. and González, A.L., 2023. Decline of insects and arachnids driven by nutrient enrichment: A meta‐analysis. Ecology, 104(2), p.e3897.
- Wang, J., Ding, C., Heino, J., Jiang, X., Tao, J., Ding, L., Su, W., Huang, M. and He, D., 2020. What explains the variation in dam impacts on riverine macroinvertebrates? A global quantitative synthesis. Environmental Research Letters, 15(12), p.124028.ugs

If you encounter any issues or bugs while installing or using GLiTRS Dynameta, please submit a new issue in the [issue tracker](https://github.com/Joemillard/GlitrsDynametaLocal/issues) with a detailed description of the problem, including steps to reproduce it.

<a name="installing-and-using-the-glitrs-dynameta-package"></a>
## Installing and using the GLiTRS Dynameta package 

### Installing in RStudio using devtools
Installing the package requires devtools, which can be installed and loaded by running the following code in the R console:

NOTE: If you have not installed devtools before you will need to restart your R session before installing to avoid problems. 
```
install.packages("devtools")
library(devtools)
```
If you have issues installing devtools, you could try the remotes package as an alternative. This is a lightweight replacement of the install_* functions in devtools. Install by running:
```
install.packages("remotes")
library(remotes)
```

Next, install and load the GLiTRS Dynameta package by running the following code in the R console:
```
devtools::install_github("Joemillard/GlitrsDynametaLocal", build_vignettes = TRUE) 
library(GlitrsDynametaLocal)

# Or if using remotes, run: 
remotes::install_github("Joemillard/GlitrsDynametaLocal", build_vignettes = TRUE) 
library(GlitrsDynametaLocal)
```

### Installing in R (not RStudio) using devtools
Installing packages hosted on GitHub uses Pandoc software, which will need to be installed before installing GLiTRS Dynameta (Pandoc comes as standard with RStudio installation). See the documentation [here](https://pandoc.org/installing.html). Pandoc is required to build vignettes. 

Once you have Pandoc installed, run the following code (as above):
```
install.packages("devtools")
library(devtools)
devtools::install_github("Joemillard/GlitrsDynametaLocal", build_vignettes = TRUE) 
library(GlitrsDynametaLocal)
```
If GLiTRS Dynameta installation is unsuccessful, try removing the 'build_vignettes = TRUE' argument from the installation command. 

### Install GLiTRS Dynameta from source
Alternatively, you could download the Source code (tar.gz file) from the GLiTRS Dynameta [releases page](https://github.com/Joemillard/GlitrsDynametaLocal/releases). Then run the following code in R/RStudio:
```
install.packages(path_to_file, repos = NULL, type = "source") # where path_to_file would represent the full path and file name
```

<br>
