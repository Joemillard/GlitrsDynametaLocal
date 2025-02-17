# GLiTRS Dynameta

GLiTRS Dynameta is a package for the visualisation of a set of effect sizes corresponding to the effect of anthropogenic activities on insects. GLiTRS Dynameta was developed as part of the [GLiTRS](https://glitrs.ceh.ac.uk/) project, a cross-institutional consortium aiming to build global threat-response models to better understand and predict insect biodiversity change.

Both current (i.e. meta-analyses carried out by a GLiTRS contributor) and prior (i.e. collected from previously published literature) effect sizes can be downloaded from the view effect sizes page. If using the effect sizes, please cite the original Dynameta publication (Skinner et al. 2023) and the Methods publication describing the full effect size database (Millard et al. in review):

* Skinner, G., Cooke, R., Keum, J., Purvis, A., Raw, C., Woodcock, B.A. and Millard, J., 2023. Dynameta: A dynamic platform for ecological meta-analyses in R Shiny. SoftwareX, 23, p.101439.

* Millard, J., Skinner, G., Bladon, A., Cooke, R., Outhwaite, C., Rodger, J. G., Barnes, L. A., Isip, J., Keum J., Raw, C., Wenban-Smith, E., Dicks, L. V., Hui, C., Jones, I. J., Woodcock, B., Isaac, N. J. B., Purvis, A. A multi-threat meta-analytic database for understanding insect biodiversity change (in review)

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
