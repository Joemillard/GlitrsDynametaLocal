# GLiTRS Dynameta

<br>

GLiTRS Dynameta is a package for the visualisation of a set of effect sizes corresponding to the effect of anthropogenic activities on insects.

<br>

GLiTRS Dynameta was developed as part of the [GLiTRS](https://glitrs.ceh.ac.uk/) project, a cross-institutional consortium aiming to build global threat-response models to better understand and predict insect biodiversity change.

<br>

If you encounter any issues or bugs while installing or using GLiTRS Dynameta, please submit a new issue in the [issue tracker](https://github.com/Joemillard/GlitrsDynametaLocal/issues) with a detailed description of the problem, including steps to reproduce it.

<br>

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
library(Dynameta)

# Or if using remotes, run: 
remotes::install_github("Joemillard/GlitrsDynametaLocal", build_vignettes = TRUE) 
library(Dynameta)
```
<br> 

### Installing in R (not RStudio) using devtools
Installing packages hosted on GitHub uses Pandoc software, which will need to be installed before installing GLiTRS Dynameta (Pandoc comes as standard with RStudio installation). See the documentation [here](https://pandoc.org/installing.html). Pandoc is required to build vignettes. 

Once you have Pandoc installed, run the following code (as above):
```
install.packages("devtools")
library(devtools)
devtools::install_github("Joemillard/GlitrsDynametaLocal", build_vignettes = TRUE) 
library(Joemillard/GlitrsDynametaLocal)
```
If GLiTRS Dynameta installation is unsuccessful, try removing the 'build_vignettes = TRUE' argument from the installation command. 

<br>

### Install GLiTRS Dynameta from source
Alternatively, you could download the Source code (tar.gz file) from the Dynameta [releases page](https://github.com/Joemillard/GlitrsDynametaLocal/releases). Then run the following code in R/RStudio:
```
install.packages(path_to_file, repos = NULL, type = "source") # where path_to_file would represent the full path and file name
```

<br>
