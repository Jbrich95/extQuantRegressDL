# extQuantRegressDL
This repository contains the code used for the application and simulation study of Chapter 21 "Extreme Quantile Regression with Deep Learning" for Chapman and Hall/CRC Handbook on Statistics of Extremes. A preview of the chapter can be found on [arXiv](https://arxiv.org/abs/2404.09154).

## Installation 

First, install the relevant R packages.

```r
source("install_packages.R")

```


We first create a virtual Python environment, which we call 'eQRDL'. The code below uses the R-reticulate package to install Python v3.9.18. If you run into any issues installing Python, please see [this installation guide for reticulate](https://rstudio.github.io/reticulate/articles/python_packages).

We then install CPU tensorflow and Keras in 'eQRDL'. See [this installation guide](https://tensorflow.rstudio.com/install/) for further details on installation of tensorflow in R.


```r
#Install Python 3.9.18
py_version <- "3.9.18"
path_to_python <- reticulate::install_python(version=py_version)

#Create a virtual envionment 'eQRDL'.
reticulate::virtualenv_create(envname = 'eQRDL',
                              python=path_to_python,
                              version=py_version,
                              force=T)
path<- paste0(reticulate::virtualenv_root(),"/eQRDL/bin/python")
Sys.setenv(RETICULATE_PYTHON = path) #Set Python interpreter to that installed in myenv

#Install tensorflow in 'eQRDL'. The R session will restart after this block is run.
tf_version="2.13.1" 
tensorflow::install_tensorflow(method="virtualenv", envname="eQRDL",
                               version=tf_version) #Install version of tensorflow in virtual environment
                               
#Similarly, install Keras                               
keras::install_keras(method = c("virtualenv"), envname = "eQRDL",version=tf_version) #Install keras

#Check if keras is available
reticulate::use_virtualenv("eQRDL", required = T)
keras::is_keras_available() 

```

## Details

The directory `sim_study` contains the code used to perform the simulation study in Section 1.4 of the handbook chapter. We consider estimation of return levels using different machine learning methods. Two settings are considered for the true data-generating distribution, i.e., $Y | \mathbf{X}=\mathbf{x}$ : i) generalised Pareto (GP) upper-tails and ii) log-normal. The code for generating these data are found in the scripts `sim_GP.R` and `sim_lnorm.R`, respectively.

The different algorithms for estimating return levels are listed below. The first three methods (`evGAM.R`, `gbex.R`, and `GP_MLP.R`) utilise GP regression models, but with the scale and shape parameters, $\sigma$ and $\xi$, modelled using different machine learning algorithms (generalised additive models, gradient boosting, and neural networks, respectively). The script `Q_MLP.R` fits single-quantile deep regression models using multi-layered perceptrons. Each script requires the user to set two arguments: `type` and `no.experiment`. The argument `type` will change the data-generating process, with `type=1` corresponding to case i) above and `type=2` corresponding to case ii), i.e., log-normal data.  The argument `no.experiment` will change the seed for the data generation scheme, and represents a different experiment in the simulation study. In the handbook chapter, we consider `no.experiment` ranging from 1 to 250; these results have been saved in the directory `sim_study_results`, and are separated by type.


```bash
├── sim_GP.R          (Functions for generating GP data)
├── sim_lnorm.R       (Functions for generating log-normal data)
├── evGAM.R           (Generalised additive models -- evGAM)
├── gbex.R            (Gradient boosting of extremes)
├── GP_MLP.R          (Deep GP regresion with multi-layered perceptrons)
├── Q_MLP.R           (Deep quantile regression with multi-layered perceptrons)
```
