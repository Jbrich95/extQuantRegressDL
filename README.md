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
