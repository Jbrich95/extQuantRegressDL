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

The script `Figure1_Chapter21.R` can be used to recreate Figure 1.1 of the handbook chapter.

Before running any scripts in either `sim_study/` or `application/`, navigate to the directory and change the working directory.


### Simulation study

The directory `sim_study/` contains the code used to perform the simulation study in Section 1.4 of the handbook chapter. We consider estimation of return levels using different machine learning methods. Two settings are considered for the true data-generating distribution, i.e., $Y | \mathbf{X}=\mathbf{x}$ : i) generalised Pareto (GP) upper-tails and ii) log-normal. The functions for generating these data are found in the scripts `sim_GP.R` and `sim_lnorm.R`, respectively.

The different algorithms for estimating extreme quantiles are listed below. The first three methods (`evGAM.R`, `gbex.R`, and `GP_MLP.R`) utilise GP regression models, but with the scale and shape parameters, $\sigma$ and $\xi$, modelled using different machine learning algorithms (generalised additive models, gradient boosting, and neural networks, respectively). The script `Q_MLP.R` fits single-quantile deep regression models using multi-layered perceptrons.

Each script requires the user to set two arguments: `type` and `no.experiment`. The argument `type` will change the data-generating process, with `type=1` corresponding to case i) above and `type=2` corresponding to case ii), i.e., log-normal data.  The argument `no.experiment` will change the seed for the data generation scheme, and represents a different experiment in the simulation study. In the handbook chapter, we consider `no.experiment` ranging from 1 to 250.

Running the scripts for the four algorithms will fit the corresponding model to estimate a sequence of quantiles, ranging from the 25th percentile to the 99.99th percentile. This will be repeated for two sample sizes: $n=10000$ and $n=1\times 10^6$. The mean-squared error (MSE) for each sample size and quantile is then saved in the directory `sim_study_results`; in the directory, we provide all of the MSE estimates used in the handbook. Figure 1.4, showcasing boxplots of the root normalised MSE estimates, can then be compiled by running `plot_MSE.R`.


```bash
├── sim_GP.R          (Functions for generating GP data)
├── sim_lnorm.R       (Functions for generating log-normal data)
├── evGAM.R           (Generalised additive models -- evGAM)
├── gbex.R            (Gradient boosting of extremes)
├── GP_MLP.R          (Deep GP regresion with multi-layered perceptrons)
├── Q_MLP.R           (Deep quantile regression with multi-layered perceptrons)
├── sim_study_results/           (Contains all MSE estimates)
├── plot_MSE.R        (Create panels for Figure 1.4)
├── Figures/           (Contains figures)
```

### Application

The directory `application/` contains the code and data used in the application in Section 1.5 of the handbook chapter. We model return levels of European precipitation using a blended generalised extreme value (GEV) deep regression model. For details on the blended GEV, see Castro-Camilo, D., et al. (2022) [doi:10.1002/env.2742](https://doi.org/10.1002/env.2742).

The data are contained within `monthly_max_data.Rdata`. Response data `Y` are monthly maxima of hourly precipitation values (mm) for a regular spatial grid encompassing the British Isles, as well as parts of France, Belgium, and the Netherlands. The grid-boxes are arranged on a regular $65\times 65$ latitude/longitude grid, with spatial resolution $0.25^\circ \times 0.25^\circ$. The observation period encompasses only the summer months (June, July, August) and the years 2001--2022, inclusive. This leaves 66 observations of the monthly maximum hourly rainfall per grid-cell. The variable `Y` is a $66 \times 4225$ matrix, with the rows corresponding to observations and the columns corresponding to sampling locations. The variable `coords` is a $4552 \times 2$ matrix of (longitude, latitude) coordinates for the sampling locations. `times` is a vector of the year-month for each observations.

We have $q=17$ covariates in `X` for each space-time observation of `Y`, and so `X` is a $66 \times 4225 \times 17$ array. The covariates include the monthly mean and maximum of the following six dynamic meteorological variables: air temperature at a 2m altitude (K), mean sea level pressure (Pa), surface level pressure (Pa), total ozone (in a column extending from the surface of the Earth to the atmosphere; kg/m$^2$), eastward and northward components of wind speed at a 10m altitude (m/s$^2$). We also have five static covariates that do not change with time: anisotropy (unitless), slope (unitless), angle (radians), and standard deviation (unitless) of the orography within a grid-cell, and a land-sea mask (unitless) which measures the proportion of land contained within a grid-box. The ordering of the covariates in the last dimension of `X` is determined by the vector `cov_names`.

We perform a bootstrap analysis using `bootstrap.R`, with the source functions provided in `bGEV_loss_functions.R`. The user must specify `boot.num`, which will determine the seed for the bootstrap sampling scheme. In the handbook chapter, we consider `boot.num` ranging from 1 to 200. For each bootstrap sample, we fit a bGEV regression model to the resampled data, and save the predictions, i.e., the parameter estimates, in `Predictions/`. The script `application_plots.R` then compiles these estimates into the plots provided in the handbook chapter.

Note that fitting the deep bGEV model to each bootstrap sample can be computationally intensive, and will take 1-2 hours on a laptop using CPUs. Taking a smaller subsample from the dataset if recommended if you are not that patient!

```bash
├── bGEV_loss_functions.R          (Functions that comprise the bGEV loss function)
├── bootstrap.R                    (Estimates the deep bGEV model for a single bootstrap sample)
├── monthly_max_data.Rdata         (Precipitation data and covariates)
├── application_plots.R            (Creates plots for Figures 1.5, 1.6, and 1.7)
├── Predictions/                   (Contains bootstrap parameter estimates)
├── Figures/                       (Contains figures)
```
