# segnet
This repositories contains the replication files for:
Mele, Angelo (2019), "Does school desegregation promote diverse interactions? An equilibrium model of segregation within schools", American Economic Journal: Economic Policy.

The paper provides an empirical application of the model developed in 
Mele, Angelo (2017), "A structural model of dense network formation", Econometrica, 85(3), pp 825-850

Data are from the National Longitudinal Study of Adolescents Health (Add Health). Because of the sensitive nature of the data they are available under contract from UNC Chapel Hill. Researchers interested in the data can apply at https://www.cpc.unc.edu/projects/addhealth/documentation/restricteduse 

## 1. extract data
Before running the R scripts, you need to use Stat Transfer to transform the .stc files into .xpt files. 

XXXXXXXXXXXXX need to list the specific files for Stat Transfer

Then run the following file to extract the data and save in .csv.

extract_addhealth.R



## 2. create variables and datasets
Need to run the following scripts in sequence.

1. sample_addhealth.R : this will collect the sample used in estimation.

2. datasets_addhealth.R: this will generate the datasets

This will create the files used for descriptive stats.

The final datasets for the estimation using Rmpi and ergm are obtained by running the following script.

3. datasets_for_estimation.R: this create the files for estimation using parallelization through the package Rmpi.

This will create the files used in estimation. It creates school fractions of each race and edge covariates to include in the
formula for estimation. It will generate a set of .Rdata files from the saturated sample of Add Health, which is the one used in the estimation exercise

## 3. plots
link to file

## 4. estimation 

### Rmpi package in r
To run the estimation we need to use MPI in R. Notice that your installation will depend on the OS you are using. My codes were run on a Windows machine, with Windows 7.

[Rmpi package installation](http://fisher.stats.uwo.ca/faculty/yu/Rmpi/)


### Custom network statistics in ergm
The payoff functions for the indirect links are not included
in the standard `r ergm` package. To add these statistics you need to 
download the `r ergm.userterms` package, designed to include new statistics.

The instruction on how to use this package are here: LINK TO JOSS.

Our function is here: LINK TO ind_homophily

And the corresponding .c code is here: LINK TO ind_homophily.c

### Estimation codes
The codes for estimation are:

*Table 1*

[run 1](Rmpi_estimation_run1.R)
Rmpi_estimation_run2.R 
Rmpi_estimation_run3.R 

We run the code 3 times, to find a good starting value and to improve the 
proposal distribution. In the first run we use a random walk proposal with
diagonal variance, that is each parameter is independent. This is clearly suboptimal. 
We use 10000 parameter simulations and 1000 network simulations per parameter. This finds
a suitable starting value in few hours. We then use the estimated series of parameters to compute the covariance matrix to input in the random walk for the second run.
We repeat for another run, and in the third run we use 100000 simulations and 5000 network simulations. 



TABLE 2

Rmpi_estimation_dirutil_run1.R
Rmpi_estimation_dirutil_run2.R

3.
4.

## 5. policy counterfactuals
link to file

