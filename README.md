# segnet
This repositories contains the replication files for:
Mele, Angelo (2019), "Segregation in Social Networks: A Structural Approach"

The paper provides an empirical application of the model developed in 
Mele, Angelo (2017), "A structural model of dense network formation", Econometrica, 85(3), pp 825-850

Data are from the National Longitudinal Study of Adolescents Health (Add Health). Because of the sensitive nature of the data they are available under contract from UNC Chapel Hill. Researchers interested in the data can apply at https://www.cpc.unc.edu/projects/addhealth/documentation/restricteduse 

## 1. extract data
Before running the R scripts, you need to use Stat Transfer to transform the .stc files into .xpt files. 

XXXXXXXXXXXXX need to list the specific files for Stat Transfer

Need to run the following script in sequence

extract_addhealth.R

This will extract the relevant data and save in .csv format

## 2. create variables and datasets
Need to run the following scripts in sequence.

sample_addhealth.R

datasets_addhealth.R

This will create the files used in estimation.
This creates also a set of files from the saturated sample, which is the one used in the estimation exercise

## 3. plots
link to file

## 4. estimation 
link to file

## 5. policy counterfactuals
link to file

