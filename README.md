# BES_SupplementaryMaterial
Supplementary Material for reproducing the results of the paper "Assessing the well-being spatial association over time: an empirical illustration on Italian provinces based on BES data".

In the supplementary material you will find:

Source files
--------
1_data.R (Importing the data, translate in english, creating the Prior Spatial Matrices and output)

Sampler_Factor.R (Gibbs Sampler estimating the posterior distribution of the model's parameters)

functions.R (additional function for processing data)

Data files
--------
Imputed_BES_dataset.csv (Imputed Province BES dataset)

Imputed_BES_dataset.csv (Imputed Province BES dataset normalized)

Location_BES.csv (Longitude and latitude for Italian Provinces)

italgeo.shp (Shape file with Italian province geographical borders)

ord_name.RData (R file with ordered indicator names)

Implementation files
--------

2_model_implementation_BES.R (Implementing the function estimating Social-Economic-Environmental latent well being indicators and factor loadings)

3_delta_df_d.R (Creating dataframe with estimated Social-Economic-Environmental latent wellbeing for each Italian province)

4_delta_overall_df.R (Implementing the function estimating the Overall latent well being, create the dataframe)

5_hierarchical_stan.R  (Stan Code for estimating hierarchical model, i.e. well-being for Italian Macro Areas across time)

Additional Results
--------
diagnostic.R (Script for Sampler's Diagnostic Convergence)

spatial_EDA.R (Script for Spatial Exploratory Data analysis)

results_BES.R (Script for posterior distribution summaries, plots and maps)
