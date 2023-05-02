# vivian-erin-456

This repository was created by Vivian Powell and Erin Franke to study water contamination in the Twin Cities, specifically with a focus on superfund sites and PFAS in the Twin Cities. Superfund sites contain hazardous material including PFAS, which can have harmful affects on the health of the environment, humans, and other organisms. We created a Shiny app to identify where these superfund sites are located and how they have progressed in their cleanup of harmful chemicals overtime. 

Our repository is structured as follows. Our app is located in the *final_app* folder of the *project* folder. Our data is located in the *data* folder of our repository and was downloaded from the Minnesota Pollution Control Agency (MPCA) wesbite. Each superfund site's data is located within *superfund_site_data* folder of the *data* folder, and they are combined to one file called *superfund.rds*. The steps to create this file are located in the *dataCleaning.R* script. In our analysis, we also use a file provided by the MPCA called *Twin Cities PFAS Data Ticket WO00000021224685.xlsx* to get more specific data on PFAS. From this dataset, we filtered for the seven most popular PFAS and created the dataset *pfas7.csv*.

To run our analysis, we recommend installing the following R packages: tidyverse, readxl, sf, USAboundaries, shiny, shinythemes, lubridate, and plotly. 
