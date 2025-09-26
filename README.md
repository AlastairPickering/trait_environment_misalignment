# Forest Trait Environment Misalignment Repository

This repository explores how functional composition affects trait-environment misalignment under climate change and identifies communities where projected climate conditions create the strongest mismatch between current and future community-level functional composition. The main model is implemented in the `trait-environment misalignment.ipynb` notebook.

Due to the large size of the data (~31 GB), the data/ folder and its subfolders are not included in the repo clone. They’re zipped, split, and attached to a GitHub Release (data-v1.0.0). To use the project, download the data assets from the Release and extract them into the repository root so that a data/ folder sits alongside code/ and output/. For split archives, download all .z01, .z02, … parts and the corresponding .zip index, then unzip the index file; your unzip tool will read the parts automatically. Ensure you have ~35–40 GB free for download and extraction.

---

## Project Components

### Code
- **forest_trait_means_data_creation.R:**  
  An R script that creates the forest trait and static geographic variables. 
  
- **extract_climate_data.R:**  
  An R script that extracts climate data from .tif files located in the `data/climate` directory, merging them with the forest trait data.

- **trait_environment_misalignment.ipynb:**  
  The main Jupyter Notebook that models forest community-weighted trait means (CWMs) as a function of abiotic variables, predicts changes in CWMs under future climate scenarios, and identifies the most functionally vulnerable plots, communities and regions.
  
- **plotting**
  Folder containing plotting scripts to recreate figures from the paper
  
- **sensitivity**
  Scripts to run leave one group out cross validation and bootstrapped resampling of model

### Data
- **climate:**  
  Contains CHELSA BIO climate .tif files (temperature, precipitation, extremes) for different scenarios:
  - **current:** Current climate data (1980-2010).
  - **ssp126:** Climate projections under SSP126.
  - **ssp370:** Climate projections under SSP370.
  - **ssp585:** Climate projections under SSP585.

- **metadata:**  
  Contains metadata files related to the project including FIA forest types and North American tree species
  
- **traits:**  
  Three trait databases:
  - `trait_syndromes.csv` - taken from Rueda et al., 2018
  - `myco_trait.csv` - taken from Averill et al., 2022
  - `physiological_traits.csv` - taken from Maynard et al., 2022 
  
- **forest_plots:**  
  Contains plot-level forest inventory data:
  - **Canada:** Data specific to Canadian forest plots.
  - **US:** Contains the `cleaned_state_data` subdirectory with data for US states.
  
- **precomputed:**  
  Computationally expensive data such as bootstrapped runs that has been precomputed for ease of use

### Output

Folder containing data that has been processed by running the scripts

---

## Usage Instructions

1. **Data Preparation:**  
   Run `forest_trait_means_data_creation.R` to generate the forest trait and static geographic variables. This script produces a base CSV file (`forest_trait_means.csv`) that is used for further analysis.

2. **Extract Climate Data:**  
   Execute `extract_climate_data.R` to extract climate data from the `.tif` files in the `data/climate` directory. The extracted data is merged with the forest trait data and exported as CSV files to the `data/precomputed` directory for each climate scenario.

3. **Analysis:**  
   Open the `trait_environment_misalignment.ipynb` Jupyter Notebook to run the models and make the predictions as per the findings reported. 

4. **Visualisation and Sensitivity Analysis:**  
   Additional analysis routines and plotting functions can be found in the `plotting` and `sensitivity` directories under the `code/forest_trait_means_data_creation.R` file.

---

## Contact

For further information or any queries regarding this project, please contact Alastair Pickering (pickeringalastair@gmail.com)
