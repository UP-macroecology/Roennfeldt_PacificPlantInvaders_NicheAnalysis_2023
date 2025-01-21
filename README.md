# NichePac - Niche Analysis 2023

**DFG Project:** NichePac <br/>
**Author:** Anna Rönnfeldt <br/>
**WP1:** Quantification of the niche dynamics of alien plant species in the Pacific region. <br/>


## Workflow

The analysis builds on the global occurrence data of the plant species listed in the [PaciFLora](https://bdj.pensoft.net/article/67318/) data set. Each occurrence point is matches with a biogeographic status (*native* or *introduced*). Code for the data preparation can be found in a seperate [repository](https://github.com/UP-macroecology/GlobalOccurrences.git). 

### 0 - set up folder structure

Set up the folder structure required for the analysis using the [00_folder_structure](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/00_folder_structure.R) script.

### 1 - Split occurrence data into regional subsets
scripts [01_regional_occs_criterion_1](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/01_regional_occs_criterion_1.R), [01_regional_occs_criterion_2](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/01_regional_occs_criterion_2.R)

The occurrence data are split into different subsets: native occurrences, and non-native occurrences located within the eigth study regions respectively. Note, that  criterion 2 was not considered in the final analysis of this study. 

### 2 - Sampling backround data and data thinning
scripts [02a_first_species_selection](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/02a_first_species_selection.R), [02b_background_thinning_intr](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/02b_background_thinning_intr.R), [02b_background_thinning_nat](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/02b_background_thinning_nat.R)

To reduce the run time for this computationally intensive step, a pre-selection of species is done. Only species with at least 20 occurrences in their native range, as well as >= 20 introduced occurrences in the Pacific Region and at least one of the other regions respectively are suitable for the following niche comparison. For each remaining species, background data is sampled within a 200 km buffer surrounding the presence points, with ten times as many pseudo-absences than presences. Both presences and pseudo-abensces are then thinned using a 3 km threshold to avoid spatial-autocorrelation. 

### 3 - Merging occurrences with climate data
scripts [03_prep_input_intr](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/03_prep_input_intr.R), [02b_background_thinning_nat](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/02b_background_thinning_nat.R)

Each occurrence point is then matched with the according climate data. For this, all 19 bioclimatic variables from [CHELSA V2](https://chelsa-climate.org/) were used at a 1 km resolution. The resulting data is the final input for the following niche comparison. This is, again, only done if there are enough occurrences (following the criteria mentioned in seciton 5) remaining after the spatial thinning.

### 4 - Second species selection 
scripts [04_second_species_selection](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/04_second_species_selection.R)

Downsizing speies selection again, to speed up the analysis run time.

### 5 - Niche comparison
scripts [05_niche_comparison](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/05_niche_comparison.R)

The niche comparison is in large parts based on the R package *ecospat*. Niche differences between native and non-native niche are quantified for each niche pair (n = 1593) resulting from the introductions of the xx study species when introduced to different regions. If a species has been introduced to five regions, this results in five distinct comparisons with the species native niche. 

The niche comparison runs through the following analysis steps:
* perparing the **PCA environment** and calculating the occurrence density grids which are the base for the next steps
* calculating **niche overlap** metric Schoener's D
* **similarity test** - are niche similarities higher or lower than expected by chance?
* calculating the standardised effect size (**SES**) output of the similarity test
* determining the **niche dynamcis**

### 6 - Final species selection and cimpiling overview of results
scripts [06a_final_species_selection](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/06a_final_species_selection.R), [06b_results_overview](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/06b_results_overview.R)

A final downsizing of the species selection, based on a minimum number of occurrences (20) wiithin the analogue niche space of each niche pair. The results obtained for the remaining niche pairs are then compiled in a separate script. There, required post-processing steps take place, e.g., the calculation of the **relative niche dynamics** from the original *ecospat* output. This script also includes the statistical analysis linked to the regional differences of the niche dynamics. 

### 7 - Trait data

#### Time since introduction:
script [07a_trait_data_time](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/07a_trait_data_time.R)

The time since the initial introduction of the species' to the individual study regions is determined using data from the first record database by [Seebens](https://doi.org/10.12761/sgn.2016.01.022). 

#### Species functional traits:
script [07b_trait_data_species](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/07b_trait_data_species.R)

Functional trait data provided by the [GIFT](10.1111/jbi.13623) data base is downloaded via the *GIFT* R package. 

#### Biogeographic characteristics:
script [07c_trait_data_geo](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/07c_trait_data_geo.R)

* Native range size: sum of the area of the WCvP leve 3 polygons and/or the GIFT polygons with which occurrence points were matched during the status assignment
* Native niche centroid:  Centroid along the first two PCA axes relative to the global niche
* Native niche breadth: Shannon-Index of the occurrence density in the two-dimensional climatic ncihe space, using the global ncihe as background environment to make comparisons between species possible
* Distance between the range centrods of native and non-native ranges

#### Compiling input data for the trait analysis:
script [07d_trait_data_merge](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/07d_trait_data_merge.R)

Data on the functional traits, biogeographic characteristics and the time since introduction were then matched with the corresponding niche metrics for the species' regional introductions.
Before using the data as input for the trait analysis, the following steps had to be taken:
* categorical data (growth type etc.) had to be transformed to numeric values
* mean seedmass and years since introduction had to log transformed because they were right skewed
* data for all traits had to be scaled
* the niche metrics were logit transformed to deal with values too close to 0 or 1

### 8 - Trait analysis and related figures

#### Trait analysis
script [08a_trait_analysis](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/08a_trait_analysis.R)

1. Merging input data with corresponding phylogenetic data
  
2. Run trait analysis for each niche metric and region respectively.
* fit full models using the niche metric as response and all ten traits as predictors
* stepwise selection to identify parsimoniuous models based on AIC values
  
3. Determine variable importance
* the importance of each variable was determined by randomly permuatting it (n = 99) to simulate the vaiable's absence from the model. The difference between explained deviance of the model with and the model without the permutation then shows the variable importance

#### Trait analysis figures
script [08b_trait_figures](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/08b_trait_figures.R)

The figures are based on the corrplot function from the *corrplot* R package, but with slight modifications to enable us to show the variable importance as circle size and the effect size with the colours. 

### 9 - Native range characteristics
script [09_native_range_info](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/09_native_range_info.R)

Identifying the species' native ranges (by adding tags, mostly at a continental scale) and most prominent main climate classifications, to get a feeling for the species' native ranges.


### 10 - Figures
script [10_figures](https://github.com/UP-macroecology/Roennfeldt_PacificPlantInvaders_NicheAnalysis_2023/blob/main/scripts/10_figures.R)

---------------------------------------------------------------
**Required folder structure**
---------------------------------------------------------------

```
scripts
├── misc (includes script to recreate folder structure)

data
├── species_selection
├── occurrence_data
│   ├── regional_occs
│   ├── status_occs
│   ├── coords_final_nat
│   ├── coords_final_intr
│   ├── final_input_nat
│   ├── final_input_intr
├── status_assignment
│   ├── GloNAF 
├── spatial_data
│   ├── tdwg 
├── native_region_ID
├── trait_data
│   ├── PCA
│   ├── niche_breadth_centroid 
├── phylogenies

results
├── ecospat
│   ├── niche_overlap
│   ├── niche_similarity
│   ├── niche_dynamics
├── trait_analysis
│   ├── full_models
│   ├── main_analysis
│   ├── univariate

plots

```

## Operating system info
* R version 4.2.2 (2022-10-31 ucrt)
* Platform: x86_64-w64-mingw32/x64 (64-bit)
* Running under: Windows 10 x64 (build 19045)
* Attached packages: [1] ade4_1.7-22  [2] corrplot_0.92  [3] devtools_2.4.5  [4] doParallel_1.0.17  [5] dotwhisker_0.7.4  [6] dplyr_1.1.2  [7] ecospat_4.0.0  [8] fmsb_0.7.6  [9] foreach_1.5.2  [10] furrr_0.3.1  [11] ggplot2_3.5.1  [12] GIFT_1.0.0  [13] hrbrthemes_0.8.7  [14] lcvplants_2.1.0  [15] maps_3.4.1  [16] networkD3_0.4  [17] phylolm_2.6.2  [18] purrr_1.0.1  [19] RColorBrewer_1.1-3  [20] sf_1.0-16  [21] sfheaders_0.4.2  [22] stringr_1.5.0  [23] terra_1.7-78 [24] tidyr_1.3.0 [25] viridis_0.6.3 

## References
