# NichePac - Niche Analysis 2023

**DFG Project:** NichePac <br/>
**Author:** Anna Rönnfeldt <br/>
**WP1:** Quantification of the niche dynamics of alien plant species in the Pacific region. <br/>


## Workflow

Base for the species list used in the analysis is the [PaciFLora](https://bdj.pensoft.net/article/67318/) data set. It lists 3963 plant species known to occur as naturalised species in the Pacific Region. 

### 1 - Download species data
The global occurrence data for these species were downloaded from [BIEN](https://biendata.org/) and [GBIF](https://www.gbif.org/) in June 2023.

### 2 - Data preparation
The downloaded occurrence records from the two sources were then cleaned to remove duplicates and occurrences with erroneous time stamps or coordinates, and harmonised to combine them into one data set at a 1 km resolution. 

### 3 - Assign invasion status
The main source for information on the invasion status (native, introduced, contradictory or unknown) of a species occurrence is the [World Checklist of Vascular Plants (WCVP)](http://www.plantsoftheworldonline.org/) at the base resolution of level 3 of tdwg. To close data gaps, two additional sources were used: [Global Inventory of Floras and Traits (GIFT)](https://gift.uni-goettingen.de/home) and [Global Naturalized Alien Flora (GloNAF)](https://glonaf.org/). <br/>

Merging the status information from these three sources resulted in some conflicts. If the sources that caused the conflict referred to areas of different sizes (e.g., Honshu and Japan), the status from the source referring to the smaller area is used. If the sources refer to the same area size, two cirteria can be used to deal with conflicts:

* **Criterion 1**: All sources are weighted equally. Occurrences with conflicts in the status assignment get the status *contradicctory*.
* **Criterion 2**: Status information provided by WCVP is perferred, because it has been targeted towards the level 3 resolution of tdwg.

### 4 - Splitting data into regional occurrences

For each species all occurrences are split into smaller regional subsets based on their invasion status: one subset with all *native* occurrences, and then the *introduced* occurrences for the Pacific Islands, and seven of the level 1 twdg regions: Australasia, Africa, tropical Asia, temperate Asia, Europe, North America, and South America. Occurrences assigned the status *contradictory* or *unknown* were excluded. 

### 5 - Sampling backround data and data thinning

To reduce the run time for this computationally intensive step, a pre-selection of species is done. Only species with at least 20 occurrences in their native range, as well as >= 20 introduced occurrences in the Pacific Region and at least one of the other regions respectively are suitable for the following niche comparison. For each remaining species (n = XX), background data is sampled within a 200 km buffer surrounding the presence points, with ten times as many pseudo-absences than presences. Both presences and pseudo-abensces are then thinned using a 3 km threshold to avoid spatial-autocorrelation. 

### 6 - Final species selection
The final species selection returns a list of species with enough occurrences - following the above described selection criterion - to still be suitable for the analysis (n = xx). 

### 7 - Merging occurrences with climate data
Each occurrence point is then matched with the according climate data. For this, all 19 bioclimatic variables from [CHELSA V2](https://chelsa-climate.org/) were used at a 1 km resolution. The resulting data is the final input for the following niche comparison.

### 8 - Niche comparison
The niche comparison is in large parts based on the R package *ecospat*. Niche differences between native and non-native niche are quantified for each niche pair (n = XX) resulting from the introductions of the xx study species when introduced to different regions. If a species has been introduced to five regions, this results in five distinct comparisons with the species native niche. 

The niche comparison runs through the following analysis steps:
* perparing the **PCA environment** and calculating the occurrence density grids which are the base for the next steps
* calculating **niche overlap** metric Schoener's D
* **similarity test** - are niche similarities higher or lower than expected by chance?
* calculating the standardised effect size (**SES**) output of the similarity test
* determining the **niche dynamcis**

The results obtained for the individual niche pairs are then compiled in a separate script. There, required post-processing steps take place, e.g., the calculation of the **relative niche dynamics** from the original *ecospat* output.

### 9 - Geographic characteristics of the native range
~ pending ~

### 10 - Species traits
~ pending ~
### 11 - Trait analysis
~ pending ~
### 12 - Visualising the results
~ pending ~
