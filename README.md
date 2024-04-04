# NichePac - Niche Analysis 2023

**DFG Project:** NichePac <br/>
**Author:** Anna Rönnfeldt <br/>
**WP1:** Quantification of the niche dynamics of alien plant species in the Pacific region. <br/>


## Workflow

The analysis builds on the global occurrence data of the plant species listed in the [PaciFLora](https://bdj.pensoft.net/article/67318/) data set. Each occurrence point is matches with a biogeographic status (*native* or *introduced*). Code for the data preparation can be found in a seperate [repository](https://github.com/UP-macroecology/GlobalOccurrences.git). 


### 1 - Sampling backround data and data thinning

To reduce the run time for this computationally intensive step, a pre-selection of species is done. Only species with at least 20 occurrences in their native range, as well as >= 20 introduced occurrences in the Pacific Region and at least one of the other regions respectively are suitable for the following niche comparison. For each remaining species, background data is sampled within a 200 km buffer surrounding the presence points, with ten times as many pseudo-absences than presences. Both presences and pseudo-abensces are then thinned using a 3 km threshold to avoid spatial-autocorrelation. 

### 2 - First species selection to speed up next step 

### 3 - Merging occurrences with climate data

Each occurrence point is then matched with the according climate data. For this, all 19 bioclimatic variables from [CHELSA V2](https://chelsa-climate.org/) were used at a 1 km resolution. The resulting data is the final input for the following niche comparison. This is, again, only done if there are enough occurrences (following the criteria mentioned in seciton 5) remaining after the spatial thinning.

### 4 - Final species selection 

### 5 - Niche comparison
The niche comparison is in large parts based on the R package *ecospat*. Niche differences between native and non-native niche are quantified for each niche pair (n = XX) resulting from the introductions of the xx study species when introduced to different regions. If a species has been introduced to five regions, this results in five distinct comparisons with the species native niche. 

The niche comparison runs through the following analysis steps:
* perparing the **PCA environment** and calculating the occurrence density grids which are the base for the next steps
* calculating **niche overlap** metric Schoener's D
* **similarity test** - are niche similarities higher or lower than expected by chance?
* calculating the standardised effect size (**SES**) output of the similarity test
* determining the **niche dynamcis**

The results obtained for the individual niche pairs are then compiled in a separate script. There, required post-processing steps take place, e.g., the calculation of the **relative niche dynamics** from the original *ecospat* output.

### 6 - Trait analysis

~ pending ~

