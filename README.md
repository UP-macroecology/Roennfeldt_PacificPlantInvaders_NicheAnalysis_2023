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

### 6 - Trait data

To Do: inlcude Valén's code to show how data on functional traits and the time since introduction were compiled

#### Biogeographic characteristics:
* Native range size: sum of the area of the WCvP leve 3 polygons and/or the GIFT polygons with which occurrence points were matched during the status assignment
* Native niche centroid:  Centroid along the first two PCA axes relative to the global niche
* Native niche breadth: Shannon-Index of the occurrence density in the two-dimensional climatic ncihe space, using the global ncihe as background environment to make comparisons between species possible
* Distance between the range centrods of native and non-native ranges

#### Compiling input data for the trait analysis:
Data on the functional traits, biogeographic characteristics and the time since introduction were then matched with the corresponding niche metrics for the species' regional introductions.
Before using the data as input for the trait analysis, the following steps had to be taken:
* categorical data (growth type etc.) had to be transformed to numeric values
* mean seedmass and years since introduction had to log transformed because they were right skewed
* data for all traits had to be scaled
* the niche metrics were logit transformed to deal with values too close to 0 or 1

### 7 - Trait analysis

1. Merging input data with corresponding phylogenetic data
  
2. Run trait analysis for each niche mtreic and region respectively.
* fit full models using the niche metric as response and all ten traits as predictors
* stepwise selection to identify parsimoniuous models based on AIC values
  
3. Determine variable importance
* the importance of each variable was determined by randomly permuatting it (n = 99) to simulate the vaiable's absence from the model. The difference between explained deviance of the model with and the model without the permutation then shows the variable importance

The results of the trait analysis are plottet in a separate script. The figures are based on the corrplot function from the corrplot package, but with slight modifications to enable us to show the variable importance as circle size and the effect size with the colours. 

### 8 - Statistical analysis and additional figures

~ pending ~

