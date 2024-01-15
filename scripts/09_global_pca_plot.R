
# Author: Anna Rönnfeldt
# Date: 2023/11/23
# Script: plotting the variable contribution to the global PCA


# preamble ----------------------------------------------------------------

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "ecospat" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

rm(list = ls())

# required data -----------------------------------------------------------

path_imp <- file.path("/import/ecoc9/data-zurell/roennfeldt/C1/")

# load global pca 
load(paste0(path_imp, "output/PCA/global_pca.RData")) # object name: pca_env_global


# cor plot ----------------------------------------------------------------
pdf(paste0(path_imp, "plots/pca_global_cor_plot.pdf"))

ecospat.plot.contrib(contrib = pca_env_global$co, eigen = pca_env_global$eig)

dev.off()
