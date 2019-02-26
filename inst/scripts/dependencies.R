library(data.table)
library(magrittr)
library(caTools)
library(bitops)
library(knitr)
library(base64enc)
library(rmarkdown)
library(kableExtra)
library(Rcpp)
library(matrixStats)

if(!require(CelloLoad)) {
  # devtools::install_github('CelloHealthInsight/CelloWrangle')
  devtools::install_github('CelloHealthInsight/CelloLoad', auth_token = GITHUB_PATH)
  library(CelloLoad)
}