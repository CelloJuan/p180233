if(!require(CelloLoad)) {
  # devtools::install_github('CelloHealthInsight/CelloWrangle')
  devtools::install_github('CelloHealthInsight/CelloLoad', auth_token = GITHUB_PATH)
  library(CelloLoad)
}
