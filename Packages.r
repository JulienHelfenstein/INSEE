packages = c("synthpop", "ggplot2", "dplyr", "timeR", "tidyr", "purrr", "psych", "corrr", "ggbiplot", "devtools", "vcd", "rcompanion", "corrplot", "psych", "tidyverse")

installed_packages = packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))
