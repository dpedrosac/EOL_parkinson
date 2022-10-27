# This is code is intended to install all packages which are necessary 
# to run all analyses for the palliative care project;
# Code developed by Anna and David Pedrosa

# Version 1.0 # 2022-10-28, first commit


## Now load or install & load all iff necessary
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
