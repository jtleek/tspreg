tspreg
======

An R package for performing top-scoring pairs regression. The basic idea is to look for pairs of genes that 
switch rankings between phenotypes. The tspreg package allows the discovery of top-scoring pairs for multi-class
classification and for survival classification. 


Installation
======


```S
## If needed
install.packages("devtools")

## Install pre-requisite packages from Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("genefilter")

source("http://bioconductor.org/biocLite.R")
biocLite("tspair")

## Install the survhd package from Bitbucket

install_bitbucket(repo="survhd",user="lwaldron",ref="default")

```

Use
======

```S
library(tspair)
library(tspreg)
data(tspdata)

## Build a top-scoring pairs regression predictor

tspr1 <- tspreg(dat,as.factor(grp),npair=1)

# Shows the pairs of indices contributing to the predictor

tspr1$index

# Create the data frame for prediction

predictors <- calculateTspairs(dat,tspr1$index)

# Fit the linear model and predict

modelFit <- lm(grp=="diseased" ~ .,data=predictors$pairMat)


# Get the pairs on the test data set

validationPredictors <- calculateTspairs(dat2,tspr1$index)

# Apply predictor to test data set

predict(modelFit,newdata=validationPredictors$pairMat) > 0.5

```


Citation
======

Please use the following to find citations for the tspreg package `citation("tspreg")`.





