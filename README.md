# Figures for The Samplr Package: A Tool for Modeling Human Cognition with Sampling Algorithms
This repository contains code to produce figures in Castillo et al. (n.d). 

## Installation
This R repository uses the `renv` package to ensure a reproducible environment. It was run using R version 4.3.3. You can see a list of all packages used in the `renv.lock` file. 

To start, open the `samplr-paper.Rproj` file in RStudio, which will automatically run `.Rprofile`, thus installing the `renv` package if needed. 
Then, run 
```r
renv::restore()
``` 
which will install the needed packages. These packages will be installed in the `./renv/library/` folder, thus not affecting your R environments elsewhere. Read more about `renv` [here](https://rstudio.github.io/renv/articles/renv.html).

## Citation
To cite this work, cite
```
@misc{castilloSamplrPackageTool,
  title = {The Samplr Package: {{A}} Tool for Modeling Human Cognition with Sampling Algorithms},
  author = {Castillo, Lucas and Li, Yun-Xiao and Sanborn, Adam N},
  copyright = {All rights reserved}
}

```