# Figures for The Samplr Package: A Tool for Modeling Human Cognition with Sampling Algorithms
This repository contains code to produce figures in Castillo et al. (2025). 

## File structure
Each of the `*.R` files produces one of the figures in the paper and stores them in `plots/`. These files can be run in any order. Figure 1 was produced manually using a vector graphics editor ([Inkscape](https://inkscape.org/)), and is stored in `plots/fig1.svg`. Figure 5 is reproduced from Sanborn et al. (2025). Noise in cognition: Bug or feature? *Perspectives on Psychological Science*. https://doi.org/10.1177/17456916241258951

The rest of the figures are produced by the `*.R` files as follows:

- Figure 2: `1. Probability Judgments.R`
- Figure 3: `2. Tapping Estimates.R`
- Figure 4: `3. Random Generation.R`
- Figure 6: `4. Choice and RT.R`
- Figure 7: `5. Over-precision.R`
- Figure 8: `6. Confidence judgments.R`
- Figure 9: `7. Repulsion effect.R`
- Figure 10: `8. Random Generation ABC.R`


The `src/theme.R` function sets a ggplot theme used by all the figures. Other files in `src/` externalize functions for ease of reading (used by `5. Random Generation.R` and `8. Random Generation ABC.R`). Computations in `8. Random Generation ABC.R` take longer to compute and so results are stored in `cache/` for speed if re-running. 

## Installation
This R repository uses the `renv` package to ensure a reproducible environment. It was run using R version 4.5.3. You can see a list of all packages used in the `renv.lock` file. 

To start, open the `samplr-paper.Rproj` file in RStudio, which will automatically run `.Rprofile`, thus installing the `renv` package if needed. 
Then, run 
```r
renv::restore()
``` 
which will install the needed packages. These packages will be installed in the `./renv/library/` folder, thus not affecting your R environments elsewhere. Read more about `renv` [here](https://rstudio.github.io/renv/articles/renv.html).

## Citation
To cite this work, cite

**APA**: Castillo, L., Li, Y.-X., & Sanborn, A. N. (2025). The samplr package: A tool for modeling human cognition with sampling algorithms. *PsyArXiv*. https://doi.org/10.31234/osf.io/ax8hm_v1

**BibLaTeX**:
```
@online{castillo2025SamplrPackageTool,
  title = {The Samplr Package: {{A}} Tool for Modeling Human Cognition with Sampling Algorithms},
  author = {Castillo, Lucas and Li, Yun-Xiao and Sanborn, Adam N},
  date = {2025},
  doi = {10.31234/osf.io/ax8hm_v1},
}
```
