# Data and analysis for the Click-and-drag belief interface paper

This repository contains data and analysis for the Paolo Crosetto & Thomas De Haan paper "*Comparing input interfaces to elicit belief distributions*". It allows anyone to reproduce all the analyses carried out in the paper and to download the data for further analysis.

If you want, you can [download the paper here](https://ekstern.filer.uib.no/svf/Econ%20web/2022/08%202022.pdf)

## Dependencies
To run the analysis you need R and the following packages (available on CRAN):

- `tidyverse` -- a set of tools to work with tidy -- i.e. well behaved -- data. Two extra packages from the `tidyverse` collection are also needed, `scales` and `magrittr`.
- `broom` -- a library to tidy the output of tests, regressions, so that it can be used with tidyverse
- `kable` and `kableExtra` -- a tool to make beautiful tables
- `hrbrthemes` and `ggtext` -- a set of good-looking `ggplot` themes and tools to use markdown-formatted text in plots
- `syhuzet` -- a sentiment analysis text mining package

## How to run the analysis

- Download or clone this repository.
- Open the .Rproj file.
- Open and execute the Analysis.R file.

The analysis is fully carried out in the file Analysis.R. This file:

- loads the packages (do install them first if you do not have them yet)
- loads the data
- calls on individual scripts in the `/Scripts` folder to generate individual figures or tables

For each figure or table in the paper, there is one dedicated file. The files are self-standing and can be executed in any order.

### Figures
Figures are saved to the `Figures/` folder. They are the high-resolution images (and do not fit well in the github preview screen) included in the paper. This repo also includes the extra figures created for presentation purposes; those are not included in the paper, and are a visual representation of the results that are mainly exposed using tables in the paper. 

### Tables
Tables are saved to the `Tables/` folder. They are .pdf version of the latex-compiled tables included in the paper. 

## License

Creative Commons Attribution-NonCommercial-ShareAlike -- CC BY-NC-SA

