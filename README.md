# Data and analysis for the Click-and-drag belief interface paper


Data and analysis scripts to replicate the results of the paper presenting the Click-and-drag belief elicitation interface by Thomas De Haan and me

This repository contains data and analysis for the Paolo Crosetto & Thomas De Haan paper "ADD TITLE". It allows anyone to reproduce all the analyses carried out in the paper and to download the data for further analysis.

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
- calls on individual files to generate individual figures or tables

For each figure or table in the paper, there is one dedicated file. The files are self-standing and can be executed in any order.

### Figures
Figures are saved to the Figures/ folder. They are the high-resolution images (and do not fit well in the github preview screen) included in the paper. This repo also includes the extra figures created for presentation purposes; those are not included in the paper, and are a visual representation of the results that are mainly exposed using tables in the paper. 

### Tables
Tables are saved to the Tables/ folder. They contain the exact same information as in the paper, and they are formatted for use in the LaTeX source of the paper (i.e. they might not look that good on your screen. 

## License

Creative Commons Attribution-NonCommercial-ShareAlike -- CC BY-NC-SA see here

