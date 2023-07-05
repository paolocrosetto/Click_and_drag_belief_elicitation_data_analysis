# Data and analysis for the Click-and-drag belief interface paper

This repository contains data and analysis for the Paolo Crosetto & Thomas De Haan paper "*Comparing input interfaces to elicit belief distributions*". It allows anyone to reproduce all the analyses carried out in the paper and to download the data for further analysis.

If you want, you can [download the paper here](https://ekstern.filer.uib.no/svf/Econ%20web/2022/08%202022.pdf)

## Dependencies

To run the analysis you need R and the following packages (available on CRAN):

-   `tidyverse` -- a set of tools to work with tidy -- i.e. well behaved -- data. Two extra packages from the `tidyverse` collection are also needed, `scales` and `magrittr`.
-   `broom` -- a library to tidy the output of tests, regressions, so that it can be used with tidyverse
-   `kable` and `kableExtra` -- a tool to make beautiful tables
-   `hrbrthemes` and `ggtext` -- a set of good-looking `ggplot` themes and tools to use markdown-formatted text in plots
-   `syhuzet` -- a sentiment analysis text mining package

## How to run the analysis

-   Download or clone this repository.
-   Open the .Rproj file.
-   Open and execute the Analysis.R file.

The analysis is fully carried out in the file Analysis.R. This file:

-   loads the packages (do install them first if you do not have them yet)
-   loads the data
-   calls on individual scripts in the `/Scripts` folder to generate individual figures or tables

For each figure or table in the paper, there is one dedicated file. The files are self-standing and can be executed in any order.

## `Original oTree data` vs `Data` folders

For full transparency, we provide two different data files and folders.

In `Original oTree data` we provide the raw dump that we got from oTree at the end of the sessions. It is a csv formatted the oTree way -- lots and lots of variables, as each screen is recorded over several variables. In that folder there is also an `import_data.R` script that takes the oTree data and cleans them so that they are in a nice, rectangular, panel format amenable to analysis.

In `Data` you can find the clean data file -- i.e., the result of running `import_data.R` on the original oTree data. All further analyses rely on this second, clean data file.

## Variable codebook

| Variable       | Type      | Description                                                                                                                              |
|----------------|-----------|------------------------------------------------------------------------------------------------------------------------------------------|
| `ID`           | character | Unique subject identifier                                                                                                                |
| `trial`        | integer   | Unique trial identifier for each subject                                                                                                 |
| `time`         | integer   | Time allotted to finish the task (15 or 45 seconds)                                                                                      |
| `second`       | integer   | Snapshot of the data after `seconds` seconds have passed                                                                                 |
| `payoff`       | float     | Payment in a given `trial`                                                                                                               |
| `delay_ms`     | integer   | Time (in millisecond) the subject interacted with the interface                                                                          |
| `score`        | integer   | 1 - distance to the target attained at time `second`                                                                                     |
| `nclicks`      | integer   | Number of times subject interacted with the interface                                                                                    |
| `nbins`        | integer   | Number of bins of the given trial (7,15,30)                                                                                              |
| `shape`        | character | Shape of the given trial (Symmetric, Skewed, Bimodal, Random)                                                                            |
| `treatment`    | character | Between-subject treatment: type of elicitation interface (bins = slider, number = text, metaculus = distribution, ours = click and drag) |
| `final_payoff` | float     | Amount earned by the subject at the end (\$)                                                                                             |
| `device`       | character | Type of device used for the task (desktop, smartphone)                                                                                   |
| `os`           | character | Operating System of the subject (Linux, WIndows, Mac)                                                                                    |
| `CQerrors`     | integer   | Number of times subjects tried to validate control questions and failed (0,1,2; at 3 errors they were out)                               |
| `age`          | integer   | Age of the subject (years)                                                                                                               |
| `gender`       | character | Gender of the subject                                                                                                                    |
| `easy`         | integer   | LIkert scale (1-7): how hard to use did you find the interface?                                                                          |
| `frustrating`  | integer   | LIkert scale (1-7): how frustrating did you find the interface?                                                                          |
| `understood`   | integer   | LIkert scale (1-7): how difficult was to understand the instructions for the interface?                                                  |
| `commentary`   | character | Free text asking subjects to comment on our experiment                                                                                   |
| `keyboard`     | integer   | Did subjects use a keyboard? (0 = no, 1 = yes)                                                                                           |
| `mouse`        | integer   | Did subjects use a mouse? (0 = no, 1 = yes)                                                                                              |
| `touchpad`     | integer   | Did subjects use a touchpad? (0 = no, 1 = yes)                                                                                           |
| `touchscreen`  | integer   | Did subjects use a touchscreen? (0 = no, 1 = yes)                                                                                        |
| `slack`        | integer   | Number of trials in which the subjects did not interact at all (0-24)                                                                    |

### Figures

Figures are saved to the `Figures/` folder. They are the high-resolution images (and do not fit well in the github preview screen) included in the paper. This repo also includes the extra figures created for presentation purposes; those are not included in the paper, and are a visual representation of the results that are mainly exposed using tables in the paper.

### Tables

Tables are saved to the `Tables/` folder. They are .pdf version of the latex-compiled tables included in the paper.

## License

Creative Commons Attribution-NonCommercial-ShareAlike -- CC BY-NC-SA
