# fellow-traits
Research compendium for extracting trait values from a list of interesting species of French flora


## General

This repository is structured as follow:

- :file_folder: &nbsp;`analyses/`: contains R scripts to prepare the dataset and make the analysis;
- :file_folder: &nbsp;`data/`: contains raw and derived data;
- :file_folder: &nbsp;`R/`: contains home made functions for the analysis;


## Usage

The analysis is divided in four sequential steps:  

1. combine survey's species lists into a 'fellow' species list
2. clean species name using TaxRef and GBIF and evaluate possible synonyms
3. extract trait values for the species of interest (one file per trait database)
4. merge extracted trait databases into a single 'fellow' trait database  

These four steps will be run automatically when run this command in R/RStudio (approx 5 minutes): 

```r
source("make.R")
```


An exploratory analysis was carried out in the file [05_explore_traits.qmd](analyses/05_explore_traits.qmd).


## To do

1. Update the metadata of traits in [metatraits.xlsx](https://github.com/FELLOW-flora/fellow-traits/raw/refs/heads/main/data/raw-data/traits/Metatraits.xlsx): select only relevant traits, add missing traits or trait databases, coherently rename traits that are similar across different database (to simplify later merging).  
2. Decide how to handle taxa that are not defined at species level (families, genus, subsepecies and varieties)
3. Better handle numerical traits (make checks to be sure that they are properly handled)
4. Decide how to handle mutliple values in trait database and synonyms (e.g. Silene latifolia is accepted synonyms of Atocion armeria yet different values in Midolo)
5. Further develop sanity checks and comparison across database


source of information
- best if available for "original name"
- if not, synonyms


## Issues
Load trait data
Clean taxa name (homogenize to maximize number of matches)
Replace by synonyms (if the accepted is not in the original db)
