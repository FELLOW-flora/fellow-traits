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

These four steps will be run automatically with this command in R/RStudio (approx. 5 minutes): 

```r
source("make.R")
```

An exploratory analysis is carried out in the file [analysis/05_explore_traits.pdf](https://github.com/FELLOW-flora/fellow-traits/blob/main/analyses/05_explore_traits.pdf).


## Methodological choice

1. The taxonomic backbone is [TaxRef v18.0](https://inpn.mnhn.fr/telechargement/referentielEspece/taxref/18.0/menu), completed with [GBIF](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c) (based on Catalogue of Life). The taxonomic description of all taxa listed in Fellow are in [derived_data/species_list_taxo.csv](https://github.com/FELLOW-flora/fellow-traits/blob/main/data/derived-data/species_list_taxo.csv)
2. A list of known synonyms was built from TaxRef and GBIF. We removed ambiguous synonyms, e.g. one synonym refers to a single accepted name only; and loops, e.g. an accepted name can not be a synonym.
3. For taxa with multiple matches in the trait database, we take the average values for numerical traits, and concatenate the different categories for categorical traits.
4. For genus with no trait information, we aggregate values of species that are listed in the taxa list.  


In summary, the source of trait information is:   

- direct match between taxa  
- if not available, look for known synonyms  
- if not available, remove subspecies or variety (consider only binomial names)  
- if not available and taxa is genus, look for all species of the same genus within the species list   

In all cases, the original name of the taxa in the trait database is kept as a variable 'original_name' (and when multiple taxa area aggregated, the taxa are collated).  


## To do

1. Update the metadata of traits in [metatraits.xlsx](https://github.com/FELLOW-flora/fellow-traits/raw/refs/heads/main/data/raw-data/traits/Metatraits.xlsx): select only relevant traits, add missing traits or trait databases, coherently rename traits that are similar across different database (to simplify later merging).  
2. Further develop sanity checks and comparison across databases

