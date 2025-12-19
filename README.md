# Operationalization of tools for the NOAA SWFSC Climate & Ecosystems Group

This repository serves as a centralized location for running scheduled jobs that provide input for a variety of operational tools. Specifically, it contains all code necessary to download environmental data (CMEMS) on a scheduled basis, make model predictions for relevant marine species, and generate products (e.g., rasters, images).

Here is an example prediction from Top Predator Watch:
![](https://github.com/joshcullen/CEG_operationalization/blob/main/model_prediction/TopPredatorWatch/img/leatherbackTurtle_2024-11-08.png?raw=true)

Based on need, this repo may change to account for additional data sources, models, and species of interest. In its current form, below is a directory tree to show how this repo will be structured:

```bash

├── DESCRIPTION
├── README.md
├── data_acquisition
│   ├── R
│   │   ├── acquire_cmems.R
│   │   ├── acquire_example.R
│   │   └── acquire_utils.R
│   └── netcdfs
│       └── cmems_ncdfs
├── data_processing
│   ├── R
│   │   ├── hw_test.R
│   │   ├── process_cmems.R
│   │   ├── process_example.R
│   │   ├── process_roms.R
│   │   └── process_utils.R
│   └── TopPredatorWatch
│       ├── rasters
│       └── static
├── metadata
│   └── model_metadata.csv
├── model_prediction
│   ├── R
│   │   ├── predict_ROMS.R
│   │   ├── predict_TopPredatorWatch.R
│   │   ├── predict_example.R
│   │   ├── predict_gulf.R
│   │   └── predict_utils.R
│   ├── TopPredatorWatch
│   │   ├── img
│   │   ├── rasters
│   │   └── static
│   └── gulf
│       ├── data
│       ├── predictions
│       └── results
├── repo_cleanup
│   └── delete_old_files.R
└── website
    ├── _extensions
    │   └── quarto-ext
    ├── _quarto.yml
    ├── docs
    │   ├── index.html
    │   ├── lbst_cmems.html
    │   ├── leatherback.html
    │   ├── robots.txt
    │   ├── search.json
    │   ├── site_libs
    │   └── styles.css
    ├── gulf
    │   ├── app.R
    │   └── pelagic_longline.qmd
    ├── hawaii
    │   └── placeholder.qmd
    ├── index.qmd
    ├── south_atlantic
    │   └── placeholder.qmd
    ├── styles.css
    ├── utils.R
    └── west_coast
        ├── cmems_app.R
        ├── lbst_cmems.qmd
        ├── leatherback.qmd
        └── placeholder.txt
```

