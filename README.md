# Operationalization of tools for the NOAA SWFSC Climate & Ecosystems Group

This repository serves as a centralized location for running scheduled jobs that provide input for a variety of operational tools. Specifically, it contains all code necessary to download environmental data (CMEMS) on a scheduled basis, make model predictions for relevant marine species, and generate products (e.g., rasters, images).

Here is an example prediction from Top Predator Watch:
![](https://github.com/joshcullen/CEG_operationalization/blob/main/model_prediction/TopPredatorWatch/img/leatherbackTurtle_2024-11-08.png?raw=true)

Based on need, this repo may change to account for additional data sources, models, and species of interest. In its current form, below is a directory tree to show how this repo will be structured:

```bash

EcoCast-Plus/operationalization/ (Repository Root)
│
├── .github/
│   └── workflows/                  # <-- THE AUTOMATION ENGINE
│       ├── acquire_cmems.yml       # (1) Daily Cron Job trigger (9:00 AM ET)
│       ├── predict_gulf.yml        # (2) Runs models after data acquisition
│       ├── update_website.yml      # (3) Rebuilds site after predictions
│       └── cleanup_repo.yml        # (4) Maintenance: Deletes old files daily
│
├── repo_cleanup/                   # NEW FOLDER
│   └── delete_old_files.R          # Script run by the cleanup workflow
│
├── data_acquisition/
│   ├── R/
│   │   └── acquire_cmems.R         # Script run by the first workflow
│   └── netcdfs/cmems_ncdfs/        # Raw downloaded environmental data (cleaned weekly)
│
├── model_prediction/
│   ├── R/
│   │   └── predict_gulf.R          # The core modeling script (loads data, predicts, saves TIFFs)
│   └── gulf/
│       ├── data/                   # Static inputs (bathymetry, shapefiles)
│       ├── results/                # Trained model objects (.rds files)
│       └── predictions/            # <-- THE OUTPUT DESTINATION (cleaned weekly)
│           ├── PRED_2026-01-07_sst.tif
│           ├── PRED_2026-01-07_Swordfish_Target.tif
│           └── ... (Daily TIFFs live here)
│
├── website/
│   ├── _quarto.yml                 # Website navigation configuration
│   ├── index.qmd                   # Homepage
│   ├── gulf/
│   │   └── pelagic_longline.qmd    # The page containing the iframe to Posit Connect
│   └── gulf_app_source/            # Source code for the live app
│       ├── app.R                   # <-- THE SHINY APP BRAIN (deployed to Posit Connect)
│       └── data/                   # App-specific static files (fishing area polygons)
```
