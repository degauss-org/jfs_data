# jfs_data <a href='https://degauss-org.github.io/DeGAUSS/'><img src='DeGAUSS_hex.png' align="right" height="138.5" /></a>

> DeGAUSS pipeline that generates one CSV file of aggregated, monthly data for JFS from a geocoded CSV file

[![Docker Build Status](https://img.shields.io/docker/automated/degauss/jfs_aggregated_data_report)](https://hub.docker.com/repository/docker/degauss/jfs_aggregated_data_report/tags)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/degauss-org/jfs_aggregated_data_report)](https://github.com/degauss-org/jfs_aggregated_data_report/releases)

## Instructions for Use

### Open the Starter App

1.  In Docker Desktop, search for "degauss/jfs_data" and "Pull" the container

2.  Once the container is in your local Docker Desktop, find it and click "Run". In the popup box, expand the options and in the space marked "Local Host", type in "3838" and click Run

3.  Navigate to the Containers page and in your line for the shiny_starter, click to button to open in browser

4.  Now that you're in the app, you can upload your .csv file

5.  After the upload, the app will show you a sample of your original file and the prepared file for use in the next program

6.  Click the button to download the prepped file. Save the downloaded file to a known location that you can navigate to (such as your downloads folder)

7.  Now in PowerShell, navigate to your Downloads folder (it may be as simple as typing "cd Downloads")

8.  Copy & paste the first Docker command into PowerShell and hit enter

9.  Repeat for the 2nd and 3rd Docker commands, all in the Downloads folder

10. After the 3rd command, you should have the final output, "monthly_report_v5.0.1.csv" that you can send along to Cole & Andrew


## Dataset Notes

- Screening status `SCREENED IN AR` included with `SCREENED IN`
- Intakes that either have no listed address or were unsuccessfully geocoded will be included as a "neighborhood" called `Missing`

## geomarker data

- census tract-level [deprivation index](https://geomarker.io/dep_index/) from 2015 ACS measures
- census tract-level population under age 18 from 2018 ACS
- `00_create_tract_to_neighborhood.R` (based on code from Stu) makes `tract_to_neighborhood.rds` which is used to convert tracts to neighborhoods
- `00_create_tract_to_neighborhood.R` also aggregates tract-level deprivation index (mean) and tract-level population under 18 (sum) to neighborhood and creates the neighborhood shapefile called `ham_neighborhoods_dep_index_shp.rds`
- Neighborhood level fields with values less than 5 have been censored for privacy considerations

## DeGAUSS details

For detailed documentation on DeGAUSS, including general usage and installation, please see the [DeGAUSS README](https://degauss.org/).
