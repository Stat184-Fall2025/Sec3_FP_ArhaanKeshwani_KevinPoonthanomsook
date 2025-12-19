# Rocket League Pro Performance & Earnings Analysis

This repo examines professional Rocket League players’ in-game stats and compares them with their earnings to identify performance trends.

## Overview
This project analyzes in-game stats of professional Rocket League players alongside their tournament earnings to investigate how measurable stats relate to success measured through earnings. The goal is to uncover trends, correlations, and predictors of earnings, providing insights for esports analysts, and fans interested in understanding what performance factors "matter most" at a pro level.

### Interesting Insight (Optional)

This is optional but highly recommended. You'll include one interesting insight from your project as part of the README. This insight is most effective when you include a visual. Keep in mind that this visual must be included as an image file (e.g., JPG, PNG, etc.). You can export plots created with `{ggplot2}` by using the function `ggsave`.

## Data Sources and Acknowledgements

Rocket League pro player earnings data: https://www.esportsearnings.com/games/409-rocket-league
Rocket League pro player in-game stats: https://docs.google.com/spreadsheets/d/1XeDEL-1jZ-P4gTezOREV11O4bRiFXlBXRhc1tXSPx1g/edit?gid=139406561#gid=139406561

## Current Plan

Prepare data for exploratory analysis
1. Srape both datasets from the web
2. Wrangle the data so that we have a final data frame where a case is a combintation of a Rocket League pro player with their stats and earnings
3. Document our wrangling and scraping process
4. Ensure that all members are aware of the style guide we are using, tidyverse

Setup our Github Repository
1. Create appropriate branches for each member
2. Write out our README file
3. Make sure each member knows how to use Github properly and understands how to commit

Setup our QMD file
1. Using R create a QMD file and make sure the output is set to a PDF file
2. Write out a YAML header with appropriate parameters such as title and authors

Initial Exploratory data analysis
1. Here we are looking to get familiar with our data before moving past EDA
3. We will create two scatterplots: one comparing average points per game to earnings, and another comparing average goals per game to earnings.
4. Add trend lines to see what has a higher correlation
5. Make sure code is properly formatted and documented

Work in Progress presentation
1. Create an visually appealing slideshow
2. Have one slide be dedicated to showcase what we are interesed in along as what we plan to learn from our data
3. Have another slide showcase our EDA visualizations and the inight we have so far
4. Finally, in our last slide, we discuss potential questions and analyses to investigate beyond our exploratory data analysis
5. Choose a presentation time slot and rehearse
6. Present

Final Exploratory data analysis
1. Per instructions of our assignment we need to create two more tables and one more data visualization
2. First create a summary table of in game statistics of pro players
3. Next create a correlation table that shows how each in game statistic is correlated to total earnings
4. Finally create a scatterplot that visualizes the performance and earnings of players, showing the relationship between their average goals, average assists, earnings, and average saves

Create the final QMD file
1. Add code chunks for each of our visualizations
2. Make sure there's captions, alt text, and narrataive text
3. Add a code appendix
4. Add citations
5. Render as PDF and submit


## Repo Structure

This repo has a main branch for stable, tested code, and two separate branches, one for each developer to work independently. Developers make changes on their branches and merge into main once ready.


## Authors

Arhaan Keshwani
Email: ask6023@psu.edu

Kevin Poonthanomsook
Email: kzp5692@psu.edu

