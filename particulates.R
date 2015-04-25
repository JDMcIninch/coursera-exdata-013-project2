### Coursera / Exploratory Data Analysis / exdata-013 / Project 2

# Download and unzip the assignment data

zip.file <- 'NEI_data.zip'

if ( !file.exists(zip.file) ) {
    download.file(
        'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip',
        zip.file,
        method='curl'
    )
}

if ( !file.exists('Source_Classification_Code.rds') | !file.exists('summarySCC_PM25.rds') ) {
    unzip( zip.file )
}

if ( !file.exists('plots') ) {
    dir.create('plots')
}

### ASSIGNMENT
# 
# The overall goal of this assignment is to explore the National Emissions
# Inventory database and see what it say about fine particulate matter
# pollution in the United states over the 10-year period 1999–2008. You may
# use any R package you want to support your analysis.
# 
# Questions
# 
# You must address the following questions and tasks in your exploratory
# analysis. For each question/task you will need to make a single plot. Unless
# specified, you can use any plotting system in R to make your plot.
# 
# 1. Have total emissions from PM2.5 decreased in the United States from 1999
#    to 2008? Using the base plotting system, make a plot showing the total
#    PM2.5 emission from all sources for each of the years 1999, 2002, 2005,
#    and 2008.
# 
# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#    (fips == "24510") from 1999 to 2008? Use the base plotting system to make
#    a plot answering this question.
# 
# 3. Of the four types of sources indicated by the type (point, nonpoint,
#    onroad, nonroad) variable, which of these four sources have seen decreases
#    in emissions from 1999–2008 for Baltimore City? Which have seen increases
#    in emissions from 1999–2008? Use the ggplot2 plotting system to make a
#    plot answer this question.
# 
# 4. Across the United States, how have emissions from coal combustion-related
#    sources changed from 1999–2008?
# 
# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
#    Baltimore City?
# 
# 6. Compare emissions from motor vehicle sources in Baltimore City with
#    emissions from motor vehicle sources in Los Angeles County, California
#    (fips == "06037"). Which city has seen greater changes over time in motor
#    vehicle emissions?
# 
# Making and Submitting Plots
# 
# For each plot you should:
# 
# - Construct the plot and save it to a PNG file.
# 
# - Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the
#   corresponding plot, i.e. code in plot1.R constructs the plot1.png plot.
#   Your code file should include code for reading the data so that the plot
#   can be fully reproduced. You must also include the code that creates the
#   PNG file. Only include the code for a single plot (i.e. plot1.R should only
#   include code for producing plot1.png)
# 
# - Upload the PNG file on the Assignment submission page
#
# - Copy and paste the R code from the corresponding R file into the text box
#   at the appropriate point in the peer assessment.

source('plot_1.R')
source('plot_2.R')
source('plot_3.R')
source('plot_4.R')
source('plot_5.R')
source('plot_6.R')