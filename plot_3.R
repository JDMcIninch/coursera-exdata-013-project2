### Coursera / Exploratory Data Analysis / exdata-013 / Project 2
### plot_3.R

# 3. Of the four types of sources indicated by the type (point, nonpoint,
#    onroad, nonroad) variable, which of these four sources have seen decreases
#    in emissions from 1999–2008 for Baltimore City? Which have seen increases
#    in emissions from 1999–2008? Use the ggplot2 plotting system to make a
#    plot answer this question.

library(ggplot2)

nei <- readRDS('summarySCC_PM25.rds')
scc <- readRDS('Source_Classification_Code.rds')

# Calculate the sum of all the emissions for fips == 24510
annual.emissions        <- aggregate(Emissions ~ year + type, subset(nei, fips=='24510'), sum)

# Open a plot file
png('plots/plot_3.png', height=720, width=720)

# Create a plot faceted by emission type
ggplot(annual.emissions, aes(year, Emissions)) +
    facet_grid(type ~ .) +
    geom_line() +
    geom_area(fill='#3399CC80') +
    ggtitle('Emissions in Baltimore City, MD by Source Type') +
    xlab('Year\nNon-road, Non-point, and On-road emissions have decreased 1999-2008.') +
    ylab('Emissions (tons)') +
    theme_grey(base_size=18)

dev.off()