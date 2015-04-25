### Coursera / Exploratory Data Analysis / exdata-013 / Project 2
### plot_1.R

# 1. Have total emissions from PM2.5 decreased in the United States from 1999
#    to 2008? Using the base plotting system, make a plot showing the total
#    PM2.5 emission from all sources for each of the years 1999, 2002, 2005,
#    and 2008.

# Load the data

nei <- readRDS('summarySCC_PM25.rds')
scc <- readRDS('Source_Classification_Code.rds')

# Calculate the sum of all the emissions observations
annual.emissions <- aggregate(Emissions ~ year, nei, sum)

# Create a linear regression for a trend line
annual.emissions.model  <- lm(Emissions ~ year, annual.emissions)

# Open a plot file
png('plots/plot_1.png', height=720, width=720, pointsize=18)

# Start by drawing a basic scatter plot
plot(
    annual.emissions,
    main = 'Total Annual Emissions By Year',
    ylim = c(0, max(annual.emissions$Emissions)*1.1),
    xlab = 'Year',
    ylab = 'Emissions (tons)'
)

# Connect the dots with a line
lines(annual.emissions, col='blue')

# Add a dotted-red trend line
abline(annual.emissions.model, col='red', lty=2)

# Add the year above each point
with(
    annual.emissions,
    text(
        year, Emissions,
        year,
        cex = 0.5,
        pos = 3
    )
)

# Add the emissions level below each point
with(
    annual.emissions,
    text(
        year, Emissions,
        signif(Emissions,3),
        cex = 0.5,
        pos = 1
    )
)

# Add the answer to the question as a legend
legend(
    'bottomleft',
    legend='Total annual emissions in US trend downward from 1999-2008.',
    cex=0.7,
    bty='n'
)

dev.off()