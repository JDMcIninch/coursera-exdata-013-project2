### Coursera / Exploratory Data Analysis / exdata-013 / Project 2
### plot_4.R

# 4. Across the United States, how have emissions from coal combustion-related
#    sources changed from 1999–2008?

library(ggplot2)

nei <- readRDS('summarySCC_PM25.rds')
scc <- readRDS('Source_Classification_Code.rds')

# The subset of SCC codes that are from coal combustion can be identified
# by the word 'Coal' in the EI.Sector
scc.coal    <- scc[ grep('Coal', scc$EI.Sector), ]

# Convert the nei SCC to a factor - should speed up the merge process
nei$SCC     <- as.factor(nei$SCC)

# this will drop from the nei data anything that isn't coal-combustion
# related
nei.coal <- merge(nei, scc.coal)

# Calculate the sum of all the emissions (Coal combustion)
annual.emissions        <- aggregate(Emissions ~ year + SCC.Level.Two, nei.coal, sum)

# Add a newline in the factor label for 'Total Area Source Fuel Combustion'
# This is perhaps easier to do with plyr, but it's not entirely clear
levels(annual.emissions$SCC.Level.Two)[levels(annual.emissions$SCC.Level.Two)=='Total Area Source Fuel Combustion'] = 'Total Area Source\nFuel Combustion'

# Open a plot file (looks better wide)
png('plots/plot_4.png', height=720, width=1280)

# Create a plot faceted by sector
ggplot(annual.emissions, aes(year, Emissions)) +
    facet_grid(. ~ SCC.Level.Two) +
    geom_line() +
    geom_area(fill='#3399CC80') +
    ggtitle('National Coal Combustion Emissions by Sector') +
    xlab('Year\nOverall coal combustion declined from 1999-2008, except in the industrial sector, which peaked and declined.') +
    ylab('Emissions (tons)') +
    theme(
        axis.text.x = element_text(angle=90, vjust=0.5),
        strip.text.x = element_text(size=8)
    ) + theme_grey(base_size=18)

dev.off()