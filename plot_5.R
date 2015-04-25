### Coursera / Exploratory Data Analysis / exdata-013 / Project 2
### plot_5.R

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
#    Baltimore City?

library(ggplot2)

nei <- readRDS('summarySCC_PM25.rds')
scc <- readRDS('Source_Classification_Code.rds')

# The subset of SCC codes that are from vehicles can be identified by the
# data category "Onroad"
scc.vehicle    <- scc[ scc$Data.Category == 'Onroad', ]

# Convert the nei SCC to a factor - should speed up the merge process
nei$SCC     <- as.factor(nei$SCC)

# this will drop from the nei data anything that isn't vehicle
# related
nei.vehicle <- merge(nei, scc.vehicle)

# Calculate the on-road emissions by year and type for Baltimore City, MD
annual.emissions        <- aggregate(Emissions ~ year + SCC.Level.Two, subset(nei.vehicle, fips=='24510'), sum)

# Add a newline in the factor label for 'Total Area Source Fuel Combustion'
# This is perhaps easier to do with plyr, but it's not entirely clear
levels(annual.emissions$SCC.Level.Two)[levels(annual.emissions$SCC.Level.Two)=='Highway Vehicles - Diesel'] = 'Highway Vehicles\n(Diesel)'
levels(annual.emissions$SCC.Level.Two)[levels(annual.emissions$SCC.Level.Two)=='Highway Vehicles - Gasoline'] = 'Highway Vehicles\n(Gasoline)'

# Open a plot file (looks better wide)
png('plots/plot_5.png', height=720, width=720)

# Create a plot faceted by sector
ggplot(annual.emissions, aes(year, Emissions)) +
    facet_grid(. ~ SCC.Level.Two) +
    geom_line() +
    geom_area(fill='#3399CC80') +
    ggtitle('Motor Vehicle Emissions for Baltimore City, MD') +
    xlab('Year\nMotor vehicle emissions have declined substantially between 1999 and 2008.') +
    ylab('Emissions (tons)') +
    theme(
        axis.text.x = element_text(angle=90, vjust=0.5),
        strip.text.x = element_text(size=8)
    ) + theme_grey(base_size=18)

dev.off()