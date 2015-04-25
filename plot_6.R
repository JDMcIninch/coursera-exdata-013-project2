### Coursera / Exploratory Data Analysis / exdata-013 / Project 2
### plot_6.R

# 6. Compare emissions from motor vehicle sources in Baltimore City with
#    emissions from motor vehicle sources in Los Angeles County, California
#    (fips == "06037"). Which city has seen greater changes over time in motor
#    vehicle emissions?

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
annual.emissions        <- aggregate(Emissions ~ year + SCC.Level.Two + fips, subset(nei.vehicle, fips %in% c('24510', '06037')), sum)

# Add a newline in the factor label for 'Total Area Source Fuel Combustion'
# This is perhaps easier to do with plyr, but it's not entirely clear
levels(annual.emissions$SCC.Level.Two)[levels(annual.emissions$SCC.Level.Two)=='Highway Vehicles - Diesel'] = 'Highway Vehicles\n(Diesel)'
levels(annual.emissions$SCC.Level.Two)[levels(annual.emissions$SCC.Level.Two)=='Highway Vehicles - Gasoline'] = 'Highway Vehicles\n(Gasoline)'

# Set a human-readable location value for the sites
annual.emissions[ annual.emissions$fips == '06037', 'location' ] = 'Los Angeles County, CA'
annual.emissions[ annual.emissions$fips == '24510', 'location' ] = 'Baltimore City, MD'

# Find the baseline (1999) emissions for each site and type of vehicle
# Scale-wise, the total emissions in LA dwarf those of Baltimore; what we're going to show
# here is each city's relative progress - that is, % change from baseline.
tmp <- aggregate(Emissions ~ fips + SCC.Level.Two, annual.emissions, function(x) { x[1] } )
colnames(tmp)[colnames(tmp)=='Emissions'] = 'Baseline'

# Compute the % change from baseline
annual.emissions <- merge(annual.emissions, tmp)
annual.emissions$Change <- with(annual.emissions, 100 * (Emissions - Baseline) / Baseline)

# Open a plot file (looks better wide)
png('plots/plot_6.png', height=720, width=720)

# Create a plot faceted by sector
ggplot(annual.emissions, aes(year, Change)) +
    facet_grid(SCC.Level.Two ~ location) +
    geom_area(fill='#3399CC80') +
    geom_line(aes(y=0), size=1, colour='black') + 
    ggtitle('Relative Motor Vehicle Emissions Changes') +
    xlab('Year\nWhile emissions have decreased substantially in Baltimore City,\ndiesel emissions in LA have increased between 1999 and 2008.') +
    ylab('Emissions (% Change From 1999 Baseline)') +
    scale_fill_manual(values=c('0' = '#000000', '-1' = '#3399CC80', '1' = '#CC993380')) +
    theme(
        axis.text.x = element_text(angle=90, vjust=0.5),
        strip.text.x = element_text(size=8)
    ) + theme_grey(base_size=18)

dev.off()