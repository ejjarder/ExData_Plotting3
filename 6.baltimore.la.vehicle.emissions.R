###############################################################################
#
#   6.baltimore.la.vehicle.emissions.R
#   Author: Eugene Jarder
#
#   Generates a plot that answers the following question:
#
#       Compare emissions from motor vehicle sources in Baltimore City with
#       emissions from motor vehicle sources in Los Angeles County, California
#       (fips == 06037). Which city has seen greater changes over time in motor
#       vehicle emissions?
#
#   Dependencies:
#
#       The following files are expected to be in the working directory:
#           - summarySCC_PM25.rds - contains emission data
#           - Source_Classification_Code.rds - contians SCC definitions
#
#       The script is also dependent on the following package:
#           - plyr - to summarize the data
#           - ggplot2 - to plot the data
#
###############################################################################

require(plyr)
require(ggplot2)

# get.fips.to.location()
# Data frame that matches fips with the name of the location
#
get.fips.to.location <- function()
{
    data.frame(fips = c('24510', '06037'),
               location = c('Baltimore', 'Los Angeles'))
}

# get.vehicle.summary()
# Get the data of the vehicle emission data of LA and Baltimore
#
get.vehicle.summary <- function()
{
    nei <- readRDS('summarySCC_PM25.rds')
    scc <- readRDS('Source_Classification_Code.rds')
    
    nei.baltimore.la <- join(nei, get.fips.to.location(), by = 'fips',
                             type = 'inner')
    
    scc.vehicles <- scc[grepl('vehicles', scc$EI.Sector, ignore.case = TRUE), ]    
    vehicle.data <- join(nei.baltimore.la, scc.vehicles, by = 'SCC',
                         type = 'inner')
    
    ddply(vehicle.data, c('year', 'location'), summarize, sum = sum(Emissions))
}

# get.plot.vehicle.summary()
# Get the plot of the vehicle emission data of LA and Baltimore given the
# data frame.
#
# parameters:
#   vehicle.data - the data frame containing the vehicle emission data
#
get.plot.vehicle.summary <- function(vehicle.data)
{
    basic.plot <- ggplot(vehicle.data, aes(year, sum))
    
    plot.title <-
        'Comparison of vehicle emissions between Baltimore and Los Angeles'
    location.aes <- aes(color = location)
    
    final.plot <- basic.plot + geom_line(location.aes) +
        geom_smooth(method = 'lm', se = FALSE, linetype = 2, location.aes) +
        labs(title = plot.title, x = 'Year', y = 'Total Emissions (in tons)',
             colour = 'Location') +
        ylim(0, max(vehicle.data$sum))
}

# plot.vehicle.summary.to.png()
# Plot the vehicle emission data to the given png.
#
# parameters:
#   vehicle.plot - the plot of the vehicle emission data
#   out.png - the filename of the output file
#
plot.vehicle.summary.to.png <- function(vehicle.plot, out.png)
{
    ggsave(filename = out.png, dpi = 60, width = 8, height = 8,
           plot = vehicle.plot)
}

# Main Body
vehicle.data <- get.vehicle.summary()
vehicle.plot <- get.plot.vehicle.summary(vehicle.data)
plot.vehicle.summary.to.png(vehicle.plot,
                            '6.baltimore.la.vehicle.emissions.png')
