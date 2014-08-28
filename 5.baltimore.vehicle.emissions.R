###############################################################################
#
#   5.baltimore.vehicle.emissions.R
#   Author: Eugene Jarder
#
#   Generates a plot that answers the following question:
#
#       How have emissions from motor vehicle sources changed from 1999-2008
#       in Baltimore City?
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

# get.baltimore.vehicle.summary()
# Gets data on emissions from vehicles in Baltimore, Maryland
#
get.baltimore.vehicle.summary <- function()
{
    nei <- readRDS('summarySCC_PM25.rds')
    scc <- readRDS('Source_Classification_Code.rds')
    
    scc.vehicles <- scc[grepl('vehicles', scc$EI.Sector, ignore.case = TRUE), ]    
    baltimore.vehicle.data <- join(nei[nei$fips == '24510', ],
                                   scc.vehicles, by = 'SCC', type = 'inner')
    
    ddply(baltimore.vehicle.data, c('year'), summarize, sum = sum(Emissions))
}

# plot.baltimore.vehicle.summary()
# Plot the given Baltimore, Maryland vehicle emission data to the given png
# file
#
# parameters:
#   baltimore.vehicle.data - the data frame containing the vehicle
#                            emission data
#   out.png - the filename of the output file
#
plot.baltimore.vehicle.summary <- function(baltimore.vehicle.data, out.png)
{
    basic.plot <- ggplot(baltimore.vehicle.data, aes(year, sum))
    
    plot.title <- 'Emissions Per Year Due to Vehicles in Baltimore, Maryland'
    final.plot <- basic.plot + geom_line() +
        geom_smooth(method = 'lm', se = FALSE, linetype = 2) +
        labs(title = plot.title, x = 'Year', y = 'Total Emissions (in tons)') +
        ylim(0, max(baltimore.vehicle.data$sum))
    
    ggsave(filename = out.png, dpi = 60, width = 8, height = 8,
           plot = final.plot)
}

# Main Body
plot.baltimore.vehicle.summary(get.baltimore.vehicle.summary(),
                               '5.baltimore.vehicle.emissions.png')
