###############################################################################
#
#   3.baltimore.emissions.R
#   Author: Eugene Jarder
#
#   Generates a plot that answers the following question:
#
#       Of the four types of sources indicated by the type (point, nonpoint,
#       onroad, nonroad) variable, which of these four sources have seen
#       decreases in emissions from 1999-2008 for Baltimore City? Which have
#       seen increases in emissions from 1999-2008? Use the ggplot2 plotting
#       system to make a plot answer this question.
#
#   Dependencies:
#
#       The following file is expected to be in the working directory:
#           - summarySCC_PM25.rds - contains emission data
#
#       The script is also dependent on the following package:
#           - plyr - to summarize the data
#           - ggplot2 - to plot the data
#
###############################################################################

require(plyr)
require(ggplot2)

# get.baltimore.emissions.plot()
# Get the ggplot object of the Emissions in Baltimore, Maryland from the
# given data.file
#
# parameters:
#   - data.file - the file that contains the emission data
#
get.baltimore.emissions.plot <- function(data.file)
{
    nei <- readRDS(data.file)
    baltimore.summary <- ddply(nei[nei$fips == '24510', ], c('year'),
                               summarize, sum = sum(Emissions))
    
    basic.plot <- ggplot(baltimore.summary, aes(year, sum))
    ggplot + geom_line(aes(color = type)) +
        geom_smooth(method = 'lm', se = FALSE,
                    aes(color = type), linetype = 2) +
        labs(title = 'Emissions Per Type Per Year in Baltimore, Maryland',
             x = 'Year', y = 'Total Emissions (in tons)',
             colour = 'Emission Types')
}

# plot.baltimore.emissions.to.png()
# Plots the total emissions to a png.
#
# parameters:
#   - baltimore.emissions - the plot of the baltimore emissions data
#   - out.png - the file name of the output png file
#
plot.baltimore.emissions.to.png <- function(baltimore.emissions, out.png)
{
    ggsave(filename = out.png, plot = baltimore.emissions, dpi = 60,
           width = 8, height = 8)
}

# Main Body
baltimore.emissions <- get.baltimore.emissions.plot('summarySCC_PM25.rds')

plot.baltimore.emissions.to.png(baltimore.emissions,
                                '3.baltimore.emissions.png')
