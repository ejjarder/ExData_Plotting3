###############################################################################
#
#   4.coal.emissions.R
#   Author: Eugene Jarder
#
#   Generates a plot that answers the following question:
#
#       Across the United States, how have emissions from coal
#       combustion-related sources changed from 1999-2008?
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

# get.coal.summary()
# Gets data on emissions from coal combustion.
#
get.coal.summary <- function()
{
    nei <- readRDS('summarySCC_PM25.rds')
    scc <- readRDS('Source_Classification_Code.rds')
    
    scc.coal <- scc[grepl('coal', scc$EI.Sector, ignore.case = TRUE), ]    
    coal.data <- join(nei, scc.coal, by = 'SCC', type = 'inner')
    
    ddply(coal.data, c('year'), summarize, sum = sum(Emissions))
}

# plot.coal.summary()
# Plot the given coal emission data to the given png file.
#
# parameters:
#   coal.summary - the data frame containing the coal combustion emission data
#   out.png - the filename of the output file
#
plot.coal.summary <- function(coal.summary, out.png)
{
    basic.plot <- ggplot(coal.summary, aes(year, sum / 1000))
    
    final.plot <- basic.plot + geom_line() + ylim(0, 750) +
        geom_smooth(method = 'lm', se = FALSE, linetype = 2) +
        labs(title = 'Emissions Per Year Due to Coal Combustion', x = 'Year',
             y = 'Total Emissions (in thousand tons)')
    
    ggsave(filename = out.png, dpi = 60, width = 8, height = 8,
           plot = final.plot)
}

# Main Body
plot.coal.summary(get.coal.summary(), '4.coal.emissions.png')
