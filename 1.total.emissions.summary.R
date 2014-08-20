###############################################################################
#
#   1.total.emissions.summary.R
#   Author: Eugene Jarder
#
#   Generates a plot that answers the following question:
#
#       Have total emissions from PM2.5 decreased in the United States from
#       1999 to 2008? Using the base plotting system, make a plot showing the
#       total PM2.5 emission from all sources for each of the years 1999,
#       2002, 2005, and 2008.
#
#   Dependencies:
#
#       The following file is expected to be in the working directory:
#           - summarySCC_PM25.rds - contains emission data
#
#       The script is also dependent on the following package:
#           - plyr - to summarize the data
#
###############################################################################

require(plyr)

# plot.total.emissions()
# Summarizes the data from the file and plots the total emissions for each
# year. Adds a model line to reinforce the data.
#
# parameters:
#   - data.file - the file that contains the emission data
#
plot.total.emissions <- function(data.file)
{
    nei <- readRDS(data.file)
    
    nei.summary <- ddply(nei, c('year'), summarize,
                         sum = (sum(Emissions) / 1000000))
    
    with(nei.summary,
        plot(sum ~ year, type = 'l',
             main = 'Total Emissions per Year',
             xlab = 'Year', ylab = 'Total Emissions (in millions)',
             ylim = c(0, max(sum)))
    )
    abline(lm(sum ~ year, nei.summary), col = 'blue', lty = 2)
}

# plot.total.emissions.to.png()
# Plots the total emissions to a png. The plot function is enclosed in a try
# catch loop, so if an error occurs, it will still close the png file.
#
# parameters:
#   - data.file - the file that contains the emission data
#   - out.png - the file name of the output png file
#
plot.total.emissions.to.png <- function(data.file, out.png)
{
    png(filename = out.png, bg = 'transparent')
    tryCatch(
        {
            plot.total.emissions(data.file)
        },
        finally =
        {
            dev.off()
        }
    )
}

# Main Body
plot.total.emissions.to.png('summarySCC_PM25.rds',
                            '1.total.emissions.summary.png')
