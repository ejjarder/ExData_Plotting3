###############################################################################
#
#   2.baltimore.emissions.summary.R
#   Author: Eugene Jarder
#
#   Generates a plot that answers the following question:
#
#       Have total emissions from PM2.5 decreased in the Baltimore City,
#       Maryland (fips == "24510") from 1999 to 2008? Use the base plotting
#       system to make a plot answering this question.
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

# plot.baltimore.emissions.summary()
# Summarizes the data from the file and plots the total emissions in Baltimore, 
# Maryland for each year. Adds a model line to reinforce the data.
#
# parameters:
#   - data.file - the file that contains the emission data
#
plot.baltimore.emissions.summary <- function(data.file)
{
    nei <- readRDS(data.file)
    baltimore.summary <- ddply(nei[nei$fips == '24510', ], c('year'),
                               summarize, sum = sum(Emissions))
    with(baltimore.summary,
         plot(sum ~ year, type = 'l',
              main = 'Total Emissions in Baltimore, Maryland per Year',
              xlab = 'Year', ylab = 'Total Emissions (in tons)',
              ylim = c(0, max(sum)))
    )
    abline(lm(sum ~ year, baltimore.summary), col = 'blue', lty = 2)
}

# plot.baltimore.emissions.summary.to.png()
# Plots the total emissions to a png. The plot function is enclosed in a try
# catch loop, so if an error occurs, it will still close the png file.
#
# parameters:
#   - data.file - the file that contains the emission data
#   - out.png - the file name of the output png file
#
plot.baltimore.emissions.summary.to.png <- function(data.file, out.png)
{
    png(filename = out.png, bg = 'transparent')
    tryCatch(
        {
            plot.baltimore.emissions.summary(data.file)
        },
        finally =
        {
            dev.off()
        }
    )
}

# Main Body
plot.baltimore.emissions.summary.to.png('summarySCC_PM25.rds',
                                        '2.baltimore.emissions.summary.png')
