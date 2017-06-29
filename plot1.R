
    #**************************************************************************
    # 1. Use the base plotting system to show TOTAL emission from all      ####
    #    sources for each of the years 1999, 2002, 2005, 2008.
    #**************************************************************************

    # Read files as needed
    if(!exists("nei")) {
        nei <- readRDS("./summarySCC_PM25.rds")
        names(nei) <- tolower(names(nei))
    }
    suppressMessages(library(dplyr))
    
    # Calculate total US emissions for available years 1999 to 2008
    total.emissions <- nei %>%
        group_by(year) %>%
        summarize_at("emissions", sum) %>%
        mutate(emissions = round(emissions/1000000, digits=2))
    
    # Plot total US emissions by year using base plotting system
    png("./plot1.png", width=480, height=480)
    us.plot <- with(total.emissions, 
                    barplot(emissions, year,
                            names.arg = year,
                            ylim = c(0, ceiling(max(emissions))),
                            ylab = "Total emissions (megatons)",
                            main = "US total pm2.5 emissions by year (megatons)")
                    )
    text(x = us.plot, y = total.emissions$emissions, 
         label = total.emissions$emissions,
         cex = .75,
         pos = 3)
    dev.off()