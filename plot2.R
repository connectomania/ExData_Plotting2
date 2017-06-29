
    #**************************************************************************
    # 2. Use the base plotting system to see if Baltimore, MD              ####
    #    (fips=="24510") emissions decreased from 1999 to 2008.
    #**************************************************************************
    
    # Read files as needed
    if(!exists("nei")) {
        nei <- readRDS("./summarySCC_PM25.rds")
        names(nei) <- tolower(names(nei))
    }
    suppressMessages(library(dplyr))
    
    # Calculate emissions in Baltimore from 1999 to 2008
    baltimore <- nei %>%
        filter(fips=="24510") %>%
        group_by(year) %>%
        summarize_at("emissions", sum) %>%
        mutate(emissions = round(emissions/1000, digits=2))
    
    # Plot Baltimore emissions by year using base plotting system
    png("./plot2.png", width=480, height=480)
    baltplot <- with(baltimore,
                 barplot(emissions, year,
                         names.arg = year,
                         ylim = c(0, ceiling(max(emissions))),
                         ylab = "Emissions (kilotons)",
                         main = "Baltimore total pm2.5 emissions (kilotons)")
    )
    text(x = baltplot, y = baltimore$emissions, 
         label = baltimore$emissions,
         cex = .75,
         pos = 3)
    dev.off()