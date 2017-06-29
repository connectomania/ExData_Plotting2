
    #**************************************************************************
    # 5. How have emissions from motor vehicle sources changed from        ####
    #    1999-2008 in Baltimore City?
    #**************************************************************************
    
    # Read files as needed
    if(!exists("nei")) {
        nei <- readRDS("./summarySCC_PM25.rds")
        names(nei) <- tolower(names(nei))
    }
    if(!exists("scc")) {
        scc <- readRDS("./Source_Classification_Code.rds")        
        names(scc) <- tolower(names(scc))
    }
    suppressMessages(library(dplyr))
    
    # Find scc codes for motor vehicle emission sources
    vehicle.index <- with(scc, grepl("highway", scc.level.two, ignore.case =T))
    vehicle.codes <- scc[vehicle.index, ] %>%
        select(scc, scc.level.two)
    
    # Merge vehicle source info with monitor data for Baltimore City
    nei.vehicle <- merge(nei, vehicle.codes, by="scc") %>%
        filter(fips=="24510")
    
    # Calculate total emissions by year for Baltimore City
    vehicle.emissions <- nei.vehicle %>%
        group_by(year) %>%
        summarize_at("emissions", sum) %>%
        mutate(emissions = round(emissions, digits=0))
    
    # Plot emissions using base plotting system
    png("./plot5.png", width=480, height=480)
    
    vplot <- with(vehicle.emissions,
                  barplot(emissions, year,
                          names.arg = year,
                          ylim = c(0, 1.1 * ceiling(max(emissions))),
                          ylab = "Emissions from motor vehicle sources (tons)",
                          main = "Baltimore pm2.5 emissions from  
    motor vehicle sources, by year (tons)")
                  )
    text(x = vplot, y = vehicle.emissions$emissions, 
         label = vehicle.emissions$emissions,
         cex = .75,
         pos = 3)
    
    dev.off()