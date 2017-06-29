
    #**************************************************************************
    # 6. Compare emissions from motor vehicle sources in Baltimore City    ####
    #    with emissions from motor vehicle sources in Los Angeles County, CA 
    #    (fips=="06037"). Which has seen greater changes over time?
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
    
    # Merge vehicle source info with monitor data for Baltimore and LA County
    nei.vehicle <- merge(nei, vehicle.codes, by="scc") %>%
        filter(fips=="24510" | fips=="06037") %>%
        filter(year==1999 | year==2008) %>%
        rename(area=fips) %>%
        mutate(area = replace(area, area=="06037", "LA County")) %>%
        mutate(area = replace(area, area=="24510", "Baltimore"))
    
    # Calculate total emissions by area for 1999 and 2008
    vehicle.emissions <- nei.vehicle %>%
        group_by(area, year) %>%
        summarize_at("emissions", sum) %>%
        mutate(delta = emissions - lag(emissions, default = 0)) %>%
        mutate(delta = round(delta), digits=0)

    # Plot emissions levels and annotate with magnitude of change using ggplot2
    suppressMessages(library(ggplot2))
    png("./plot6.png", width=480, height=480)
    
    ggplot(data=vehicle.emissions, aes(x=year, y=emissions, fill=area)) +
        geom_line(aes(color=area)) + 
        geom_point(aes(color=area)) +
        labs(y = "Motor vehicle emissions (tons)", x = "Year") +
        labs(title = "Baltimore vs. LA vehicle emissions 1999 & 2008",
             subtitle = "(tons)") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              legend.title = element_blank()) +
        geom_text(data = subset(vehicle.emissions, year=="2008"), 
              aes(x = 2008, 
                  y = emissions *.9, 
                  label = paste("change = ",delta),
                  hjust = .95, vjust = -1.5
                  )
              )

        dev.off()