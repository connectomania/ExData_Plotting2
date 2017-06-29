
    #**************************************************************************
    # 3. Use ggplot2 to see which source types (point, nonpoint, onroad,   ####
    #    nonroad) have increased vs. decreased in Baltimore from 1999-2008
    #**************************************************************************
    
    # Read files as needed
    if(!exists("nei")) {
        nei <- readRDS("./summarySCC_PM25.rds")
        names(nei) <- tolower(names(nei))
    }
    suppressMessages(library(dplyr))
    
    # Calculate emissions by source type for 1999 and 2008
    baltimore <- nei %>%
        filter(fips=="24510") %>%
        filter(year==1999 | year==2008) %>%
        mutate(year = as.factor(year)) %>%
        group_by(type, year) %>%
        summarize_at("emissions", sum) %>%
        mutate(emissions = round(emissions/1000, digits=2))
    
    # Plot emissions changes from 1999-2008 by source type using ggplot2 system
    suppressMessages(library(ggplot2))
    png("./plot3.png", width=480, height=480)
    
    baltgg <- ggplot(baltimore, aes(x=year, y=emissions, group=type))
    baltgg + 
        geom_line(aes(color=type)) + 
        geom_point(aes(color=type)) +
        labs(y = "Emissions (kilotons)", x = "Year") +
        labs(title = "Baltimore emissions by source type 1999 & 2008",
             subtitle = "(kilotons)") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    
    dev.off()