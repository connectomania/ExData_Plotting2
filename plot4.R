
    #**************************************************************************
    # 4. Across the US, how have emissions from coal combustion-related    ####
    #    sources changed from 1999-2008?
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
    
    # Find scc codes for coal combustion emission sources
    coal.code.index <- with(scc, grepl("coal", ei.sector, ignore.case = TRUE))
    coal.codes <- scc[coal.code.index, ] %>%
        select(scc, ei.sector)
        
    # Merge coal combustion source info with monitor data for the US
    nei.coal <- merge(nei, coal.codes, by="scc")
    
    # Calculate total US coal combustion emissions by year
    coal.emissions <- nei.coal %>%
        group_by(year) %>%
        summarize_at("emissions", sum) %>%
        mutate(emissions = round(emissions/1000000, digits=2))
    
    # Plot total US coal combustion emissions by year with base plotting system
    png("./plot4.png", width=480, height=480)
    
    coal.plot <- with(coal.emissions,
               barplot(emissions, year,
                       names.arg = year,
                       ylim = c(0, .75 * ceiling(max(emissions))),
                       ylab = "Emissions from coal combustion (megatons)",
                       main = "US pm2.5 emissions from coal combustion 
         by year (megatons)")
    )
    text(x = coal.plot, y = coal.emissions$emissions, 
         label = coal.emissions$emissions,
         cex = .75,
         pos = 3)
    
    dev.off()