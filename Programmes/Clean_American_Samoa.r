##
##    Programme:  Clean_American_Samoa.R
##
##    Objective:  Cleans up the Benefish 4 American Samoa data
##
##    Author:     James Hogan, FAME - SPC, 10 September 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Collect up the downloaded files
   ##
      load('Data_Intermediate/Country_Data.rda')
      
   ##
   ##    Collect up and process the American_Samoa files
   ##
      American_Samoa <- Country_Data[["American_Samoa"]]
      
      Clean_American_Samoa <- list()

   ##
   ##    Catches of the major fisheries in Papau_New_Guinea - Table6-1 & Table6-2
   ##
      ##
      ##    Table 6-2
      ##
         X <- American_Samoa[["Catches by American Samoa longline vesselsXXTable20-2"]]
         names(X) <- X[2,]
         names(X)[c(1,12,13)] <- c("Species", "Measure", "Table")
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Species"),
                             factorsAsStrings = FALSE)
         X <- with(X,
                 aggregate(list(Value = as.numeric(value)),
                           list(Year = variable,
                                Species = Species,
                                Table = Table,
                                Measure = Measure),
                           sum,
                           na.rm = TRUE))    
         X <- X[!is.na(X$Value),]
         X <- X[X$Species != "",]
         X <- X[!str_detect(X$Species, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Species <- str_trim(X$Species, side = "both")
         X$Method  <- 'Long Liners'
                     
         Clean_American_Samoa[["Catches by Method"]] <- X[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]

   ##
   ##    Annual fisheries and aquaculture harvest - Table20-3
   ##
      X <- American_Samoa[["Annual fisheries and aquaculture harvestXXTable20-3"]]
      names(X) <- X[1,]
      
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Measure <- "Annual fisheries and aquaculture harvest"
      X$Year    <- 2021
      X$Unit  = ifelse(X$variable == "Volume (t)", "Tonnes", "US$")
      #Clean_American_Samoa[["Annual fisheries and aquaculture harvest"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table20-4
   ##
      X <- American_Samoa[["Estimates by the Benefish studies of annual fisheries harvestsXXTable20-4"]]
      
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Volume (t)", "Tonnes", "US$")
      Clean_American_Samoa[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to American Samoa GDP in 2021 - Table20-5
   ##
      X <- American_Samoa[["Fishing contribution to American Samoa GDP in 2021XXTable20-5"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Year    <- 2021
      X$Measure <- "Fishing contribution to GDP - VAR Method"
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "US$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_American_Samoa[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]

      
   ##
   ##    Fish Exports
   ##
      X <- American_Samoa[["Value of fishery product exportsXXTable20-6"]]
      names(X) <- X[1,]
      names(X)[1] <- "Aggregate Commodity"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Aggregate Commodity"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Aggregate Commodity"
      X$Dimension_Value <- str_trim(X$`Aggregate Commodity`)
      #X <- X[!str_detect(X$`Number of People Employed`, regex("Total", ignore_case = TRUE)),]
      X$Year  <- X$variable
      X$Measure <- "Fish Exports"
      X$Unit    <- "US$"
      Clean_American_Samoa[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]



   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_American_Samoa, file = 'Data_Intermediate/Clean_American_Samoa.rda')
##
##    And we're done
##
