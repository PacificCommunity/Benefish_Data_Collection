##
##    Programme:  Clean_Palau.R
##
##    Objective:  What is this programme designed to do?
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
   ##    Collect up and process the Palau files
   ##
      Palau <- Country_Data[["Palau"]]
      
      Clean_Palau <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Palau[["Estimates by the Benefish studies of annual fisheries harvestsXXTable13-6"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      X$V1[1] <- "Harvest sector"
      X$V2[1] <- "Year"
      names(X) <- X[1,]
      ##
      ##    Aquaculture - split between tonnes AND pieces. Keep the value the same for each and look at allocating at the end
      ##
         Aquaculture <- X[(X$`Harvest sector` == "Aquaculture"),]
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Year", "Nominal value ", "Measure", "Table"))   
         Aquaculture$Tonnes <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,2],
                               ifelse(str_detect(Aquaculture$value, "t"), Aquaculture$value, ""))
                               
         Aquaculture$Pieces <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,1],
                               ifelse(str_detect(Aquaculture$value, "pcs"), Aquaculture$value, ""))
         Aquaculture$Tonnes <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Tonnes, "t", ""), ",",""))
         Aquaculture$Pieces <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Pieces, "pcs", ""), ",",""))
         
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Year", "Measure", "Table"))   
         Aquaculture <- Aquaculture[!(Aquaculture$variable %in% c("value","variable")),]
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X <- rbind(X[(X$`Harvest sector` != "Aquaculture"),], 
                 Aquaculture)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$variable <- as.character(X$variable)
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value ", "US$", 
                ifelse(X$Harvest_Sector == "Aquaculture",as.character(X$variable), "Tonnes"))
      Clean_Palau[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Palau GDP in 2021 - Table20-5
   ##
      X <- Palau[["Fishing contribution to GDP in 2021 using an alternative approachXXTable13-8"]]
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
      Clean_Palau[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
      
   ##
   ##    Catches of the major fisheries in Palau - Table6-1 & Table6-2
   ##
      ##
      ##    Table 6-2
      ##
         X <- Palau[["Catches by foreign-based longliners in the Palau EEZ (t)XXTable13-3"]]
         X$V1[1] <- "Year"
         names(X) <- X[1,]
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         X$Species <- as.character(X$variable)
         X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!str_detect(X$Species, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Species <- str_trim(X$Species, side = "both")
         X$Method  <- 'Long Liners'
      ##
      ##    Table 6-1
      ##
         Y <- Palau[["Catches by foreign-based purse seiners in the Palau EEZ (t)XXTable13-4"]]
         Y$V1[1] <- "Year"
         names(Y) <- Y[1,]
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         Y$Species <- as.character(Y$variable)
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))
         Y <- Y[!is.na(Y$Value),]
         Y <- Y[!str_detect(Y$Species, regex("total", ignore_case = TRUE)),]
         Y$Measure <- "Catches by Method"
         Y$Unit    <- "Tonnes"
         Y$Species <- str_trim(Y$Species, side = "both")
         Y$Method  <- 'Purse seine'

         Combined_Table <- rbind.fill(X,Y)
                     
         Clean_Palau[["Catches by Method"]] <- Combined_Table[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]


   ##
   ##    Fishing contribution to Palau GDP in 2021 - Table7-7
   ##
      X <- Palau[["The fishing contribution to GDP (US$ millions)XXTable13-7"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[str_detect(X$Harvest_Sector, "Fishing contribution to GDP"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "US$"
      Clean_Palau[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
  
      
   ##
   ##    Fish Employment
   ##
      X <- Palau[["Formal employment in the fisheries sectorXXTable13-12"]]
      names(X) <- X[1,]
      names(X)[1] <- "Number of People Employed"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Number of People Employed"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Employed"
      #X$Dimension_Value <- str_trim(X$`Village `)
      X$Dimension_Value <- "Number of People Employed"
      X <- X[!str_detect(X$`Number of People Employed`, regex("Total", ignore_case = TRUE)),]
      X$Year  <- X$variable
      X$Measure <- "Fishing Employment"
      X$Unit    <- "Headcount"
      Clean_Palau[["Fishing Employment"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]
      
   ##
   ##    Fish Exports
   ##
      ##
      ##    Volume
      ##
         X <- Palau[["Volume of fishery exports of Palau (pounds)XXTable13-9" ]]
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
         X$Unit    <- "Pounds"

      ##
      ##    Value
      ##
         Y <- Palau[["Value of the fishery exports of Palau (thousands of US$)XXTable13-10"]]
         names(Y) <- Y[1,]
         names(Y)[1] <- "Year"
    
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))*1000
         Y <- Y[!is.na(Y$Value),]
         Y$Dimension       <- "Aggregate Commodity"
         Y$Dimension_Value <- str_trim(Y$variable)
         Y$Measure <- "Fish Exports"
         Y$Unit    <- "US$"
         
         X <- rbind.fill(X,Y)
         X <- X[!str_detect(X$Dimension_Value, regex("total", ignore_case = TRUE)),]

      Clean_Palau[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]

   ##
   ##    Fisheries Revenue
   ##
      X <- Palau[["Access fees (US$ millions) compared to total government revenue (%)XXTable13-11"]]
      X$V1[1] <- "Year"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X$Value <- as.numeric(str_replace_all(X$Value, "\\%", ""))
      X <- X[!is.na(X$Value),]
      X$Revenue_Source <- str_trim(X$variable)
      X$Measure <- "Fisheries Revenue"
      X$Unit    <- "US$"      
      Clean_Palau[["Fisheries Revenue"]] <- X[,c("Measure","Table", "Revenue_Source", "Year", "Unit", "Value")]

   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Palau, file = 'Data_Intermediate/Clean_Palau.rda')
##
##    And we're done
##
