##
##    Programme:  Clean_Fiji.R
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
   ##    Collect up and process the Fiji files
   ##
      Fiji <- Country_Data[["Fiji"]]
      
      Clean_Fiji <- list()
   ##
   ##    Volumes and values of the catch of Fiji'92s longline fleet - Table8-2
   ##
      ##
      ##    Table 8-2
      ##
         X <- Fiji[["Volumes and values of the catch of Fiji\x92s longline fleetXXTable8-2"]]
         X$V1[1] <- "Species"
         names(X) <- X[1,]
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Species"),
                             factorsAsStrings = FALSE)
         X$Year <- as.numeric(as.character(X$variable))
         X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!str_detect(X$Species, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Species <- str_trim(X$Species, side = "both")
         X$Method  <- 'Long Liners'
         Clean_Fiji[["Catches by Method"]] <- X[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]

   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests
   ##
      ##
      ##    Table 8-5
      ##
         Y <- Fiji[["Estimates by the Benefish studies of annual fisheries harvestsXXTable8-5"]]
            for(i in 2:nrow(Y))
            {
               Y$V1[i] <- ifelse((Y$V1[i] == "") &(Y$V1[(i-1)] != ""), Y$V1[(i-1)], Y$V1[i])
            }
         Y$V1[1] <- "Harvest_Sector"
         Y$V2[1] <- "Year"
         names(Y) <- c("Harvest_Sector", "Year", "Volume", "Nominal_Value", "Measure","Table")
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Harvest_Sector", "Year"),
                             factorsAsStrings = FALSE)
         ##
         ##    Aquaculture tonnes or pcs... Choose...
         ##       Pieces
         ##
            Y$value[((Y$Harvest_Sector == "Aquaculture") & (Y$Year == 2007) & (Y$variable == "Volume"))] <- 85236
            Y$value[((Y$Harvest_Sector == "Aquaculture") & (Y$Year == 2014) & (Y$variable == "Volume"))] <- 48100
            Y$value[((Y$Harvest_Sector == "Aquaculture") & (Y$Year == 2021) & (Y$variable == "Volume"))] <- 20000
                             
         Y$Year <- as.numeric(as.character(Y$Year))
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))
         Y <- Y[!is.na(Y$Value),]
         
         Y$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
         Y$Unit  = ifelse(Y$variable == "Value (F$)", "F$", 
                     ifelse(Y$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
                     
         Clean_Fiji[["Estimates by the Benefish studies of annual fisheries harvests"]] <- Y[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Fishing contribution to Fiji GDP
   ##
      X <- Fiji[["Official contribution of fishing and aquaculture to GDP (F$ millions)XXTable8-6"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Year    <- as.numeric(str_replace_all(X$variable, " p", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "F$"
      Clean_Fiji[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Fishing contribution to Fiji GDP in 2021 - Table20-5
   ##
      X <- Fiji[["Fishing contribution to GDP in 2021 using an alternative approachXXTable7-8"]]
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "F$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Fiji[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
   ##
   ##    Fish Consumption
   ##
      X <- Fiji[[" Fishery product consumption at PROCFish sitesXXTable8-14"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Village "),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Location"
      X$Dimension_Value <- str_trim(X$`Village `)
      X <- X[!str_detect(X$Dimension_Value, "Average"),]
      X$Metric  <- X$variable
      X$Year    <- 2021
      X$Measure <- "Domestic Fish Consumption"
      X$Unit  = "Kgs per capital per annum"
      Clean_Fiji[["Domestic Fish Consumption"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Metric", "Year", "Unit", "Value")]
      
      
   ##
   ##    Fish Employment
   ##
      X <- Fiji[["Employment in Fiji\x92s tuna industryXXTable8-13"]]
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
      X <- X[!str_detect(X$Dimension_Value, "Average"),]
      X$Year  <- X$variable
      X$Measure <- "Fishing Employment"
      X$Unit    <- "Headcount"
      Clean_Fiji[["Fishing Employment"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]
      
   ##
   ##    Fish Exports
   ##
      X <- Fiji[["Fish ExportsXXTable8-10"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Aggregate Commodity"
      X$Dimension_Value <- str_trim(X$variable)
      X$Measure <- "Fish Exports"
      X$Unit    <- "F$"
      Clean_Fiji[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]


   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Fiji, file = 'Data_Intermediate/Clean_Fiji.rda')
##
##    And we're done
##
