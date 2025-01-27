##
##    Programme:  Clean_Northern_Marianas_Islands.R
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
   ##    Collect up and process the Northern_Marianas_Islands files
   ##
      Northern_Marianas_Islands <- Country_Data[["Northern_Marianas_Islands"]]
      
      Clean_Northern_Marianas_Islands <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Northern_Marianas_Islands[["Estimates by the Benefish studies of annual fisheries harvestsXXTable24-4"]]
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
                                       id.var = c("Harvest sector", "Year", "Nominal value  ", "Measure", "Table"))   
         Aquaculture$Tonnes <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,1],
                               ifelse(str_detect(Aquaculture$value, "t"), Aquaculture$value, ""))
                               
         Aquaculture$Pieces <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,2],
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
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value  ", "US$", 
                ifelse(X$Harvest_Sector == "Aquaculture",as.character(X$variable), "Tonnes"))
      Clean_Northern_Marianas_Islands[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to Northern_Marianas_Islands GDP in 2021 - Table20-5
   ##
      X <- Northern_Marianas_Islands[["Fishing contribution to CNMI GDP in 2021XXTable24-5"]]
      X_Name <- names(Northern_Marianas_Islands[6])
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
      Clean_Northern_Marianas_Islands[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
   ##
   ##    Price Measures
   ##
      X <- Northern_Marianas_Islands[["Fish prices in WPacFIN\x92s Best Estimated Total Commercial LandingsXXTable24-2"]]
      names(X) <- X[1,]
      names(X)[1] <- "Year"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Landed Price"
      X$Dimension_Value <- str_split_fixed(X$variable, "\\(", 2)[,1]
      X$Unit <- str_replace_all(str_split_fixed(X$variable, "\\(", 2)[,2], "\\)","")
      X$Unit <- ifelse(X$Unit == "", "US$/Pound", X$Unit)
      X$Measure <- "Price Measures"
      Clean_Northern_Marianas_Islands[["Price Measures"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]

   
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Northern_Marianas_Islands, file = 'Data_Intermediate/Clean_Northern_Marianas_Islands.rda')
##
##    And we're done
##
