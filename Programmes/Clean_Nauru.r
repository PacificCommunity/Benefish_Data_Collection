##
##    Programme:  Clean_Nauru.R
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
   ##    Collect up and process the Nauru files
   ##
      Nauru <- Country_Data[["Nauru"]]
      
      Clean_Nauru <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table9-4
   ##
      X <- Nauru[["Estimates by the Benefish studies of annual fisheries harvestsXXTable11-6"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      X$V1[1] <- "Harvest sector"
      X$V2[1] <- "Year"
      names(X) <- X[1,]
      ##
      ##    Aquaculture tonnes or pcs... Choose...
      ##       Pieces
      ##
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 8
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- .1
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 0
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (A$)", "A$", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
      Clean_Nauru[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to Nauru GDP in 2021 - Table20-5
   ##
      X <- Nauru[["Fishing contribution to GDP in 2021 using an alternative approachXXTable11-8"]]
      X_Name <- names(Nauru[6])
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "A$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Nauru[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
      
   ##
   ##    Fishing contribution to Nauru GDP in 2021 - Table7-7
   ##
      X <- Nauru[["Fishing contribution to GDP XXTable11-7"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[str_detect(X$Harvest_Sector, "Fishing contribution"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "A$"
      Clean_Nauru[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      
   ##
   ##    Fish Consumption
   ##
      X <- Nauru[["Consumption of fishery products on Nauru in 2019 according to the Nauru Bureau of StatisticsXXTable11-11"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Type of fish consumed"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Type of fish consumed"
      X$Dimension_Value <- str_trim(X$`Type of fish consumed`)
      #X <- X[!str_detect(X$Location, "Average"),]
      X$Metric  <- X$variable
      X$Year    <- 2021
      X$Measure <- "Domestic Fish Consumption"
      X$Unit  = "Number of Households"
      Clean_Nauru[["Domestic Fish Consumption"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Metric", "Year", "Unit", "Value")]
      
   ##
   ##    Fish Employment
   ##
      X <- Nauru[["Employment in the formal marine fishing sector in NauruXXTable11-10"]]
      names(X) <- X[1,]
      names(X)[1] <- "Sex"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Sex"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Employed"
      X$Dimension_Value <- str_trim(X$Sex)
      X <- X[!str_detect(X$Dimension_Value, "Total"),]
      X$Year  <- 2021
      X$Measure <- "Fishing Employment"
      X$Unit  = "Headcount"
      Clean_Nauru[["Fishing Employment"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]
   ##
   ##    Price Measures
   ##
      X <- Nauru[["2021 fish prices in NauruXXTable11-1"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      names(X) <- X[1,]
      names(X)[1] <- "Fish Type"
 
      X$Value <- as.numeric(str_replace_all(X$`Estimated price  per kg (A$)`, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Measure <- "Price Measures"
      X$Dimension       <- "Landed Price"
      X$Unit    <- str_trim(X$`Selling unit`)
      X$Dimension_Value <- str_trim(X$Commodity)
      X$Year  <- 2021
      Clean_Nauru[["Price Measures"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]

   ##
   ##    Fisheries Revenue
   ##
      X <- Nauru[["Nauru\x92s access fees (A$)XXTable11-9"]]
      X$V1[1] <- "Revenue_Source"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Revenue_Source"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X <- X[!is.na(X$Value),]
      X$Year  <- ifelse(X$variable == "2017/18", 2018, 
                 ifelse(X$variable == "2018/19", 2019, 
                 ifelse(X$variable == "2019/20", 2020, 
                 ifelse(X$variable == "2020/21", 2021,2022))))
      X$Measure <- "Fisheries Revenue"
      X$Unit    <- "A$"
      X <- X[!str_detect(X$Revenue_Source, "Total"),]
      
      Clean_Nauru[["Fisheries Revenue"]] <- X[,c("Measure","Table", "Revenue_Source", "Year", "Unit", "Value")]


      
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Nauru, file = 'Data_Intermediate/Clean_Nauru.rda')
##
##    And we're done
##
