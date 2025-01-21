##
##    Programme:  Clean_Cook_Islands.R
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
   ##    Collect up and process the Cook_Islands files
   ##
      Cook_Islands <- Country_Data[["Cook_Islands"]]
      
      Clean_Cook_Islands <- list()
   ##
   ##    Catches of the major fisheries in Cook Islands - Table6-1 & Table6-2
   ##
      ##
      ##    Table 6-2
      ##
         X <- Cook_Islands[[2]]
         X_Name <- names(Cook_Islands[2])
         names(X) <- X[1,]
         X$Year <- 2021
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Gear ", "Year"),
                             factorsAsStrings = FALSE)
         X$variable <- as.character(X$variable)
         X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!str_detect(X$variable, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- ifelse(str_detect(X$value, "hooks"), "Hooks",
                        ifelse(str_detect(X$value, "days"), "Days","Tonnes"))
         X$Species <- ifelse(X$variable == "ALB ", "Albacore",
                        ifelse(X$variable == "BET ", "Bigeye",
                          ifelse(X$variable == "YFT ", "Yellowfin",
                           ifelse(X$variable == "SKJ ", "Skipjack",str_trim(X$variable, side = "both")))))
         X$Method  <- X$Gear
      ##
      ##    Table 6-1
      ##
         Y <- Cook_Islands[[1]]
         Y_Name <- names(Cook_Islands[1])
         Y$V1[1] <- "Year"
         names(Y) <- Y[1,]
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         Y$Value <- as.numeric(str_replace_all(Y$value, "\\D+", ""))
         Y <- Y[!is.na(Y$Value),]
         Y <- Y[!str_detect(Y$variable, regex("total", ignore_case = TRUE)),]
         Y$Measure <- "Catches by Method"
         Y$Unit    <- ifelse(str_detect(Y$variable, "Effort"), "Hours","Tonnes")
         Y$Species <- Y$variable
         Y$Method  <- 'Small Scale Trolling'

         Combined_Table <- rbind.fill(X,Y)
                     
         Clean_Cook_Islands[["Catches by Method"]] <- Combined_Table[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]

   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Cook_Islands[[3]]
      X_Name <- names(Cook_Islands[3])
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      names(X) <- X[1,]
      ##
      ##    Aquaculture - split between tonnes AND pieces. Keep the value the same for each and look at allocating at the end
      ##
         Aquaculture <- X[(X$`Harvest sector` == "Aquaculture"),]
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Estimate year", "Value (NZ$)", "Measure", "Table"))   
         Aquaculture$Tonnes <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,1],
                               ifelse(str_detect(Aquaculture$value, "t"), Aquaculture$value, ""))
                               
         Aquaculture$Pieces <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,2],
                               ifelse(str_detect(Aquaculture$value, "pcs"), Aquaculture$value, ""))
         Aquaculture$Tonnes <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Tonnes, "t", ""), ",",""))
         Aquaculture$Pieces <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Pieces, "pcs", ""), ",",""))
         
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Estimate year", "Measure", "Table"))   
         Aquaculture <- Aquaculture[!(Aquaculture$variable %in% c("value","variable")),]
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Estimate year"),
                          factorsAsStrings = FALSE)
      X <- rbind(X[(X$`Harvest sector` != "Aquaculture"),], 
                 Aquaculture)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Unit  = ifelse(X$variable == "Value (NZ$)", "NZ$", 
                ifelse(X$Harvest_Sector == "Aquaculture",as.character(X$variable), "Tonnes"))
      
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Year    <- X$`Estimate year`
      Clean_Cook_Islands[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to Cook Islands GDP in 2021 - Table6-5
   ##
      X <- Cook_Islands[[4]]
      X_Name <- names(Cook_Islands[4])
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Year    <- X$variable
      X$Measure <- "Fishing contribution to GDP"
      X <- X[X$Harvest_Sector == "Fishing (including pearls)",]
      X$Unit    <- "NZ$"
      Clean_Cook_Islands[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Fishing contribution to  Cook Islands GDP in 2021 - Table20-5
   ##
      X <- Cook_Islands[[6]]
      X_Name <- names(Cook_Islands[6])
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "NZ$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Cook_Islands[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]


   ##
   ##    Fishing Exports - Table6-8
   ##
      X <- Cook_Islands[["Exports of fishery productionXXTable6-8"]]
      X_Name <- names(Cook_Islands["Exports of fishery productionXXTable6-8"])
      X$V1[1] <- "Year"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X <- X[!is.na(X$Value),]
      X$Fish_Commodity <- str_trim(X$variable)
      
      X$Dimension       <- "Aggregate Commodity"
      X$Dimension_Value <- str_trim(X$variable)
      
      X$Unit  = ifelse(X$variable == "Fisheries as a % of total exports", "Proportion", "NZ$")
      X$Value <- ifelse(X$variable == "Fisheries as a % of total exports", X$Value/10000, X$Value)
      X$Measure <- "Fish Exports"
      Clean_Cook_Islands[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]

   ##
   ##    Number of households engaged in fishing - Table6-10
   ##
      X <- Cook_Islands[["Number of households engaged in fishingXXTable6-10"]]
      X$V1[1] <- "Location"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Location"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X <- X[!is.na(X$Value),]
      X$Year  <- 2021
      X$Measure <- "Number of households engaged in fishing"
      X$Fish_Activity <- str_trim(X$variable)
      X$Location <- str_trim(X$Location)
      X$Unit  = ifelse(X$Location == "% participation", "Proportion", "Number")
      X$Value <- ifelse(X$Location == "% participation", X$Value/10000, X$Value)
      Clean_Cook_Islands[["Number of households engaged in fishing"]] <- X[,c("Measure","Table", "Location", "Fish_Activity", "Year", "Unit", "Value")]

   ##
   ##    Fisheries Revenue
   ##
      X <- Cook_Islands[["Fisheries revenue (NZ$ thousands)XXTable6-9" ]]
      X$V1[1] <- "Revenue_Source"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Revenue_Source"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000
      X <- X[!is.na(X$Value),]
      X$Year  <- ifelse(X$variable == "2018/19 Actual", 2019, 
                 ifelse(X$variable == "2019/20 Actual", 2020, 2021))
      X$Measure <- "Fisheries Revenue"
      X$Unit    <- "NZ$"
      Clean_Cook_Islands[["Fisheries Revenue"]] <- X[,c("Measure","Table", "Revenue_Source", "Year", "Unit", "Value")]



   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Cook_Islands, file = 'Data_Intermediate/Clean_Cook_Islands.rda')
##
##    And we're done
##

