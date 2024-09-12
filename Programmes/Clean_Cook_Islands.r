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
      ##    Aquaculture tonnes or pcs... Choose...
      ##       Pieces
      ##
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Estimate year` == 2007))] <- 190000
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Estimate year` == 2014))] <-  52000
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Estimate year` == 2021))] <-  81500
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Estimate year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Year    <- X$`Estimate year`
      X$Unit  = ifelse(X$variable == "Value (NZ$)", "NZ$", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
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
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Year    <- 2021
      X$Measure <- "Fishing contribution to GDP - VAR Method"
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "NZ$")
      Clean_Cook_Islands[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


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
      X$Unit  = ifelse(X$variable == "Fisheries as a % of total exports", "Proportion", "NZ$")
      X$Value <- ifelse(X$variable == "Fisheries as a % of total exports", X$Value/10000, X$Value)
      Clean_Cook_Islands[["Exports of fishery production"]] <- X[,c("Measure","Table", "Fish_Commodity", "Year", "Unit", "Value")]

   ##
   ##    Fishing Revenue - Table6-9
   ##
      X <- Cook_Islands[["Fisheries revenue (NZ$ thousands)XXTable6-9"]]
      X_Name <- names(Cook_Islands["Fisheries revenue (NZ$ thousands)XXTable6-9"])
      X$V1[1] <- "Component"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Component"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))*1000
      X <- X[!is.na(X$Value),]
      X$Year  <- ifelse(X$variable == "2018/19 Actual", 2019, 
                  ifelse(X$variable == "2019/20 Actual", 2020, 2021))
      X$Unit  <- "NZ$"
      X$Measure <- "Fisheries revenue"
      Clean_Cook_Islands[["Fisheries revenue"]] <- X[,c("Measure","Table", "Component", "Year", "Unit", "Value")]

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
   ##    Catches by American Samoa longline vessels - Table20-2
   ##
      X <- Cook_Islands[[2]]
      X_Name <- names(Cook_Islands[2])
      X$V1[2] <- "Species"
      Map <- data.frame(variable = as.character(names(X)),
                        Year = as.character(X[2,]))
      
      
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("V12","V13", "V1"),
                          factorsAsStrings = FALSE)
      X <- merge(X,
                 Map,
                 by = c("variable"))
      X$value <- as.numeric(str_replace_all(X$value, ",",""))
      X <- X[!is.na(X$value),]
      X$V1 <- str_trim(X$V1, side = "both")
      X$V1 <- str_replace_all(X$V1, "\\,  NPO \\(t\\)", "")
      X$V1 <- str_replace_all(X$V1, "\\,  SPO \\(t\\)", "")
      X$V1 <- str_replace_all(X$V1, "  ", " ")
      X$V1 <- str_replace_all(X$V1, " \\(t\\)", "")
      X <- X[!str_detect(X$V1, "total"),]
      X <- X[!str_detect(X$V1, "Total"),]
      X <- X[!str_detect(X$V1, "Species"),]
      
		Y <- with(X[X$V1 != "Number of vessels",],
              aggregate(list(Value = value),
                        list(Year = Year,
                             Species = V1,
                             Measure = V12),
                        sum,
                        na.rm = TRUE))    
      Y$Unit  = "Tonnes"
      Y$Table  = "20-2a"
                        
		Z <- with(X[X$V1 == "Number of vessels",],
              aggregate(list(Value = value),
                        list(Year = Year,
                             Measure = V12,
                             Table = V13),
                        sum,
                        na.rm = TRUE))      
      Z$Unit  = "Number"
      Z$Table  = "20-2b"
      
      Clean_Cook_Islands[["Catch Volume - Longline"]]      <- Y[,c("Measure","Table", "Species", "Year", "Unit", "Value")]
      Clean_Cook_Islands[["Number of vessels - Longline"]] <- Z[,c("Measure","Table",            "Year", "Unit", "Value")]


   ##
   ##    Annual fisheries and aquaculture harvest - Table20-3
   ##
      X <- Cook_Islands[[3]]
      X_Name <- names(Cook_Islands[3])
      names(X) <- X[1,]
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",",""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Measure <- "Annual fisheries and aquaculture harvest"
      X$Year    <- 2021
      X$Unit  = ifelse(X$variable == "Volume (t)", "Tonnes", "US$")
      Clean_Cook_Islands[["Annual fisheries and aquaculture harvest"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Cook_Islands, file = 'Data_Intermediate/Clean_Cook_Islands.rda')
##
##    And we're done
##





 [2,] "Cook_Islands"                  
 [3,] "Federated_States_of_Micronesia"
 [4,] "Fiji"                          
 [5,] "French_Polynesia"              
 [6,] "Guam"                          
 [7,] "International_Waters"          
 [8,] "Kiribati"                      
 [9,] "Marshall_Islands"              
[10,] "Nauru"                         
[11,] "New_Caledonia"                 
[12,] "Niue"                          
[13,] "Northern_Marianas_Islands"     
[14,] "Palau"                         
[15,] "Papau_New_Guinea"              
[16,] "Pitcairn_Islands"              
[17,] "Samoa"                         
[18,] "Solomon_Islands"               
[19,] "Summary_Tables"                
[20,] "Tokelau"                       
[21,] "Tonga"                         
[22,] "Tuvalu"                        
[23,] "Vanuatu"                       
[24,] "Wallis_and_Futuna"  




   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
