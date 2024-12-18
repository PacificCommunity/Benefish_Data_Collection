##    Programme:  Benefish_Data_Collection.r
##
##    Objective:  Benefish is the report developed by Bob Gillet. However, its a big stonking
##				      pdf report and its data is locked away inside it.
##
##
##    Plan of  :  Aymeric Desurmont has Bob's data in excel spreadsheet form. But how much of the pdf 
##    Attack   :  can we also read with pdftools
##
##                Reading the PDF into R created a massive blob of rubbish. Lets work on the excel 
##                spreadsheets.
##
##
##    Important:  
##    Linkages :  
##
##    Authors  :  James Hogan, FAME - The Pacific Community (SPC), finished 18 December 2024
##
##    Peer     :  <PROGRAMMER>, <TEAM>, <PEER REVIEWED COMPLETED>
##    Reviewer :
##
   ##
   ##    Clear the decks and load up some functionality
   ##
      rm(list=ls(all=TRUE))
      options(scipen = 999)
   ##
   ##    Core libraries
   ##
      library(ggplot2)
      library(plyr)
      library(stringr)
      library(reshape2)
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)
   ##
   ##    Project-specific libraries
   ##
      library(pdftools)
      library(data.table)
   
   ##
   ##    Set working directory
   ##
      setwd("C:/From BigDisk/GIT/Benefish_Data_Collection")
     
   ##
   ##    
   ##
      ##
      ##    Read all of Bob's spreadsheets into R
      ##
        # source("Programmes/Pdftools_Play.r")  # This turns out to be less useful than I thought.
         source("Programmes/Read_Spreadsheets.r")  
         source("Programmes/Clean_Gillet_Data.r")  

      ##
      ##    Clean the manually entered data
      ##
         source("Programmes/Cut_Up_The_Data.r") # This turns the spreadsheets into lists of member countries, and their tables as data frames.
         ##
         ##   Clean the Country Data                
         ##
            unlink("Data_Intermediate/Clean_*.rda")
            
            source("Programmes/Clean_American_Samoa.r")  
            source("Programmes/Clean_Cook_Islands.r")  
            source("Programmes/Clean_Federated_States_of_Micronesia.r")  
            source("Programmes/Clean_Fiji.r")  
            source("Programmes/Clean_French_Polynesia.r")  
            source("Programmes/Clean_Guam.r")  
            source("Programmes/Clean_International_Waters.r")  
            source("Programmes/Clean_Kiribati.r")
            source("Programmes/Clean_Marshall_Islands.r")
            source("Programmes/Clean_Nauru.r")
            source("Programmes/Clean_New_Caledonia.r")
            source("Programmes/Clean_Niue.r") 
            source("Programmes/Clean_Northern_Marianas_Islands.r")
            source("Programmes/Clean_Palau.r")  
            source("Programmes/Clean_Papau_New_Guinea.r")
            source("Programmes/Clean_Pitcairn_Islands.r")
            source("Programmes/Clean_Samoa.r")
            source("Programmes/Clean_Solomon_Islands.r")
            source("Programmes/Clean_Tokelau.r")
            source("Programmes/Clean_Summary_Tables.r")
            source("Programmes/Clean_Tonga.r")
            source("Programmes/Clean_Tuvalu.r")
            source("Programmes/Clean_Vanuatu.r")
            source("Programmes/Clean_Wallis_and_Futuna.r")
            
      ##
      ##    Hoover it all back up again, and make the final tables ready for Giulio
      ##
         source("Programmes/Hoover_Up_Cleaned_Data.r") # This reads back in all of the cleaned data, and identifies excluded tables
         
         
      ##
      ##    Report write ups
      ##
         rmarkdown::render("Programmes/Project_Writeup.rmd", output_file = "C:/From BigDisk/GIT/Benefish_Data_Collection/Product_Output/Project_Writeup.docx")

         
##
##   End of programme
##
