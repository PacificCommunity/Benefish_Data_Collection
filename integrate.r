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
##    Authors  :  James Hogan, FAME - The Pacific Community (SPC)
##                Giulio Dalla Riva, SDD - The Pacific Community (SPC)
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
      setwd("C:/Users/jamesh/GIT/Benefish_Data_Collection")
      
   ##
   ##    
   ##
      ##
      ##    Read all of Bob's spreadsheets into R
      ##
         source("Programmes/Pdftools_Play.r")  
         source("Programmes/Pdftools_Play.r")  


         
##
##   End of programme
##
