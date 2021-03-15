This readme.txt file was generated on 2021-03-11 by Althea A. Archer.


-------------------
GENERAL INFORMATION
-------------------


1. Data and R Code to Support: Owl Occupancy in Three of El Salvador's Protected Areas from 2003 through 2013


2. Author Information


  Principal Investigator Contact Information
        Name: Althea Archer
           Institution: St. Cloud State University & University of Minnesota
           Address: Department of Biological Sciences, St. Cloud State University, St. Cloud, MN 56301 USA
           Email: althea.archer@gmail.com
	   ORCID: 0000-0003-1927-0783

  Co-investigator Contact Information
        Name: Jane Noll West
           Institution: None (retired)
           Address: 1120 Sidney Road, Washburn, ND 58577 USA
           Email: buhobay@protonmail.com



3. Date of data collection (single date, range, approximate date) 2003-04-27 through 2013-04-23


4. Geographic location of data collection (where was data collected?): El Salvador


5. Information about funding sources that supported the collection of the data: No direct funding sources, but authors would like to acknowledge: The Ministerio de Medio Ambiente y Recursos Naturales (MARN) for their authorization to conduct scientific research in El Salvador’s protected natural areas (permits were obtained annually starting in 2002: AIMA MARN-DGPN 006-2002 and ending in 2013: MARN AIMA-005-2013). All research reported here was carried out in compliance with the relevant laws, guidelines, and permits of the country in which it was conducted.  Thank you to: MARN and SalvaNatura for their employee’s assistance in San Salvador and in the field; and to the MARN for logistics support and the GIS layers.  Especially thank you to Alfonso Sermeño Martínez, Vidal Campos Aguirre, Charles Starner and our families. Thank you also to Dr. David Andersen for his technical review and the US Fish and Wildlife Service. 




--------------------------
SHARING/ACCESS INFORMATION
-------------------------- 


1. Licenses/restrictions placed on the data: 


2. Links to publications that cite or use the data: West and Archer. Owl Occupancy in Three of El Salvador's Protected Areas from 2003 through 2013 (in review for The Journal of Raptor Research). 


3. Links to other publicly accessible locations of the data: 


4. Links/relationships to ancillary data sets: 


5. Was data derived from another source?
           If yes, list source(s):


6. Recommended citation for the data:

Archer and West. 2021. Data and R Code to Support: Owl Occupancy in Three of El Salvador's Protected Areas from 2003 through 2013




---------------------
DATA & FILE OVERVIEW
---------------------


1. File List
   A. Filename:       Route_Table.csv 
      Short description:        Information about the six survey routes used in this study.
        
   B. Filename:        Stations_Table.csv
      Short description:        Information about the climate conditions and species broadcast for each survey station during each survey conducted during this study. 
        
   C. Filename:        Survey_Table.csv
      Short description:		Information about the surveyors, survey start and end date, and environmental conditions for each survey conducted during this study.

   D. Filename:        Owls_Table.csv
	Short description: 	Information about each owl observed at each station during each survey conducted during this study.

   E. Filename:        a_process_data_global.html
	Short description: 	HTML showing R Program that imports raw data and converts it into processed data ready for analysis in subsequent programs. 

   F. Filename:        b_occupancy_model_global.html
	Short description: 	HTML showing R Program that defines the single-species occupancy models in JAGS language. 

   G. Filename:        b_richness_model_global.html
	Short description: 	HTML showing R Program that defines the multiple-species richness model in JAGS language. 

   H. Filename:        c01_process_data_ferpy.html
	Short description: 	HTML showing R Program that processes all detections of Ferruginous Pygmy-Owls (FerPy) during the surveys conducted in this study. Output includes the detection history in a format ready for the single-species JAGS model.

   I. Filename:        c02_process_data_Mottd.html
	Short description: 	HTML showing R Program that processes all detections of Mottled Owls (Mottd) during the surveys conducted in this study. Output includes the detection history in a format ready for the single-species JAGS model.

   J. Filename:        c03_process_data_specd.html
	Short description: 	HTML showing R Program that processes all detections of Spectacled Owls (Specd) during the surveys conducted in this study. Output includes the detection history in a format ready for the single-species JAGS model.

   K. Filename:        c04_process_data_richness.html
	Short description: 	HTML showing R Program that processes all detections of all owls during the surveys conducted in this study. Output includes the detection history in a format ready for the multiple-species JAGS model.

   L. Filename:        d01_occupany_analysis_ferpy.html
	Short description: 	HTML showing R Program that conducts the single-species occupancy model for FerPy. 

   M. Filename:        d02_occupany_analysis_mottd.html
	Short description: 	HTML showing R Program that conducts the single-species occupancy model for Mottd. 

   N. Filename:        d03_occupany_analysis_specd.html
	Short description: 	HTML showing R Program that conducts the single-species occupancy model for Specd. 

   O. Filename:        d04_richness_analysis.html
	Short description: 	HTML showing R Program that conducts the multiple-species richness model for all owl species. 

   P. Filename:        e01_process_results_ferpy.html
	Short description: 	HTML showing R Program that processes output from the single-species occupancy JAGS model for FerPy.

   Q. Filename:        e02_process_results_mottd.html
	Short description: 	HTML showing R Program that processes output from the single-species occupancy JAGS model for Mottd.

   R. Filename:        e03_process_results_ferpy.html
	Short description: 	HTML showing R Program that processes output from the single-species occupancy JAGS model for Specd.

   S. Filename:        e04_process_results_richness.html
	Short description: 	HTML showing R Program that processes output from the multiple-species richness JAGS model for all owl species.

   T. Filename:        f01_graphing_results_ferpy.html
	Short description: 	HTML showing R Program that creates graphs showing results for the FerPy single-species occupancy model.

   U. Filename:        f02_graphing_results_mottd.html
	Short description: 	HTML showing R Program that creates graphs showing results for the Mottd single-species occupancy model.

   V. Filename:        f03_graphing_results_specd.html
	Short description: 	HTML showing R Program that creates graphs showing results for the Specd single-species occupancy model.

   W. Filename:        g_graphing_results_global.html
	Short description: 	HTML showing R Program that creates graphs showing results for thesingle-species occupancy models, the multiple-species richness model, and the site locations. This program creates the figures shown in the manuscript.

   X. Filename:       R_programs.zip
	Short description: 	R programs that can be used to run all of the above analyses. 


2. Relationship between files:        

Raw data (Files A through D) are relationally connected in a nested way: Each owl detection record (rows in Owls_Table.csv) was found in one specific station during each survey (rows in Stations_Table.csv), and each station (rows in Stations_Table.csv) correspond to specific surveys (rows in Survey_Table.csv). Each survey (rows in Survey_Table.csv) was located in one of the six specific routes (rows in the Routes_Table.csv). Unique identifiers were used to relate tables, as described below in the file-specific details.


Raw data (Files A through D) are imported into the first program, File E, and processed for use in all subsequent programs (Files F through W). The processed global data that is created by File E is used to create species-specific processed data in Files H through J and richness data in File K. Files F and G define the JAGS occupancy and richness models, respectively, that are read into Files L through O and used to analyze data created by Files H through K. Files P through S process the results of the JAGS models, and get them ready to cite in the results section of the paper and to create graphs, which are created with Files T through W. The raw R programs are provided in File X.



3. Additional related data collected that was not included in the current data package:




4. Are there multiple versions of the dataset? No







--------------------------
METHODOLOGICAL INFORMATION
--------------------------


1. Description of methods used for collection/generation of data: 

Nighttime foot surveys were conducted along 6 routes with 10 survey stations each in 3 protected areas of El Salvador up to twice a year from 2003 through 2013. At each station, surveyors conducted silent listening for owl calls for two minutes, followed by 3 minutes of broadcast calling, and another 7 minutes of silent listening. Full methodology can be found in West and Archer (in review).


2. Methods for processing the data: 

Processing the data included using the owl detections given in Owls_Table.csv and corresponding them with each survey and survey station in order to convert those detections by species to binary "presence/absence" data. 


3. Instrument- or software-specific information needed to interpret the data: Program R (https://www.r-project.org) and JAGS program (https://mcmc-jags.sourceforge.io)  


4. Standards and calibration information, if appropriate:


5. Environmental/experimental conditions: Surveys were conducted during various environmental conditions over the course of the 10 year study, which are described in the Survey_Table.csv and Stations_Table.csv.


6. Describe any quality-assurance procedures performed on the data:


7. People involved with sample collection, processing, analysis and/or submission: 

Jane Doll West conducted the surveys and entered the data. Althea Archer processed the data and conducted the analyses.






-----------------------------------------
DATA-SPECIFIC INFORMATION FOR: Route_Table.csv
-----------------------------------------

1. Number of variables: 3

2. Number of cases/rows: 6

3. Missing data codes:
        None applicable

4. Variable List
         
    A. Route_ID
       Description: Unique route identifier.

    B. Route_Name
       Description: Unique route name used by survey crews. 
                    
    C. Forest
	Description: The protected area in which each route was located.
		El Imposible = El Imposible National Park
		Montecristo = Montecristo National Park
		Nancuchiname = Nancuchiname Forest


-----------------------------------------
DATA-SPECIFIC INFORMATION FOR: Stations_Table.csv
-----------------------------------------

1. Number of variables: 11

2. Number of cases/rows: 859

3. Missing data codes:
        None applicable

4. Variable List
         
    	A. Stations_ID
       		Description: Unique station identifier.

    	B. Survey_ID
       		Description: Unique survey identifier.
                    
    	C. Station
		Description: Station identifier which was concatenated Route ID plus Station number. Not unique. 
		
	D. Station_Start_Time
		Description: The time of day (24-hour time) at which each station's acoustic survey began. 

	E. Temperature
		Description: The temperature (Celsius) at the time of each station's acoustic survey, as measured with a portable temperature monitor (Brunton Sherpa). 

	F. Barometer
		Description: The atmospheric pressure (ATM) at the time of each station's acoustic survey, as measured with a portable pressure monitor (Brunton Sherpa).

	G. Fog
		Description: The percentage of fog present at the time of each station's acoustic survey, measured on a scale from 0 to 100 by 5s. 

	H. Wind_Speed
		Description: The wind speed (kph) at the time of each station's acoustic survey, measured with a portable windspeed monitor (Brunton Sherpa)

	I. Broadcast_Species
		Description: The owl species that was used as a broadcast species at each station's acoustic survey. Owl species' broadcast calls were specific to the station and consistent across years. Some broadcast species names are missing from this dataset, but a_process_data_global.html shows how those missing values were completed, based on the known species used at each station.

	Black and White Owl = Ciccaba nigrolineata
	Crested Owl = Lophostrix cristata
	Great Horned Owl = Bubo Virginianus
	Guat. Barred Owl = Strix fulvescens
	Mottled Owl = Ciccaba virgata
	Pacific Screech Owl = Megascops cooperi
	Spectacled Owl = Pulsatrix perspicillata
	Stygian Owl = Asio stylus
	Whiskered Screech Owl = Megascops trichopsis

	J. Background_Noise
		Description: Ambient noise level at the time of each station's acoustic survey, on a scale from 0 (no background noise) to 3 (steady and loud background noise). 

	K. Station_Comments
		Description: General comments made by survey crews at each station during acoustic surveys. Often describe the background noise or other observations. 



-----------------------------------------
DATA-SPECIFIC INFORMATION FOR: Survey_Table.csv
-----------------------------------------

1. Number of variables: 13

2. Number of cases/rows: 90

3. Missing data codes:
        None applicable

4. Variable List
         
    	A. Survey_ID
       		Description: Unique survey identifier, which corresponds with field of the same name from Stations_Table.csv.

	B. Route_ID
       		Description: Route identifier, which corresponds with field of the same name from Route_Table.csv. Not unique.

    	C. Surveyor_ID
       		Description: Abbreviated name of lead surveyor (Jane West)
                    
    	D. Surveyor_Assistant_ID_1
		Description: Abbreviated name of field assistant.

	E. Surveyor_Assistant_ID_2
		Description: Abbreviated name of additional field assistant, when applicable.
		
	F. Survey_Date
		Description: The date on which each survey (row) was conducted. In the format of M/D/YY. 

	G. Survey_Start_Time
		Description: The time at which each survey was begun (24-hour time format).

	H. Survey_Finish_Time
		Description: The time at which each survey was finished (24-hour time format).

	I. Cloud_Cover_Start
		Description: The percentage of cloud cover at the beginning of each survey.

	J. Cloud_Cover_Finish
		Description: The percentage of cloud cover at the end of each survey.

	K. Precipitation
		Description: The amount of rain (heavy or light or none) during the survey.

	L. Moon_Phase
		Description: The phase of the moon during each survey. 

	M. Survey_Comments
		Description: General comments made by survey crews at each station during acoustic surveys. 



-----------------------------------------
DATA-SPECIFIC INFORMATION FOR: Owls_Table.csv
-----------------------------------------

1. Number of variables: 10

2. Number of cases/rows: 939

3. Missing data codes:
        Only applicable for variable "Owl_Species_ID" (see description below)

4. Variable List
         
    	A. Owl_ID
       		Description: Unique owl identifier.

	B. Stations_ID
       		Description: Unique station identifier, which corresponds with field of the same name in Owls_Table.csv. Not unique.

    	C. Owl_Species_ID
       		Description: Owl species identifier. 

	Barn = Barn Owl, Tyto alba
	FerPy = Ferruginous Pygmy-Owl, Glaucidium brasilianum
	Fulvous = Fulvous Owl, Strix fulvescens
	GrHor = Great Horned Owl, Bubo virginianus
	Mottd = Mottled Owl, Ciccaba virgata
	NoID = these are records that don't have associated owls observed at those survey stations. 
	None = these are records that don't have associated owls observed at those survey stations. 
	PacSc = Pacific Screech Owl, Megascops cooperi
	Specd = Spectacled Owl, Pulsatrix perspicillata
	Styg = Stygian Owl, Asio stylus
	Whisk = Whiskered Screech Owl, Megascops trichopsis
                    
    	D. Owl_Number
		Description: An identifier to demonstrate if there were more than 1 owls detected during any specific Station's acoustic survey. The first owl detected during any station's survey would get designated with the number "1," and the second with the number "2," and so on.

	E. Minute_1
		Description: Binary variable to indicate if the owl was detected in the first minute of the survey (TRUE) or not (FALSE). These observations indicate owl presence during the pre-broadcast period of each survey.
		
	F. Minute_2
		Description: Binary variable to indicate if the owl was detected in the second minute of the survey (TRUE) or not (FALSE). These observations indicate owl presence during the pre-broadcast period of each survey.

	G. Minute_6.12
		Description: Binary variable to indicate if the owl was detected in the range of time from the 6th minute of the survey through the 12th minute of the survey (TRUE) or not (FALSE). These observations indicate owl presence during the post-broadcast period of each survey.

	H. Owl_Distance
		Description: Approximate distance from station at which each owl was detected in meters. Distance was estimated by field crews. 

	I. Owl_Direction
		Description: Approximate bearing from station at which each owl was detected in cardinal directions. Bearings were estimated by field crews. 

	M. Owl_Comments
		Description: General comments made by survey crews at each station during acoustic surveys about that owl detection record.