This repository contains code and data sufficient to reproduce the analyses & results within the manuscript 
"Fluctuating selection facilitates the discovery of broadly effective but difficult to reach adaptive 
outcomes in yeast."

This project consisted of (1) a 500 generation experimental evolution experiment within which the proportions
of evolving barcoded yeast strains were monitored over 50 days (Subdirectory: Evoluitonary_Dynamics), and (2) 


Evolutionary_Dynamics:
	1_FastQtoCounts -- the ED_FastqtoCounts.R script in the R_Workflow directory converts FastQ files from the evolution experiment into counts matrices used in the following step. 
		***FastQ files are not provided (large file size), but are available on demand from Vince Fasanello, vincefasanello@gmail.com.
	2_CountstoAnalysis -- the ED_Prepare_Datasets.rmd script in this directory converts raw counts matrices to dataframes formatted for analysis.


Fitness_Assays:
	1_FastQtoCounts -- the FA_Lib1_FastQtoCounts.R script in the Library_1/R_Workflow directory converts FastQ files from the Fitness Assays sequencing library 1 into counts matrices used in the following step.
			   the FA_Lib2_FastQtoCounts.R script in the Library_2/R_Workflow directory converts FastQ files from the Fitness Assays sequencing library 2 into counts matrices used in the following step.
		***FastQ files are not provided (large file size), but are available on demand from Vince Fasanello, vincefasanello@gmail.com.
	2_CountstoAnalysis -- the FA_Prepare_Datasets.rmd script in this directory combines the raw counts matrices and converts the combined data to dataframes formatted for analysis.
	3_Analysis -- The FS_Analysis.rmd script in this directory conducts all analyses, produces all figures, and provides all data necessary to reproduce the tables included in the manuscript. 


Manually_Modified_Publication_Tables:
	Contains all tables and supplemental tables referenced throughout the manuscript. Tables are produced from 3_Analysis data and outputs with formatting modification. 


Manuscript:
	Contains the manuscript as submitted to BioArchive.


Submitted_Data_&_Metadata:
	Contains the raw counts matrices and required metadata for analysis as submitted to Open Source Foundation (OSF).