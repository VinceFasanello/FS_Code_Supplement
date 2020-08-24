# FS_Analysis
 Fluctuating Selection Analysis

To Do:
   Issue: FA library 1 and FA library 2 ec and uc matrices do not have identical dimensions.
   Proposed Fix: Rerun FastQtoCounts on FA library 1 and FA library 2 raw fastq files to generate new data from the raw data files. 
   Steps:
      1. check and copy FastQtoCounts script from mm directory to fs directory. Update any library specific bits. 
      2. Prep all unzipped files and place in correct locations
      3. run FastQtoCounts script on library 1 and library 2 data for the FA datasets. 
      4. Assess, hopefully dimensionality mismatch will be resolved
      5. If dimensionality mismatch is not resolved: revisit archive folders on BOX and elsewhere for unmanipulated metadata files from time of library submission. 
