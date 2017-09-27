# Neuronal Activity
## Processing of .nex files exported from Neuroexplorer using R.

This repo deals with processing and analyzing files generated with Plexon's Neuroexplorer (NEX) software when recording activity from populations of neurons. Each NEX file is data recorded from the neurons of one animal during one behavioral session. These files record the time stamps of behavioral events (as defined by the user, in this case I used the script called  "NEX event generator 2 cue task" in the folder "NEX event generation").

First, in the working directory, create the following folders:
+ **NEX Files**. Save the files generated with NEX here. Don't have anything else other than these files
+ **R Functions**. Save the following functions:
  + *nexreaderwaves*. First line of processing, it extracts timestamps from NEX files.
  + *eventfinder*. It identifies the events.
  + *neuraldataextract*. It identifies other variables and locates the values of these variables.
  + *neuralhist*. This one is the big function that puts data into a user-friendly form. It creates several data objects that will be useful when analyzing the data.
+ **R Scripts**. In this folder are the scripts that were used for writing the functions in the "Functions" folder. It's good to have them handy in case you need to edit the functions to suit your needs. Other scripts are also included (scripts for other more advanced analyses or creating visualizations). 
+ **Graphs**. You'll save your graphs here.
