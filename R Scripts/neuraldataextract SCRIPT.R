# This function labels all of the variables I'm going to include in my final list of neuronal and behavioral timestamps (the name of the rat, the file, the session, etc.). It also corrects the timestamps by subtracting the interval that preceded the beginning of the session

neuraldataextract = function (inpath) {

load(file = paste(functionFolder, "/nexreaderwaves.f", sep=""))
  
filepath = inpath
filename = substr(filepath, nchar(filepath)-24, nchar(filepath))
ratname = substr(filename, 10, 11)
exptdate = paste(substr(filename, 1, 2), substr(filename, 3, 4), substr(filename, 5, 6), sep = "/")
expt = substr(filename, 20, 21)
nexfile = nexreaderwaves.f(paste(filepath, sep = ""))
neuralindx = grep("sig", names(nexfile))
onsetindx = grep("onset", names(nexfile))
entryindx = grep("Entry", names(nexfile))
exitindx = grep("Exit", names(nexfile))
eventsindx = c(onsetindx, entryindx, exitindx) #This was the only way to select the "events"
eventdata = nexfile[eventsindx]
neuraldata = nexfile[neuralindx]
eventstamps = lapply(eventdata, function(x) x$tstamp)
neuralstamps = lapply(neuraldata, function(x) x$tstamp)
neuronnames = names(neuralstamps)
eventnames = names(eventstamps)

if(length(eventstamps$SessionStart) < 1) (eventstamps$SessionStart = 0)

starttime = eventstamps$SessionStart
eventstamps = lapply(seq(1, length(eventstamps)), function(x) eventstamps[[x]] - starttime)
correctedstamps = lapply(seq(1, length(neuralstamps)), function(x) neuralstamps[[x]] - starttime)
neuralstamps = lapply(seq(1, length(neuralstamps)), function(x) correctedstamps[[x]][which(correctedstamps[[x]] > 0)])


names(eventstamps)[grep("DS_onset", eventnames)] = "DS_onset"
names(eventstamps)[grep("NS_onset", eventnames)] = "NS_onset"
names(eventstamps)[grep("Entry", eventnames)] = "Entry"
names(eventstamps)[grep("Exit", eventnames)] = "Exit"
names(eventstamps)[grep("Event011", eventnames)] = "timingpulses"
names(eventstamps)[grep("DSonset_Entered", eventnames)] = "DSonset_Entered"
names(eventstamps)[grep("DSonset_Missed", eventnames)] = "DSonset_Missed"
names(eventstamps)[grep("NSonset_Entered", eventnames)] = "NSonset_Entered"
names(eventstamps)[grep("NSonset_Missed", eventnames)] = "NSonset_Missed"
names(eventstamps)[grep("Entry_DS", eventnames)] = "Entry_DS"
names(eventstamps)[grep("Entry_ITI", eventnames)] = "Entry_ITI"
names(eventstamps)[grep("Exit_PostRew", eventnames)] = "Exit_PostRew"
names(eventstamps)[grep("Entry_NS", eventnames)] = "Entry_NS"
names(eventstamps)[grep("Entry_DS_Long", eventnames)] = "Entry_DS_Long" #Entries after DS longer in duration than X (defined in another file)


eventstamps = eventstamps[-which(is.na(names(eventstamps)) == T)]

names(neuralstamps) = neuronnames  #here for safe keeping; might be redundant
eventstamps$ratname = ratname
eventstamps$date = exptdate
eventstamps$filename = filename
eventstamps$expt = expt
#toremove = grep("i", substr(names(neuralstamps), 7,7))
#neuralstamps = neuralstamps[-toremove]
return(c(eventstamps, neuralstamps))
}


functionFolder <- paste(getwd(), "/Functions/", sep="")
save(neuraldataextract, file = paste(functionFolder, "/neuraldataextract.r", sep=""))
