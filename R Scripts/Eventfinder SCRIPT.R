### This function will allow me to indicate my final BIG function (in MV neural histograms.R) which event I want to look at by indicating the number

## THIS IS THE LEGEND FOR THE EVENTS (in order to enter them as an argument in the MV neural histograms function)
#1 = DS_onset 
#2 = NS_onset 
#3 = Entry 
#4 = Exit
#5 = DSonset_Entered
#6 = DSonset_Missed
#7 = NSonset_Entered
#8 = NSonset_Missed
#9 = Entry_DS
#10 = Entry_ITI
#11 = Exit_PostRew



eventfinder = function(nexdata, event) {
        lapply(seq(1, length(nexdata)), function(x) {
                if(event == 1) (events = nexdata[[x]]$DS_onset)
                if(event == 2) (events = nexdata[[x]]$NS_onset)
                
                
                if(event == 3) (events = nexdata[[x]]$Entry)
                if(event == 4) (events = nexdata[[x]]$Exit)
                
                if(event == 5) (events = nexdata[[x]]$DSonset_Entered)
                if(event == 6) (events = nexdata[[x]]$DSonset_Missed)
                
                if(event == 7) (events = nexdata[[x]]$NSonset_Entered)
                if(event == 8) (events = nexdata[[x]]$NSonset_Missed)
                
                if(event == 9) (events = nexdata[[x]]$Entry_DS)
                if(event == 10) (events = nexdata[[x]]$Entry_ITI)
                
                if(event == 11) (events = nexdata[[x]]$Exit_PostRew)
                
                if(event == 12) (events = nexdata[[x]]$Cue)
                if(event == 13) (events = nexdata[[x]]$Baseline_trials)
                
                if(event == 14) (events = nexdata[[x]]$Entry_NS)
                if(event == 15) (events = nexdata[[x]]$Entry_DS_Long)
                
                
                return(events)
        })
}

# Save this function
functionFolder <- paste(getwd(), "/Functions/", sep="")
save(eventfinder, file = paste(functionFolder, "eventfinder.r", sep=""))

#functionFolder <- paste(getwd(), "/R scripts neural parsing/Infusion midsession/Functions/", sep="")
#save(eventfinder, file = paste(functionFolder, "eventfinder.r", sep=""))

