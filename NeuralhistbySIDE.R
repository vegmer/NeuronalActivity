####### THIS FUNCTION PROCESSES NEX FILES AND GIVES YOU DATA FOR NEURONAL ACTIVITY HISTOGRAMS #######
neuralhist = function(path, startt=0, endt, binw, psthmin, psthmax, event, cueexonly=F, side="both", allResults=F) { 
  
  # Load necessary functions
  functionFolder <- paste(getwd(), "/Functions/", sep="")        
  load(file = paste(functionFolder, "eventfinder.r", sep=""))
  load(file = paste(functionFolder, "neuraldataextract.r", sep=""))
  
  #specify path of nex files
  nexfiles = list.files(path)
  
  #extracts all NeuroExplorer data
  nexdata = lapply(nexfiles, function(x) {
        neuraldataextract.r(paste(path, x, sep = ""))})
  

  ### These two functions split up the NEURONS vs BEHAVIORAL EVENTS for each experiment
  
    #### Selection of hemisphere based on "SIDE" parameter ("drug": drug-treated side; "vehicle":saline treated side; "both":both sides)
   neurons = lapply(seq(1, length(nexdata)), function(x, drugornot=side) {
   nexnames = names(nexdata[[x]])
   neuronidx = grep("sig", nexnames)
    
    neuronumbers <- as.numeric(substr(nexnames[neuronidx], 4, 6))
    code <- c()
    for(i in 1:length(neuronumbers)){
      if(neuronumbers[i]<=8){code[i]="L"} else {code[i]="R"} #I'm pretty sure channels 1-8 are RIGHT hemisphere and 9-16 are LEFT hemisphere in all rats
    }
	   
    # In this part of the code you have to enter your rats manually because the selection of neurons depends on two things: whether the RIGHT connector was in the front or back, and also on WHICH SIDE of the brain the drug was infused
    if(drugornot=="drug"){
            # These rats were given infusions in RIGHT hemisphere
            if(nexdata[[x]]$ratname=="90"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="05"){neuronidx <- neuronidx[code=="L"]} #Rats 90 and 05 had the clips on the LEFT and LEFT array in front, so the sides are actually opposite of what I defined in the previous loop
            if(nexdata[[x]]$ratname=="06"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="08"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="19"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="22"){neuronidx <- neuronidx[code=="R"]}
	    if(nexdata[[x]]$ratname=="29"){neuronidx <- neuronidx[code=="R"]}
            
            # These rats were given infusions in LEFT hemisphere
            if(nexdata[[x]]$ratname=="86"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="10"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="21"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="26"){neuronidx <- neuronidx[code=="L"]}
	    if(nexdata[[x]]$ratname=="30"){neuronidx <- neuronidx[code=="L"]}
    }
    
    if(drugornot=="vehicle"){
            # These rats were given infusions in RIGHT hemisphere
            if(nexdata[[x]]$ratname=="90"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="05"){neuronidx <- neuronidx[code=="R"]} #Rats 90 and 05 had the clips on the LEFT and LEFT array in front, so the sides are actually opposite of what I defined in the previous loop
            if(nexdata[[x]]$ratname=="06"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="08"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="19"){neuronidx <- neuronidx[code=="L"]}
            if(nexdata[[x]]$ratname=="22"){neuronidx <- neuronidx[code=="L"]}
	    if(nexdata[[x]]$ratname=="29"){neuronidx <- neuronidx[code=="L"]}
            
            # These rats were given infusions in LEFT hemisphere
            if(nexdata[[x]]$ratname=="86"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="10"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="21"){neuronidx <- neuronidx[code=="R"]}
            if(nexdata[[x]]$ratname=="26"){neuronidx <- neuronidx[code=="R"]}
	    if(nexdata[[x]]$ratname=="30"){neuronidx <- neuronidx[code=="R"]}
    }
    
    if(drugornot=="both"){neuronidx <- neuronidx}
    
    ####
    if(length(neuronidx)==0){return(NA)} #If no neurons on that side were recorded, return NA
    if(length(neuronidx)>0) {return(nexdata[[x]][neuronidx])}
    
    })
  
	
  events = eventfinder.r(nexdata, event)
  
  #######################################################################
  #specify the interval of timestamps you want to examine  
    sstart = startt
    send = endt
  
  #these two functions simply restrict the timestamp values
  #to the window specified above
  #######################################################
  
  neurons = lapply(seq(1, length(neurons)), function(x) {
    lapply(seq(1, length(neurons[[x]])), function(y) {
     
             neurons[[x]][[y]][which(neurons[[x]][[y]] >= sstart & neurons[[x]][[y]] <= send)]    
     
    })
  })
  
  events = lapply(seq(1, length(events)), function(x) {
    events[[x]][which(events[[x]] >= sstart & events[[x]] <= send)] })
  

  
  #########################################################
  #Now that the relevant data is extracted, we can begin to 
  #analyze it by constructing rasters, histograms, etc.
  #########################################################
  binsize = binw #in ms
  winmin = psthmin #in s
  winmax = psthmax #in s
  
  
  
  #####################################################################
  #This function returns a list of lists of lists:
  #Level 1 is the experiment; Level 2 is each neuron in the experiment;
  #Level 3 is the time relative to the specified event for each trial
  #The object masterlist can then be used to construct PSTHs or rasters
  ########################################################################
  masterlist = lapply(seq(1, length(neurons)), function(x) {
    lapply(seq(1, length(neurons[[x]])), function(y) {
      lapply(seq(1, length(events[[x]])), function(z) {
        stampsidx = which(neurons[[x]][[y]] >= events[[x]][z] - winmin & neurons[[x]][[y]] <= events[[x]][z] + winmax)
        relstamps = neurons[[x]][[y]][stampsidx] - events[[x]][z]
      })
    })    
  })
  
  #########################################################################
  #This function will flag cue-excited neurons if that is the only population   
  #you wish to examine. It uses the criteria outlined in Vince's paper:
  #3 consecutive 10 ms bins in which the FR exceeds the 99.9% conf. interval
  #for a Poisson distribution using 1 s pre-cue as baseline.
  #########################################################################
  cueexidx = lapply(seq(1, length(masterlist)), function(x) {
    lapply(seq(1, length(masterlist[[x]])), function(y) {
      pbin = .02
      threshold = 3
      
      allvals = unlist(masterlist[[x]][[y]])
      hcounts = hist(allvals[which(allvals >= -1 & allvals <= .5)], breaks = seq(-1, .5, pbin), plot = F)$counts 
      
      freq = (hcounts/pbin)/length(masterlist[[x]][[y]])  #just for comparison's sake, to see how counts compare to actual frequency
      baseline = mean(freq[1:(1/pbin)])   
      
      critwin = hcounts[(1/pbin + 1):(1.5/pbin)]         #this is the window (500 ms) in which we look for excitation
      critval = poisson.test(x = sum(hcounts), T = length(hcounts), conf.level = 0.999)$conf.int[2]   #computes the upper limit of the confindence interval
      
      diffs = diff(which(critwin > critval))       #computes the differences in indices for bins exceeding the critical value
      cueex = F
      if(length(which(rle(diffs)$values == 1 & rle(diffs)$lengths >= threshold))>0) (cueex = T)   #looks for consecutive bins (diff equal to 1) that are at least 3 bins long (rle length of at least 3)
      
      return(cueex)  
    })
  })
  
  
  propcueex <- lapply(seq(1, length(cueexidx)), function(x){
    length(which(cueexidx[[x]]==T))/length(cueexidx[[x]])
  })
  

  
  #######################################################################
  #This function returns a list of lists:
  #Level 1 is the experiment
  #Level 2 contains the the frequency histogram (PSTH)
  #for each neuron for the event specified above.
  #
  #The object neurohist can then be used to plot histograms individually or
  #averaged together as in the next function.
  #########################################################################
  neurohist = lapply(seq(1, length(masterlist)), function(x) {
    lapply(seq(1, length(masterlist[[x]])), function(y) {
      allvals = unlist(masterlist[[x]][[y]])
      hcounts = hist(allvals, breaks = seq(-winmin, winmax, binsize/1000), plot = F)$counts 
      freq = (hcounts/(binsize/1000))/length(masterlist[[x]][[y]])
    })
  })
  
  firingmat = do.call("cbind", lapply(seq(1,length(neurohist)), function(x) {
    do.call("cbind", neurohist[[x]]) }))
  
  #When there are no neurons in the selected side, firingmat comes out with columns with only zeroes
  #NonZeroColumns = colSums(firingmat)!=0
  #firingmat = firingmat[,NonZeroColumns]
 
  
  # Average firing of all the neurons in one session per bin and number of neurons per session
  meanFreqPerBin = lapply(seq(1, length(neurohist)), function(x){
    perBin <- as.data.frame(neurohist[[x]], col.names = c(1:length(neurohist[[x]])))
    mean <- rowMeans(perBin)
    return(mean)
  })
  
  SEM <- function(x){sd(x, na.rm=T)/sqrt(length(x))}
  semFreqPerBin = lapply(seq(1, length(neurohist)), function(x){
          perBin <- as.data.frame(neurohist[[x]], col.names = c(1:length(neurohist[[x]])))
          SdErrMean <- apply(perBin, 1, SEM)
          return(SdErrMean)
  })
  
  nNeurons = lapply(seq(1, length(neurohist)), function(x){
    n <- length(neurohist[[x]])
    return(n)
  })
  
  #### What data to use/show
  
  # Cue excited neurons only?
  if(cueexonly == T) ({
    cueexneurons = which(unlist(cueexidx) == T)
    firingmat = firingmat[,cueexneurons]
  })
  
  ### Parameters used 
  parameters = data.frame(path=path, startt=startt, endt=endt, binw=binw, psthmin=psthmin, psthmax=psthmax, event=event, cueexonly=cueexonly, side=side)
  
  
  if(allResults==T){
    return(list(nexdata=nexdata, neurons=neurons, events=events, 
                masterlist=masterlist, cueexidx=cueexidx, propcueex=propcueex, 
                neurohist=neurohist, firingmat=firingmat, meanFreqPerBin=meanFreqPerBin, 
                nNeurons=nNeurons, semFreqPerBin=semFreqPerBin, 
                parameters=parameters))
    
  } else {return(firingmat)}
  
}


#SAVE this function
functionFolder <- paste(getwd(), "/Functions/", sep="")
save(neuralhist, file = paste(functionFolder, "neuralhist.r", sep=""))
