nexreaderwaves = function(infilename){
        
        infile = file(infilename, open = "rb") # Opens a connection to the file; the connection must be closed later (see end of script)
        
        seek(infile, 264) # seek() moves the position from which data is read.  This should be where the frequency is reported
        frq = readBin(infile, "double", n = 1)    # readBin reads the data in binary format at the specified position
        
        seek(infile, where = 280) # should be where the number of variables is reported
        
        nvars = readBin(infile, what = "int", n = 1)
        
        # Ok, so now we extract important info about the vars
        
        varinfo = list()
        
        for(i in 0:(nvars-1)){
                
                seek(infile, where = 544 + 208*i) # set position at the beginning of the variable headers
                varinfo[[i+1]] = list(
                        type = readBin(infile, what = "int", n = 1),
                        version = readBin(infile, what = "int", n = 1), # won't be used; this just advances the position
                        name = readChar(infile, nchars = 64, useBytes = T), #Variable Name
                        offset = readBin(infile, what = "int", n = 1), # Where in the file the data can be found
                        count = readBin(infile, what = "int", n = 1),  # The number of values
                        wire=readBin(infile, what = "int", n = 1),
                        unitnumber=readBin(infile, what = "int", n = 1),
                        gain= readBin(infile, what = "int", n = 1),
                        filter=readBin(infile, what = "int", n = 1),
                        doublex=readBin(infile, what = "double", n = 1),
                        doubley=readBin(infile, what = "double", n = 1),
                        doubleFreq= readBin(infile, what = "double", n = 1),
                        doubleADtoMV=readBin(infile, what = "double", n = 1),
                        NwavePoints=readBin(infile, what = "int", n = 1),
                        Nmarkers=readBin(infile, what = "double", n = 1),
                        Markerlength=readBin(infile, what = "double", n = 1))
                
                foo = seek(infile, where = NA)
                seek(infile, where = foo + 52)
                varinfo[[i+1]]$nmarkers = readBin(infile, what = "int", n = 1)  # For marker variables; the number of variables associated with each timestamp.
                varinfo[[i+1]]$markerlength = readBin(infile, what = "int", n = 1) # Marker values are stored as characters (for some reason); this number gives the number of characters per value
        }
        
        # And now, lets extract the data
        vars = list()
        
        for(i in 1: length(varinfo)){
                
                # For event type variables (type 1) and neurons (type 0); these should just be timestamps
                if(identical(varinfo[[i]]$type, as.integer(1)) | identical(varinfo[[i]]$type, as.integer(0))){
                        seek(infile, where = varinfo[[i]]$offset)
                        indata = readBin(infile, "int", n = varinfo[[i]]$count)
                        indata = indata/frq
                        
                        # Organizing the output.  Since these are just timestamps, there's not a lot we need to do
                        # The first list element has the info; the second has the timestamps
                        vars[[i]] = list(info = varinfo[[i]], tstamp = indata)
                        vname = gsub(" ","", varinfo[[i]]$name, fixed= T)
                        names(vars)[i] = vname
                        
                        # Here we rename for convenience
                        if(identical(names(vars)[i], "InterpolatedPosition1")){ # the red led
                                names(vars)[i] = "interpolated.red"}
                        
                        if(identical(names(vars)[i], "InterpolatedPosition2")){ # the green led
                                names(vars)[i] = "interpolated.green"}
                } # End of event and neuron data processing statement
                
                
                # Interval variables; these should be pairs of timestamps
                if(identical(varinfo[[i]]$type, as.integer(2))){
                        seek(infile, where = varinfo[[i]]$offset)
                        indata = readBin(infile, "int", n = varinfo[[i]]$count*2) # count gives the number of pairs.  The begin values are first, the end values are next.
                        indata = indata/frq
                        
                        # There are two values for each variable here (start end end).
                        # The first list element is the info; the second is a data frame that contains the starts and ends.
                        tmpint = as.data.frame(matrix(indata, ncol = 2, byrow = F))
                        names(tmpint) = c("start","end")
                        vars[[i]] = list(info = varinfo[[i]], intervals = tmpint)
                        vname = gsub(" ","", varinfo[[i]]$name, fixed= T)  # For convenience sake, we remove the spaces in the names so that they are easier to address
                        names(vars)[i] = vname
                }
                
                # Markers; these can have any number of columns, so we have to accomodate that.
                if(identical(varinfo[[i]]$type, as.integer(6))){
                        # first off, we don't want to bother extracting the strobed data
                        if(isTRUE(all.equal(varinfo[[i]]$name , "Strobed")) ) { # We use isTRUE(all.equal()) to allow "fuzzy" matching
                                vars[[i]] = list(info = varinfo[[i]], warning = "Strobed data is not extracted")
                                names(vars)[i] = "strobed"
                                next }
                        
                        seek(infile, where = varinfo[[i]]$offset)
                        indatatstamp = readBin(infile, "int", n = varinfo[[i]]$count*1)/frq # The first part of the file contains the timestamps
                        
                        # The next part of the file contains the variable names, as well as the variables themselves
                        # all of it (for some reason) encoded as characters.
                        
                        indatavalues = numeric(varinfo[[i]]$count)
                        
                        for(j in 1: varinfo[[i]]$nmarkers) {
                                readChar(infile, nchars = 64, useBytes = T) # This is just the name of the column; we read it to advance the position
                                
                                # A note on this line below: $markerlength is the number of bytes(?) per character and $count is the number of rows in the data
                                foo = readChar(infile, nchars = rep(varinfo[[i]]$markerlength, varinfo[[i]]$count), useBytes = T)  #
                                indatavalues = cbind(indatavalues,as.numeric(foo))
                        }
                        
                        indatavalues = as.data.frame(indatavalues[,-1])
                        indata = cbind(indatatstamp,indatavalues)
                        
                        #  test for the the names of known markers,
                        # and use this to give names to the data columns
                        vars[[i]] = list(info = varinfo[[i]], markers = indata)
                        vname = gsub(" ","", varinfo[[i]]$name, fixed= T)
                        names(vars)[i] = vname
                        
                        if(identical(names(vars)[i], "Position1")){ # the red led
                                names(vars)[i] = "red"
                                names(vars$red$markers) = c("tstamp", "redx", "redy")
                        }
                        
                        if(identical(names(vars)[i], "Position2")){ # the green led
                                names(vars)[i] = "green"
                                names(vars$green$markers) = c("tstamp", "greenx", "greeny")
                        }
                } # End of "Markers" (type == 6) processing segment.
                
                
                ##############
                if(identical(varinfo[[i]]$type, as.integer(3))){
                        vars[[i]] = list(info = varinfo[[i]])
                        seek(infile, where = varinfo[[i]]$offset)
                        #step 1
                        Tsdata = readBin(infile, "int", n = varinfo[[i]]$count)  #these two steps are neccesary because the tstamps are 4 bits while the a/d values are 2 bits
                        waveTstamps=Tsdata/frq
                        #step 2
                        seek(infile, where =varinfo[[i]]$offset+ (varinfo[[i]]$count*4))
                        wavevalsindx=readBin(infile, "int", n = varinfo[[i]]$count* varinfo[[i]]$NwavePoints,size=2)
                        wavevals=wavevalsindx * varinfo[[i]]$doubleADtoMV
                        wavematrixindx=as.matrix (cbind (waveTstamps,matrix(wavevals,ncol=varinfo[[i]]$NwavePoints,byrow=T)))
                        # Organizing the output.  Since these are just timestamps, there's not a lot we need to do
                        # The first list element has the info; the second has the timestamps
                        vars[[i]] = list(info = varinfo[[i]], tstamp =waveTstamps,wvals=wavevals,wavematrix=wavematrixindx)
                        
                        
                }
                
        } # End of loop that extracts variable data
        
        vars$frequency = frq  # This is good info to know
        
        close(infile)  # This step is essential; we must close the connection to this file in order to allow other programs to access it.
        return(vars)
        
}

functionFolder <- paste(getwd(), "/Functions/", sep="")
save(nexreaderwaves, file = paste(functionFolder, "nexreaderwaves.r", sep=""))
