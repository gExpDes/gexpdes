dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    
    sigT <- as.numeric(input$sigT)
    sigF <- as.numeric(input$sigF)
    
    dic(tratas, respos, quali=input$quali,  mcomp=input$mcomp, nl=input$nl, hvar=input$hvar, sigT=input$sigT, sigF=input$sigF)