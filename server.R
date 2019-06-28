#######################################################
#  Copyright 2019 - Author(s):                        #
#######################################################
#                                                     #
#  Rogerio Kormann <rogerio.kormann@grad.ufsc.br>     #
#  Eduardo Nunes Rosa - <eduardo.nunes@grad.ufsc.br>  #
#  Dr Crysttian Arantes Paixao - <crysttian@gmail.com>#
#                                                     #
#######################################################

server = function(input, output, session) {
  
  observe({
    if(input$action > 0){
      print('1')
      session$sendCustomMessage("myCallbackHandler", "1")
    }
  })
  observe({
    if(input$action1 > 0){
      print('2')
      session$sendCustomMessage("myCallbackHandler", "2")
    }
  })
  observe({
    if(input$action2 > 0){
      if(input$deli=="dic"){
        print('3')
        session$sendCustomMessage("myCallbackHandler", "3")
      }
      if(input$deli=="dbc"){
        print('4')
        session$sendCustomMessage("myCallbackHandler", "4")
      }
      if(input$deli=="dql"){
        print('9')
        session$sendCustomMessage("myCallbackHandler", "9")
      }
      if(input$deli=="faixas"){
        print('12')
        session$sendCustomMessage("myCallbackHandler", "12")
      }
      if(input$deli=="fat2addbc"){
        print('13')
        session$sendCustomMessage("myCallbackHandler", "13")
      }
      if(input$deli=="fat2addic"){
        print('14')
        session$sendCustomMessage("myCallbackHandler", "14")
      }
      if(input$deli=="fat2dbc"){
        print('15')
        session$sendCustomMessage("myCallbackHandler", "15")
      }
      if(input$deli=="fat2dic"){
        print('16')
        session$sendCustomMessage("myCallbackHandler", "16")
      }
      if(input$deli=="fat3addbc"){
        print('17')
        session$sendCustomMessage("myCallbackHandler", "17")
      }
      if(input$deli=="fat3addic"){
        print('18')
        session$sendCustomMessage("myCallbackHandler", "18")
      }
      if(input$deli=="fat3dbc"){
        print('19')
        session$sendCustomMessage("myCallbackHandler", "19")
      }
      if(input$deli=="fat3dic"){
        print('20')
        session$sendCustomMessage("myCallbackHandler", "20")
      }
      if(input$deli=="psub2dbc"){
        print('21')
        session$sendCustomMessage("myCallbackHandler", "21")
      }
      if(input$deli=="psub2dic"){
        print('22')
        session$sendCustomMessage("myCallbackHandler", "22")
      }
    }
  })
  observe({
    if(input$action3 > 0){
      print('2')
      session$sendCustomMessage("myCallbackHandler", "2")
    }
  })
  observe({
    if(input$action4 > 0){
      print('2')
      session$sendCustomMessage("myCallbackHandler", "2")
    }
  })
  observe({
    if(input$action11 > 0){
      if(input$conj=="csv"){
        print('5')
        session$sendCustomMessage("myCallbackHandler", "5")
      }
      else
      if(input$conj=="labest"){
        print('6')
        session$sendCustomMessage("myCallbackHandler", "6")
      }
    }
  })
  observe({
    if(input$action7 > 0){
      print('7')
      session$sendCustomMessage("myCallbackHandler", "7")
    }
  })
  observe({
    if(input$action8 > 0){
      print('8')
      session$sendCustomMessage("myCallbackHandler", "8")
    }
  })
  observe({
    if(input$action10 > 0){
      print('10')
      session$sendCustomMessage("myCallbackHandler", "10")
    }
  })
  observe({
    if (input$fechar > 0) stopApp() #botao fechar do shiny
  })


  ############################################################
  #     Leitura do arquivo CSV                              #
  ############################################################
  whichdataset <- reactive({
    
    if(input$conj=="labest"){
      req(input$vetornome)
      #busca dados do agricolae e LabestData
      dataset1 <- get(input$vetornome)
      return(dataset1)
    }
    
    if(input$conj=="csv"){
    # busca dados no csv
    req(input$file1)
    # ao ler arquivos separados por ponto-e-virgula,
    # ter um separador de virgula causa `read.csv` erro
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       sep = input$sep,
                       dec = input$dec)
      },
      error = function(e) {
        # devolve o erro
        stop(safeError(e))
      }
    )
      return(df)
    }
  })
  
  
  ############################################################
  #     Retorna o nome da variável resposta do modelo        #
  ############################################################
  
  get_resp_var <- reactive({
    formulario=paste(input$trat, input$resp, sep="~")
    return(as.character(formula(formulario))[2])  
  })
  
  get_factors <- reactive ({
    
    formulario=paste(input$trat, input$resp, sep="~")
    factorstr <- as.character(formula(formulario))[3]
    return(sub("\\s","",unlist(strsplit(factorstr,"[*+:]"))))
    
  })
  
  formulaText <- reactive({
    
    paste("Base de dados:", input$vetornome)
    
  })
  
  
  
  ###############################################
  #  Mostra dados do vetor/conjunto de dados e  #
  #            variável selecionados            #
  ###############################################
  
  output$tabela <- renderPrint({
     whichdataset()
   })
  output$view1 <- renderPrint({
     whichdataset()
    
  })
  
  
  ################################################
  # Gera menu do Select variável tratamento      #
  ################################################
  output$menu <- renderPrint({
    
    if ((input$deli!="faixas")&&(input$deli!="fat2addbc")&&(input$deli!="fat2addic")&&(input$deli!="fat2dbc")&&(input$deli!="fat2dic")&&(input$deli!="fat3addbc")&&(input$deli!="fat3addic")&&(input$deli!="fat3dbc")&&(input$deli!="fat3dic")&&(input$deli!="psub2dbc")&&(input$deli!="psub2dic")) {
    dataset<-whichdataset()
    
    selectInput("trat", label = "Selecione a variável Tratamento", 
                choices = names(dataset), 
                selected = 1)
    }
  })
  
  ################################################
  # Gera menu do Select variável resposta        #
  ################################################
  output$menu1<- renderPrint({
    
    dataset<-whichdataset()
    
    selectInput("resp", label = "Selecione a variável Resposta", 
                choices = names(dataset), 
                selected = 3)
  })
  
  ################################################
  # Gera menu do Select variável bloco          #
  ################################################
  output$menu2<- renderPrint({
    
    if((input$deli=="dbc")||(input$deli=="faixas")||(input$deli=="fat2addbc")||(input$deli=="fat2dbc")||(input$deli=="fat3addbc")||(input$deli=="fat3dbc")||(input$deli=="psub2dbc")){
      dataset<-whichdataset()
      
      selectInput("bloc", label = "Selecione a variável Bloco", 
                  choices = names(dataset), 
                  selected = 2)
    }
  })
  
  ################################################
  # Gera menu do Select variável linha          #
  ################################################
  output$menu3<- renderPrint({
    
    if(input$deli=="dql"){
    dataset<-whichdataset()
    
    selectInput("linha", label = "Selecione a variável Linha", 
                choices = names(dataset), 
                selected = 2)
    }
  })
  ################################################
  # Gera menu do Select variável coluna         #
  ################################################
  output$menu4<- renderPrint({
    
    if(input$deli=="dql"){
    dataset<-whichdataset()
    
    selectInput("coluna", label = "Selecione a variável Coluna", 
                choices = names(dataset), 
                selected = 3)
    }
  })
  ################################################
  # Gera menu do Select boxplot dbc              #
  ################################################
  output$menu5<- renderPrint({
    
    dataset<-whichdataset()
    
    selectInput("box1", label = "Selecione a 1 variável do Boxplot do DBC", 
                choices = names(dataset), 
                selected = 1)
  })
  ################################################
  # Gera menu do Select 2 boxplot dbc            #
  ################################################
  output$menu6<- renderPrint({
    
    dataset<-whichdataset()

    selectInput("box2", label = "Selecione a 2 variável do Boxplot do DBC", 
                choices = names(dataset), 
                selected = 3)
  })
  
  ################################################
  # Gera menu do Select variável fator1         #
  ################################################
  output$menu7<- renderPrint({
    
    if((input$deli=="faixas")||(input$deli=="fat2addbc")||(input$deli=="fat2addic")||(input$deli=="fat2dbc")||(input$deli=="fat2dic")||(input$deli=="fat3addbc")||(input$deli=="fat3addic")||(input$deli=="fat3dbc")||(input$deli=="fat3dic")||(input$deli=="psub2dbc")||(input$deli=="psub2dic")){
      dataset<-whichdataset()
      
      selectInput("fator1", label = "Selecione a variável Fator1", 
                  choices = names(dataset), 
                  selected = 3)
    }
  })
  ################################################
  # Gera menu do Select variável fator2         #
  ################################################
  output$menu8<- renderPrint({
    
    if((input$deli=="faixas")||(input$deli=="fat2addbc")||(input$deli=="fat2addic")||(input$deli=="fat2dbc")||(input$deli=="fat2dic")||(input$deli=="fat3addbc")||(input$deli=="fat3addic")||(input$deli=="fat3dbc")||(input$deli=="fat3dic")||(input$deli=="psub2dbc")||(input$deli=="psub2dic")){
      dataset<-whichdataset()
      
      selectInput("fator2", label = "Selecione a variável Fator2", 
                  choices = names(dataset), 
                  selected = 3)
    }
  })
  ################################################
  # Gera menu do Select variável fator3         #
  ################################################
  output$menu15<- renderPrint({
    
    if((input$deli=="fat3addbc")||(input$deli=="fat3addic")||(input$deli=="fat3dbc")||(input$deli=="fat3dic")){
      dataset<-whichdataset()
      
      selectInput("fator3", label = "Selecione a variável Fator3", 
                  choices = names(dataset), 
                  selected = 3)
    }
  })
  
  ################################################
  # Gera menu do qualitatito/quantitativo p Trat #
  ################################################
  output$menu9<- renderPrint({
    
    if((input$deli!="faixas")&&(input$deli!="fat2addbc")&&(input$deli!="fat2addic")&&(input$deli!="fat2dbc")&&(input$deli!="fat2dic")&&(input$deli!="fat3addbc")&&(input$deli!="fat3addic")&&(input$deli!="fat3dbc")&&(input$deli!="fat3dic")&&(input$deli!="psub2dbc")&&(input$deli!="psub2dic")){
      
      radioButtons("quali", "A variável Tratamento é:",
                   choices = c("Qualitativa" = "TRUE",
                               "Quantitativa" = "FALSE"),
                   selected = "TRUE")
    }
  })

  ################################################
  # Gera menu do qualitatito/quantitativo p Fat1 #
  ################################################
  output$menu10<- renderPrint({
    
    if((input$deli=="faixas")||(input$deli=="fat2addbc")||(input$deli=="fat2addic")||(input$deli=="fat2dbc")||(input$deli=="fat2dic")||(input$deli=="fat3addbc")||(input$deli=="fat3addic")||(input$deli=="fat3dbc")||(input$deli=="fat3dic")||(input$deli=="psub2dbc")||(input$deli=="psub2dic")){
      
      radioButtons("quali1", "A variável Fator1 é:",
                   choices = c('Qualitativa' = "TRUE",
                               'Quantitativa' = "FALSE"),
                   selected = "TRUE")
    }
  })
  
  ################################################
  # Gera menu do qualitatito/quantitativo p Fat2 #
  ################################################
  output$menu11<- renderPrint({
    
    if((input$deli=="faixas")||(input$deli=="fat2addbc")||(input$deli=="fat2addic")||(input$deli=="fat2dbc")||(input$deli=="fat2dic")||(input$deli=="fat3addbc")||(input$deli=="fat3addic")||(input$deli=="fat3dbc")||(input$deli=="fat3dic")||(input$deli=="psub2dbc")||(input$deli=="psub2dic")){
      
      radioButtons("quali2", "A variável Fator2 é:",
                   choices = c('Qualitativa' = "TRUE",
                               'Quantitativa' = "FALSE"),
                   selected = "TRUE")
    }
  })
  
  ################################################
  # Gera menu do qualitatito/quantitativo p Fat3 #
  ################################################
  output$menu16<- renderPrint({
    
    if((input$deli=="fat3addbc")||(input$deli=="fat3addic")||(input$deli=="fat3dbc")||(input$deli=="fat3dic")){
      
      radioButtons("quali3", "A variável Fator3 é:",
                   choices = c('Qualitativa' = "TRUE",
                               'Quantitativa' = "FALSE"),
                   selected = "TRUE")
    }
  })
  ################################################
  # Gera menu do resposta tratamento adicional   #
  ################################################
  output$menu12<- renderPrint({
    
    if((input$deli=="fat2addbc")||(input$deli=="fat2addic")||(input$deli=="fat3addbc")||(input$deli=="fat3addic")){
      dataset<-whichdataset()
      
      selectInput("respad", label = "Variável resposta do Trat adcional", 
                  choices = names(dataset), 
                  selected = 3)
    }
  })
  ################################################
  # Gera menu do repeticao                       #
  ################################################
  output$menu13<- renderPrint({
    
    if((input$deli=="fat2addic")||(input$deli=="fat3addic")||(input$deli=="psub2dic")){
      dataset<-whichdataset()
      
      selectInput("repeticao", label = "Selecione a Variável repetição", 
                  choices = names(dataset), 
                  selected = 3)
    }
  })
  ################################################
  # Gera menu do regressao linear                #
  ################################################
  output$menu14<- renderPrint({
    
    req(input$quali)
    if(input$quali=="FALSE")
    if((input$deli=="dic")||(input$deli=="dbc")){
      
      radioButtons("nl1", "Selecione o modelo de regressão:",
                   choices = c('Linear' = "FALSE",
                               'Nao-Linear' = "TRUE"),
                   selected = "FALSE")
    }
  })
  
  ################################################
  # Gera menu do regressao linear                #
  ################################################
  output$menu17<- renderPrint({
      
    labestdados<-ls("package:labestData")
    selectInput("vetornome", "Escolha a base de dados",
                  selected = 'BanzattoQd3.2.1', multiple = FALSE,
                  choices = labestdados)
  })
  
  ################################################
  # Gera teste de comparacao multipla adotado    #
  ################################################
  output$testecomparacao<- renderPrint({
    
    radioButtons("mcomp", "Selecione o teste de comparação múltipla:",
                 choices = c('Tukey' = "lsd",
                             'Duncan' = "duncan",
                             'SNK' = "snk",
                             'Calinski e Corsten' = "ccf",
                             'Bootstrap' = "ccboot",
                             'Scott-Knott' = "sk"),
                 selected = "lsd")
  })
  
  ################################################
  # Gera teste de homogeneidade adotado          #
  ################################################
  output$homogeneidade<- renderPrint({
    
    radioButtons("hvar", "Selecione o teste de homogeneidade de variancias:",
                 choices = c('Bartlett' = "bartlett",
                             'Levene ' = "levene",
                             'Samiuddin' = "samiuddin",
                             'ONeill e Mathews' = "oneillmathews",
                             'Layard' = "layard"),
                 selected = "bartlett")
  })
  
  ################################################
  # Gera modelos de regressao lineares escolhidos#
  ################################################
  output$linear<- renderPrint({
    
    req(input$quali)
    req(input$nl1)
    if((input$quali=="FALSE")&&(input$nl1=="FALSE"))
    if((input$deli=="dic")||(input$deli=="dbc")){
      
      checkboxGroupInput("checkbox", label = "Modelos de regressão lineares:", 
                         choices = list("Reta" = 1, "Parábola" = 2, "Curva" = 3)
                         )
    }
  })
  ################################################
  # Gera modelos de regressao lineares escolhidos#
  ################################################
  output$linear1<- renderPrint({
    
    req(input$quali)
    req(input$nl1)
    if((input$quali=="FALSE")&&(input$nl1=="TRUE"))
    if((input$deli=="dic")||(input$deli=="dbc")){
      checkboxGroupInput("checkbox1", label = "Modelos de regressão lineares:", 
                         choices = list("Potencial" = "pot", "Exponencial" = "exp", "Logmaritma" = "log", "Gompertz" = "gomp")
                         )
    }
  })

  ################################################
  # Gera o botao tipo do conjunto de dados       #
  ################################################
  output$conjunto <- renderPrint({
    
    actionButton(input$conj, label = "Avancar >", class="btn btn-primary", width="100")
    
  })
  
  ################################################
  # Gera o botao avancar da analise             #
  ################################################
  output$avancar <- renderPrint({
    
    actionButton(input$deli, label = "Avancar >", class="btn btn-primary", width="100")

  })
  
  #######################################################
  # Visualização da base de dados                     #
  #######################################################
  output$structure <- renderPrint({
    
    dataset=whichdataset()
    cat('Estrutura do Arquivo: ')
    cat("\n\n")
    str(dataset)
    cat("\n")
    cat("Análise Descritiva:")
    dataset1<-unlist(dataset)
    gds(dataset1)
  }) 
  
  #######################################################
  # Tabela ANOVA DIC                                    #
  #######################################################
  output$anovatable <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    
    sigT <- as.numeric(input$sigT)
    sigF <- as.numeric(input$sigF)
    
    if(input$quali==TRUE)
      tratam = as.factor(tratas)
    if(input$quali==FALSE)
      tratam = as.numeric(tratas)
    
    dic(tratam, respos, quali=input$quali, mcomp=input$mcomp, nl=input$nl1, hvar=input$hvar, sigT=sigT, sigF=sigF)
  }) 
  
  #######################################################
  # Tabela ANOVA    DBC                                 #
  #######################################################
  output$anovatable1 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    
    sigT1 <- as.numeric(input$sigT1)
    sigF1 <- as.numeric(input$sigF1)
    
    dbc(tratas, blocos, respos, quali=input$quali, mcomp=input$mcomp1, nl=input$nl1, hvar=input$hvar1, sigT=sigT1, sigF=sigF1)
  }) 
  
  #######################################################
  # Tabela ANOVA    DQL                                 #
  #######################################################
  output$anovatable2 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    lin<-dataset[input$linha]
    linh <- unlist(lin)
    col<-dataset[input$coluna]
    colu <- unlist(col)
    
    sigT2 <- as.numeric(input$sigT2)
    sigF2 <- as.numeric(input$sigF2)
    
    dql(tratas, linh, colu, respos, quali=input$quali, mcomp=input$mcomp2, sigT=sigT2, sigF=sigF2)
    
  })
  
  #######################################################
  # Tabela ANOVA    faixas                              #
  #######################################################
  output$anovatable3 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT3 <- as.numeric(input$sigT3)
    sigF3 <- as.numeric(input$sigF3)
    
    faixas(fat1, fat2, blocos, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp3, sigT=sigT3, sigF=sigF3)
    
  })
  
  #######################################################
  # Tabela ANOVA fat duplo com 1 trat adicional em DBC  #
  #######################################################
  output$anovatable4 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT4 <- as.numeric(input$sigT4)
    sigF4 <- as.numeric(input$sigF4)
    
    fat2.ad.dbc(fat1, fat2, blocos, respos, respAd, quali = c(input$quali1, input$quali2), mcomp=input$mcomp4, sigT=sigT4, sigF=sigF4)
    
  })
  
  #######################################################
  # Tabela ANOVA fat duplo com 1 trat adicional em DIC  #
  #######################################################
  output$anovatable5 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    repe<-dataset[input$repeticao]
    repet <- unlist(repe)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT5 <- as.numeric(input$sigT5)
    sigF5 <- as.numeric(input$sigF5)
    
    fat2.ad.dic(fat1, fat2, repet, respos, respAd, quali = c(input$quali1, input$quali2), mcomp=input$mcomp5, sigT=sigT5, sigF=sigF5)
    
  })
  
  #######################################################
  # Tabela ANOVA fat duplo em DBC                       #
  #######################################################
  output$anovatable6 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT6 <- as.numeric(input$sigT6)
    sigF6 <- as.numeric(input$sigF6)
    
    fat2.dbc(fat1, fat2, blocos, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp6, sigT=sigT6, sigF=sigF6)
    
  })
  
  #######################################################
  # Tabela ANOVA fat duplo em DIC                       #
  #######################################################
  output$anovatable7 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT7 <- as.numeric(input$sigT7)
    sigF7 <- as.numeric(input$sigF7)
    
    fat2.dic(fat1, fat2, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp7, sigT=sigT7, sigF=sigF7)
  })
  
  #######################################################
  # Tabela ANOVA Fatorial Triplo c trat adic em DBC     #
  #######################################################
  output$anovatable8 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT8 <- as.numeric(input$sigT8)
    sigF8 <- as.numeric(input$sigF8)
    
    fat3.ad.dbc(fat1, fat2, fat3, blocos, respos, respAd, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp8, sigT=sigT8, sigF=sigF8)
  })
  
  #######################################################
  # Tabela ANOVA Fatorial Triplo c trat adic em DIC     #
  #######################################################
  output$anovatable9 <- renderPrint({
  
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    repe<-dataset[input$repeticao]
    repet <- unlist(repe)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT9 <- as.numeric(input$sigT9)
    sigF9 <- as.numeric(input$sigF9)
    
    fat3.ad.dic(fat1, fat2, fat3, repet, respos, respAd, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp9, sigT=sigT9, sigF=sigF9)
   })
  
  #######################################################
  #       Tabela ANOVA Fatorial Triplo em DBC           #
  #######################################################
  output$anovatable10 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    
    sigT10 <- as.numeric(input$sigT10)
    sigF10 <- as.numeric(input$sigF10)
    
    fat3.dbc(fat1, fat2, fat3, blocos, respos, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp10, sigT=sigT10, sigF=sigF10)
  })
  
  #######################################################
  #       Tabela ANOVA Fatorial Triplo em DIC           #
  #######################################################
  output$anovatable11 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    
    sigT11 <- as.numeric(input$sigT11)
    sigF11 <- as.numeric(input$sigF11)
    
    fat3.dic(fat1, fat2, fat3, respos, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp11, sigT=sigT11, sigF=sigF11)
  })
  
  #######################################################
  #     Tabela ANOVA Parcelas subdivididas em DBC       #
  #######################################################
  output$anovatable12 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT12 <- as.numeric(input$sigT12)
    sigF12 <- as.numeric(input$sigF12)
    
    psub2.dbc(fat1, fat2, blocos, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp12, sigT=sigT12, sigF=sigF12)
  })
  
  #######################################################
  #     Tabela ANOVA Parcelas subdivididas em DBC       #
  #######################################################
  output$anovatable13 <- renderPrint({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    repe<-dataset[input$repeticao]
    repet <- unlist(repe)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    
    sigT13 <- as.numeric(input$sigT13)
    sigF13 <- as.numeric(input$sigF13)
    
    psub2.dic(fat1, fat2, repet, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp13, sigT=sigT13, sigF=sigF13)
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # incluindo outliers    DIC                    #
  ################################################
  
  output$plotar1 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # incluindo outliers    DBC                    #
  ################################################
  
  output$plotar <- renderPlotly({
    
    dataset=whichdataset()
    campo1<-dataset[input$box1]
    campo11 <- unlist(campo1)
    campo2<-dataset[input$box2]
    campo21 <- unlist(campo2)
    
    p <- plot_ly(dataset, x = campo11, y= campo21, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
    
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # incluindo outliers    DQL                    #
  ################################################
  
  output$plotar2 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # incluindo outliers    faixas                 #
  ################################################
  
  output$plotar3 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers Fat duplo com 1 Trat adic DBC #
  ################################################
  
  output$plotar4 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers Fat duplo com 1 Trat adic DIC #
  ################################################
  
  output$plotar5 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers Fat duplo DBC                 #
  ################################################
  
  output$plotar6 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers Fat duplo DIC                 #
  ################################################
  output$plotar7 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers Fat triplo trat adic em DBC   #
  ################################################
  output$plotar8 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers Fat triplo trat adic em DIC   #
  ################################################
  output$plotar9 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  #   inclu outliers -  Fatorial Triplo em DBC   #
  ################################################
  output$plotar10 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  #   inclu outliers -  Fatorial Triplo em DIC   #
  ################################################
  output$plotar11 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers - Parcelas subdivididas em DBC#
  ################################################
  output$plotar12 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })
  
  ################################################
  # Gera boxplot das variáveis selecionadas     #
  # inclu outliers - Parcelas subdivididas em DIC#
  ################################################
  output$plotar13 <- renderPlotly({
    
    dataset=whichdataset()
    trata<-dataset[input$trat]
    tratamento <- unlist(trata)
    respo<-dataset[input$resp]
    resposta <- unlist(respo)
    
    p <- plot_ly(dataset, x = ~tratamento, y= ~resposta, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    p
  })

  #######################################################
  # plot de residuos dic                               #
  #######################################################
  output$resid_plot <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    
    sigT <- as.numeric(input$sigT)
    sigF <- as.numeric(input$sigF)
    
    if(input$quali==TRUE)
      tratam = as.factor(tratas)
    if(input$quali==FALSE)
      tratam = as.numeric(tratas)

    dic.model<-dic(tratam, respos, quali=input$quali,  mcomp=input$mcomp, hvar=input$hvar, sigT=sigT, sigF=sigF)
    plotres(dic.model)
    
  })
  #######################################################
  # plot de residuos    dbc                             #
  #######################################################
  output$resid_plot1 <- renderPlot({
    
    dataset<-whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)

    
    dbc.model<-dbc(tratas, linh, colu, respos, quali=input$quali, mcomp=input$mcomp1, sigT=sigT1, sigF=sigF1)
    plotres(dbc.model)
  })
  
  #######################################################
  # plot de residuos    DQL                             #
  #######################################################
  output$resid_plot2 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    lin<-dataset[input$linha]
    linh <- unlist(lin)
    col<-dataset[input$coluna]
    colu <- unlist(col)
    
    
    dql.model<-dql(tratas, linh, colu, respos, quali=input$quali, mcomp=input$mcomp2, sigT=sigT2, sigF=sigF2)
    plotres(dql.model)
  })
  
  #######################################################
  # plot de residuos    faixas                          #
  #######################################################
  output$resid_plot3 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT3 <- as.numeric(input$sigT3)
    sigF3 <- as.numeric(input$sigF3)
    
    faixas.model<-faixas(fat1, fat2, blocos, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp3, sigT=sigT3, sigF=sigF3)
    plotres(faixas.model)
  })
  
  #######################################################
  # plot de residuos Fat duplo com 1 Trat adic em DBC   #
  #######################################################
  output$resid_plot4 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT4 <- as.numeric(input$sigT4)
    sigF4 <- as.numeric(input$sigF4)
    
    fat2.model<-fat2.ad.dbc(fat1, fat2, blocos, respos, respAd, quali = c(input$quali1, input$quali2), mcomp=input$mcomp4, sigT=sigT4, sigF=sigF4)
    plotres(fat2.model)
  })
  
  #######################################################
  # plot de residuos Fat duplo com 1 Trat adic em DIC   #
  #######################################################
  output$resid_plot5 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    repe<-dataset[input$repeticao]
    repet <- unlist(repe)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT5 <- as.numeric(input$sigT5)
    sigF5 <- as.numeric(input$sigF5)
    
    fat2.model<-fat2.ad.dic(fat1, fat2, repet, respos, respAd, quali = c(input$quali1, input$quali2), mcomp=input$mcomp5, sigT=sigT5, sigF=sigF5)
    plotres(fat2.model)
  })
  
  
  #######################################################
  # plot de residuos Fat duplo DBC                      #
  #######################################################
  output$resid_plot6 <- renderPlot({

    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT6 <- as.numeric(input$sigT6)
    sigF6 <- as.numeric(input$sigF6)

    fat2.model<-fat2.dbc(fat1, fat2, blocos, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp6, sigT=sigT6, sigF=sigF6)
    plotres(fat2.model)
  })
  
  #######################################################
  # plot de residuos Fat duplo DIC                      #
  #######################################################
  output$resid_plot7 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT7 <- as.numeric(input$sigT7)
    sigF7 <- as.numeric(input$sigF7)
    
    fat2.model<-fat2.dic(fat1, fat2, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp7, sigT=sigT7, sigF=sigF7)
    plotres(fat2.model)
  })
  
  #######################################################
  # plot de residuos Fat Triplo com um trat adic em DBC #
  #######################################################
  output$resid_plot8 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT8 <- as.numeric(input$sigT8)
    sigF8 <- as.numeric(input$sigF8)
    
    fat3.model<-fat3.ad.dbc(fat1, fat2, fat3, blocos, respos, respAd, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp8, sigT=sigT8, sigF=sigF8)
    plotres(fat3.model)
  })
  
  #######################################################
  # plot de residuos Fat Triplo com um trat adic em DIC #
  #######################################################
  output$resid_plot9 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    repe<-dataset[input$repeticao]
    repet <- unlist(repe)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    respadi<-dataset[input$respad]
    respAd <- unlist(respadi)
    
    sigT9 <- as.numeric(input$sigT9)
    sigF9 <- as.numeric(input$sigF9)
    
    fat3.model<-fat3.ad.dic(fat1, fat2, fat3, repet, respos, respAd, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp9, sigT=sigT9, sigF=sigF9)
    plotres(fat3.model)
  })
  
  #######################################################
  #     plot de residuos Fatorial Triplo em DBC         #
  #######################################################
  output$resid_plot10 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    
    sigT10 <- as.numeric(input$sigT10)
    sigF10 <- as.numeric(input$sigF10)
    
    fat3.model<-fat3.dbc(fat1, fat2, fat3, blocos, respos, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp10, sigT=sigT10, sigF=sigF10)
    plotres(fat3.model)
  })
  
  #######################################################
  #     plot de residuos Fatorial Triplo em DIC         #
  #######################################################
  output$resid_plot11 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    fatoo<-dataset[input$fator3]
    fat3 <- unlist(fatoo)
    
    sigT11 <- as.numeric(input$sigT11)
    sigF11 <- as.numeric(input$sigF11)
    
    fat3.model<-fat3.dic(fat1, fat2, fat3, respos, quali = c(input$quali1, input$quali2, input$quali3), mcomp=input$mcomp11, sigT=sigT11, sigF=sigF11)
    plotres(fat3.model)
  })
  
  #######################################################
  #   plot de residuos Parcelas subdivididas em DBC     #
  #######################################################
  output$resid_plot12 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    bloco<-dataset[input$bloc]
    blocos <- unlist(bloco)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT12 <- as.numeric(input$sigT12)
    sigF12 <- as.numeric(input$sigF12)
    
    psub2.model<-psub2.dbc(fat1, fat2, blocos, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp12, sigT=sigT12, sigF=sigF12)
    plotres(psub2.model)
  })
  
  #######################################################
  #   plot de residuos Parcelas subdivididas em DIC     #
  #######################################################
  output$resid_plot13 <- renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    repe<-dataset[input$repeticao]
    repet <- unlist(repe)
    fat<-dataset[input$fator1]
    fat1 <- unlist(fat)
    fato<-dataset[input$fator2]
    fat2 <- unlist(fato)
    
    sigT13 <- as.numeric(input$sigT13)
    sigF13 <- as.numeric(input$sigF13)
    
    psub2.model<-psub2.dic(fat1, fat2, repet, respos, quali = c(input$quali1, input$quali2), mcomp=input$mcomp13, sigT=sigT13, sigF=sigF13)
    plotres(psub2.model)
  })
  
  #######################################################
  #     plot Modelos de regressão lineares             #
  #######################################################
  output$modelos <-  renderPlot({
    
    dataset=whichdataset()
    respo<-dataset[input$resp]
    respos <- unlist(respo)
    trata<-dataset[input$trat]
    tratas <- unlist(trata)
    
    sigT <- as.numeric(input$sigT)
    sigF <- as.numeric(input$sigF)
    
    if(input$quali==TRUE)
      tratam = as.factor(tratas)
    if(input$quali==FALSE)
      tratam = as.numeric(tratas)
    
    dic.model<-dic(tratam, respos, quali=input$quali,  mcomp=input$mcomp, hvar=input$hvar, sigT=sigT, sigF=sigF)
    
    if((length(input$checkbox))>0){
    m<-length(input$checkbox)
    par(mfrow = c(2, 2))
    for (i in 1:m) {
      if(strcmp(input$checkbox[i],"1")){
        #######################################################
        #     parte da funcao graficos retirada do expdes     #
        #######################################################
          a <- dic.model
          mod = TRUE; main = " "; sub = " "; xlab = "Niveis (X)"; 
          ylab = "Resposta (Y)"; pch = 19; xlim = NULL; ylim = NULL; 
          bty = "o"
          a <- a$reg
          xob <- as.numeric(as.vector(a$"Quadro de medias"[, 1]))
          x <- seq(min(xob), max(xob), by = 0.1)
          b0 <- a$"Coeficientes reta"[1]
          b1 <- a$"Coeficientes reta"[2]
          y <- b0 + b1 * x
          yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
          if (is.null(ylim) == TRUE) 
            ylim = c(min(y, yob), max(y, yob))
          plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
               ylab = ylab, xlim = xlim, ylim = ylim)
          title(main = "Reta")
          if (mod == TRUE) 
            mtext(paste("y =", round(b0, 3), "+", round(b1, 3), 
                        "x  ", " R^2 = ", round(a$"R2 reta" * 100, 2), 
                        "%"), side = 3)
          points(xob, yob, pch = pch)
          #######################################################
          #  fim parte da funcao graficos retirada do expdes    #
          #######################################################
        }
      if(strcmp(input$checkbox[i],"2")){
        #######################################################
        #     parte da funcao graficos retirada do expdes     #
        #######################################################
        a <- dic.model
        mod = TRUE; main = " "; sub = " "; xlab = "Niveis (X)"; 
        ylab = "Resposta (Y)"; pch = 19; xlim = NULL; ylim = NULL; 
        bty = "o"
        a <- a$reg
        xob <- as.numeric(as.vector(a$"Quadro de medias"[, 1]))
        x <- seq(min(xob), max(xob), by = 0.1)
        b0 <- a$"Coeficientes parabola"[1]
        b1 <- a$"Coeficientes parabola"[2]
        b2 <- a$"Coeficientes parabola"[3]
        y <- b0 + b1 * x + b2 * x^2
        yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
        if (is.null(ylim) == TRUE) 
          ylim = c(min(y, yob), max(y, yob))
        plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
             ylab = ylab, xlim = xlim, ylim = ylim)
        title(main = "Parábola")
        if (mod == TRUE) 
          mtext(paste("y = ", round(b0, 3), "+", round(b1, 
                                                       3), "x+", round(b2, 3), "x^2  ", " R^2 = ", round(a$"R2 parabola" * 
                                                                                                           100, 2), "%"), side = 3)
        points(xob, yob, pch = pch)
        #######################################################
        #  fim parte da funcao graficos retirada do expdes    #
        #######################################################
      }
      if(strcmp(input$checkbox[i],"3")){
        #######################################################
        #     parte da funcao graficos retirada do expdes     #
        #######################################################
        a <- dic.model
        mod = TRUE; main = " "; sub = " "; xlab = "Niveis (X)"; 
        ylab = "Resposta (Y)"; pch = 19; xlim = NULL; ylim = NULL; 
        bty = "o"
        a <- a$reg
        x <- seq(min(xob), max(xob), by = 0.1)
        b0 <- a$"Coeficientes cubica"[1]
        b1 <- a$"Coeficientes cubica"[2]
        b2 <- a$"Coeficientes cubica"[3]
        b3 <- a$"Coeficientes cubica"[4]
        y <- b0 + b1 * x + b2 * x^2 + b3 * x^3
        yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
        if (is.null(ylim) == TRUE) 
          ylim = c(min(y, yob), max(y, yob))
        plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
             ylab = ylab, xlim = xlim, ylim = ylim)
        title(main = "Curva")
        if (mod == TRUE) 
          mtext(paste("y = ", round(b0, 3), "+", round(b1, 
                                                       3), "x+", round(b2, 3), "x^2+", round(b3, 3), 
                      "x^3  ", " R^2 = ", round(a$"R2 cubica" * 100, 
                                                2), "%"), side = 3)
        points(xob, yob, pch = pch)
        #######################################################
        #  fim parte da funcao graficos retirada do expdes    #
        #######################################################
        }
    }
    }
   
    if((length(input$checkbox1))>0){
    n<-length(input$checkbox1)
    for (j in 1:n) {
      if(strcmp(input$checkbox1[j], "pot"))
        graficos(dic.model,grau="pot")
      if(strcmp(input$checkbox1[j], "exp"))
        graficos(dic.model,grau="exp")
      if(strcmp(input$checkbox1[j], "log"))
        graficos(dic.model,grau="log")
      if(strcmp(input$checkbox1[j], "gomp"))
        graficos(dic.model,grau="gomp")
      }
    }
  })
 
  #######################################################
  # UI  plot no DIC Modelos de regressão lineares      #
  ####################################################### 
  output$uidicmodelos <- renderUI({
    if(input$quali == 'FALSE') {
      tabsetPanel(
        tabPanel("Box Plot",           plotlyOutput("plotar1")),
        tabPanel("Pressuposicoes/Residuo",          plotOutput("resid_plot")),
        tabPanel("ANAVA",       verbatimTextOutput("anovatable")),
        tabPanel("Modelos de regressão lineares",   plotOutput("modelos"))
      )
    } else {
      tabsetPanel(
        tabPanel("Box Plot",           plotlyOutput("plotar1")),
        tabPanel("Pressuposicoes/Residuo",          plotOutput("resid_plot")),
        tabPanel("ANAVA",       verbatimTextOutput("anovatable"))
      )
    }
  })
  
  # caixa de seleção
  output$value <- renderPrint({ input$trat })
  
  
  #######################################################
  #           Busca dos dados do LabestData             #
  #               Codigo proprio do Labest             #
  #######################################################
  output$DOC <- renderPrint({
    if (input$vetornome != "") {
      tmp <- tempfile()
      static_help("labestData", input$vetornome, tmp)
      out <- readLines(tmp)
      headfoot <- grep("body", out)
      cat(out[(headfoot[1] + 1):(headfoot[2] - 2)],
          sep = "\n")
    } else return("Processando")
  })
  output$TABLE <- renderPrint({
    if (input$vetornome != "") {
      da <- eval(parse(text = input$vetornome))
      a <- switch(class(da),
                  data.frame = da,
                  numeric = {
                    da <- data.frame(da)
                    names(da) <- input$vetornome
                    da

                  },
                  integer = {
                    da <- data.frame(da)
                    names(da) <- input$vetornome
                    da
                  })
      dig <- sapply(a, howmanydigits)
      #Encoding(dig)

      print(xtable(a, digits = c(0, dig)), type = "html")
    } else return("Processando")
  })
  static_help <- function(pkg, topic, out,
                          links = tools::findHTMLlinks()) {
    pkgRdDB = tools:::fetchRdDB(file.path(
      find.package(pkg), 'help', pkg))
    force(links)
    tools::Rd2HTML(pkgRdDB[[topic]], out, package = pkg,
                   Links = links, no_links = is.null(links))
  }
  howmanydigits <- function(x) {
    x <- na.omit(x)
    if (is.numeric(x) && all(x%%1 == 0)) {
      0
    } else if (is.numeric(x)) {
      1 + floor(log10(1/min(diff(sort(unique(x))))))
    } else {
      0
    }
  }
  output$TABLE_DOC <- renderUI({
    if(is.null(input$vetornome)) {
      return()
    } else {
      tabsetPanel(
        tabPanel("Documentação", uiOutput("DOC")),
        tabPanel("Tabela de dados",tableOutput("TABLE"))
      )
    }
  })
  #######################################################
  #         fim parte do LabestData                     #
  #######################################################
 }