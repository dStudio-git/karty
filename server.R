library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(Cairo)
library(DT)

###--------------------- ZBIÓR FUNKCJI PLOT --------------------------###

## RYSUJ KARTĘ WARTOŚCI INDYWIDUALNYCH ##################################
rysuj.I <- function(dane, estetyka, LCL.checkbox, UCL.checkbox, LCL.value, UCL.value){
  if(LCL.checkbox==TRUE){
    estetyka$LCL <- LCL.value}
  if(UCL.checkbox==TRUE){
    estetyka$UCL <- UCL.value}
  I <- ggplot(data=dane)
  I <- I + geom_line(aes(x=as.numeric(Probka), y=Pomiar, group=Grupa), linetype = "solid", colour = "black", size=0.25)
  I <- I + geom_point(aes(x=as.numeric(Probka), y=Pomiar),size = 2.5, shape = 16, colour = "black")
  ### Zasada.2 ####
  I <- I + geom_point(aes(x=as.numeric(Zasada.2), y=Pomiar), size = 3.5, shape = 16, colour = "green", na.rm = TRUE)
  I <- I + geom_text(data=dane, aes(x=as.numeric(Zasada.2), y=Pomiar, label = "II"), size = TUFTE.label.size, color = "black", vjust= -1.0, hjust = -1.0, na.rm=TRUE)
  ### Zasada.3 ####
  I <- I + geom_point(aes(x=as.numeric(Zasada.3), y=Pomiar), size = 3.5, shape = 16, colour = "blue", na.rm = TRUE)
  I <- I + geom_text(data=dane, aes(x=as.numeric(Zasada.3), y=Pomiar, label = "III"), size = TUFTE.label.size, color = "black", vjust= -1.0, hjust = -1.0, na.rm=TRUE)
  ### Zasada.4 ####
  I <- I + geom_point(aes(x=as.numeric(Zasada.4), y=Pomiar), size = 3.5, shape = 16, colour = "yellow", na.rm = TRUE)
  I <- I + geom_text(data=dane, aes(x=as.numeric(Zasada.4), y=Pomiar, label = "IV"), size = TUFTE.label.size, color = "black", vjust= -1.0, hjust = -1.0, na.rm=TRUE)
  ### Zasada.1 ####
  I <- I + geom_point(aes(x=as.numeric(Zasada.1), y=Pomiar), size = 3.5, shape = 16, colour = "red", na.rm = TRUE)
  I <- I + geom_text(data=dane, aes(x=as.numeric(Zasada.1), y=Pomiar, label = "I"), size = TUFTE.label.size, color = "black", vjust= -1.0, hjust = -1.0, na.rm=TRUE)
  I <- I + geom_text(data=estetyka, aes(poz.label-0.5, mean.by.stage, label=round(mean.by.stage, digits=2)), size = TUFTE.label.size, color = "black", vjust=-0.5)
  I <- I + geom_text(data=estetyka, aes(poz.label-0.5, UCL, label=round(UCL, digits=2)), size = TUFTE.label.size, color = "black", vjust= -0.5)
  I <- I + geom_text(data=estetyka, aes(poz.label-0.5, LCL, label=round(LCL, digits=2)), size = TUFTE.label.size, color = "black", vjust= 1.5)
  I <- I + geom_text(data=estetyka, aes(poz.linia.label, max(UCL), label=Grupa), size = 4.5, color = "black", vjust=-1)
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=mean.by.stage, xend=as.numeric(x_end_lab), yend=mean.by.stage), size=0.25)
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=mean.by.stage+1/3*(UCL-mean.by.stage), xend=as.numeric(x_end_lab), yend=mean.by.stage+1/3*(UCL-mean.by.stage)), size=0.25, linetype = "dashed", alpha = 0.1)
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=mean.by.stage+2/3*(UCL-mean.by.stage), xend=as.numeric(x_end_lab), yend=mean.by.stage+2/3*(UCL-mean.by.stage)), size=0.25, linetype = "dashed", alpha = 0.1)
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=UCL, xend=as.numeric(x_end_lab), yend=UCL), size=0.25, linetype = "dashed")
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=LCL, xend=as.numeric(x_end_lab), yend=LCL), size=0.25, linetype = "dashed")
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=mean.by.stage-1/3*(mean.by.stage-LCL), xend=as.numeric(x_end_lab), yend=mean.by.stage-1/3*(mean.by.stage-LCL)), size=0.25, linetype = "dashed", alpha = 0.1)
  I <- I + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=mean.by.stage-2/3*(mean.by.stage-LCL), xend=as.numeric(x_end_lab), yend=mean.by.stage-2/3*(mean.by.stage-LCL)), size=0.25, linetype = "dashed", alpha = 0.1)
  I <- I + scale_x_continuous(breaks=NULL, labels=NULL) + scale_y_continuous(expand = c(0.3, 0))
  I <- I + theme_tufte(ticks=FALSE, base_size = TUFTE.base.size)  
  I <- I + theme(axis.title=element_blank())
  return(I)
} #######################################################################
## RYSUJ KARTĘ WARTOŚCI INDYWIDUALNYCH ##################################
rysuj.mR <- function(dane, estetyka){
  mR <- ggplot(data=dane)
  mR <- mR + geom_line(aes(x=as.numeric(Probka), y=mR, group=Grupa), linetype = "solid", colour = "black", size=0.25, na.rm=TRUE) 
  mR <- mR + geom_point(aes(x=as.numeric(Probka), y=mR),size = 2.5, shape = 16, colour = "black", na.rm=TRUE)
  mR <- mR + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=mR.mean, xend=as.numeric(x_end_lab), yend=mR.mean), size=0.25)
  mR <- mR + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=0, xend=as.numeric(x_end_lab), yend=0), size=0.25)
  mR <- mR + geom_segment(data=estetyka, aes(x=as.numeric(x_start_lab), y=UCL, xend=as.numeric(x_end_lab), yend=UCL), size=0.25, linetype = "dashed")
  mR <- mR + geom_text(data=estetyka, aes(poz.label-0.5, mR.mean, label=round(mR.mean, digits=2)), size = TUFTE.label.size, color = "black", vjust=-0.5)
  mR <- mR + geom_text(data=estetyka, aes(poz.label-0.5, UCL, label=round(UCL, digits=2)), size = TUFTE.label.size, color = "black", vjust=-0.5)
  mR <- mR + scale_x_continuous(breaks=dane$Probka, labels=dane$Probka)
  mR <- mR + theme_tufte(ticks=FALSE, base_size = TUFTE.base.size)  
  mR <- mR + theme(axis.title=element_blank())
  return(mR)
}
###-------------------KONIEC ZBIORU FUNKCJI PLOT ----------------------###

### -----------------WESTERN ELECTRIC RULES----------------------------###
### seria testow na podanej tablicy (oryginalnej lub po podziale na stage)
### testy urychamiane są przy w funkcji plot na najnowszych danych
### w zależności od testu przeprowadzany jest na jednym punkcie lub grupie

### Dodaj do oryginalnej tablicy (potrzebne są wszystkie punkty) kolumnę z
### pogrupowanymi danymi: 3S, -3S, 2S, -2S, 1S, -1S
zasady.tablica <- function(tabela){
  tabela <- tabela
  tabela <- tabela %>%
    group_by(Grupa) %>%
    arrange(Probka) %>%
    mutate(rozmiar.grupy = n(),
      mov.R = abs(Pomiar - lag(Pomiar)),
      mov.R.mean = mean(mov.R, na.rm=TRUE),
      Mean = mean(Pomiar),
      Sigma = 2.66 * mov.R.mean / 3,
      S = Mean + Sigma,
      m.S = Mean - Sigma,
      two.S = Mean + 2 * Sigma,
      m.two.S = Mean - 2 * Sigma,
      three.S = Mean + 3 * Sigma,
      m.three.S = Mean - 3 * Sigma)
}

## zasada 1 : jeden punkt wypada poza obszar +/- 3 Sigma
## RED
zasada.1  <- function(tabela) {
  tabela <- as.data.frame(tabela)
  test <- tabela %>%
    group_by(Grupa) %>%
    arrange(Probka) %>%
    mutate(Zasada.1 = ifelse(Pomiar > three.S | Pomiar < m.three.S, Probka, "") )
  return(as.data.frame(test))
}

## zasada 2 : dwa z trzech punktów leżą obszarze między 2 a 3 Sigma 
## po tej samej stronie wzlędem średniej
## GREEN
zasada.2  <- function(tabela) {
  tabela <- as.data.frame(tabela)
  test <- tabela %>%
    group_by(Grupa) %>%
    arrange(Probka) %>%
    mutate(Zasada.2.pom =  ifelse(rozmiar.grupy>3,
      ifelse(ifelse(Pomiar>two.S & Pomiar<=three.S, 1, 0)+
          ifelse(lag(Pomiar)>two.S & lag(Pomiar)<=three.S, 1, 0)+
          ifelse(lag(Pomiar,2)>two.S & lag(Pomiar,2)<=three.S, 1, 0)>=2 |
          ifelse(Pomiar<m.two.S & Pomiar>=m.three.S, 1, 0)+
          ifelse(lag(Pomiar)<m.two.S & lag(Pomiar)>=m.three.S, 1, 0)+
          ifelse(lag(Pomiar,2)<m.two.S & lag(Pomiar,2)>=m.three.S, 1, 0)>=2,
        Probka, ""),""),
      Zasada.2 = ifelse(lead(Zasada.2.pom) != "", "",Zasada.2.pom)
    )
  return(as.data.frame(test))
}

### zasada 3 : cztery z pięciu punktów są poza limitem 1 Sigma  #######
### po tej samej stronie wzlędem średniej #############################
#### STAŁE GLOBALNE ###################################################
## BLUE
zasada.3  <- function(tabela) {
  tabela <- as.data.frame(tabela)
  test <- tabela %>%
    group_by(Grupa) %>%
    arrange(Probka) %>%
    mutate(Zasada.3.pom = ifelse(rozmiar.grupy>5,ifelse( ifelse(Pomiar>S, 1, 0)+
        ifelse(lag(Pomiar)>S, 1, 0) + ifelse(lag(Pomiar,2)>S, 1, 0)+
        ifelse(lag(Pomiar,3)>S, 1, 0) + ifelse(lag(Pomiar,4)>S, 1, 0) >= 4 |
        ifelse(Pomiar<m.S, 1, 0) + ifelse(lag(Pomiar)<m.S, 1, 0)+ 
        ifelse(lag(Pomiar,2)<m.S, 1, 0) + ifelse(lag(Pomiar,3)<m.S, 1, 0)+ 
        ifelse(lag(Pomiar,4)<m.S, 1, 0) >= 4,
      Probka, ""),""),
      Zasada.3 = ifelse(lead(Zasada.3.pom) != "", "",Zasada.3.pom)
    )
  return(as.data.frame(test))
}

### zasada 4 : dziewięć punktów z rzędu po jednej stronie od średniej #
### po tej samej stronie wzlędem średniej #############################
zasada.4  <- function(tabela) {
  tabela <- as.data.frame(tabela)
  test <- tabela %>%
    group_by(Grupa) %>%
    arrange(Probka) %>%
    mutate(Zasada.4.pom = ifelse(rozmiar.grupy>9,ifelse(ifelse(Pomiar>Mean, 1, 0)+ ifelse(lag(Pomiar)>Mean, 1, 0)+
        ifelse(lag(Pomiar,2)>Mean, 1, 0) + ifelse(lag(Pomiar,3)>Mean, 1, 0)+
        ifelse(lag(Pomiar,5)>Mean, 1, 0) + ifelse(lag(Pomiar,5)>Mean, 1, 0)+ 
        ifelse(lag(Pomiar,6)>Mean, 1, 0) + ifelse(lag(Pomiar,7)>Mean, 1, 0)+ 
        ifelse(lag(Pomiar,8)>Mean, 1, 0) > 8 |
        ifelse(Pomiar<Mean, 1, 0) + ifelse(lag(Pomiar)<Mean, 1, 0)+ 
        ifelse(lag(Pomiar,2)<Mean, 1, 0) + ifelse(lag(Pomiar,3)<Mean, 1, 0)+ 
        ifelse(lag(Pomiar,4)<Mean, 1, 0) + ifelse(lag(Pomiar,5)<Mean, 1, 0)+ 
        ifelse(lag(Pomiar,6)<Mean, 1, 0) + ifelse(lag(Pomiar,7)<Mean, 1, 0)+ 
        ifelse(lag(Pomiar,8)<Mean, 1, 0) > 8,
      Probka, ""),""),
      Zasada.4 = ifelse(lead(Zasada.4.pom) != "", "",Zasada.4.pom)
    )
  return(as.data.frame(test))
}

WE.tests <- function(dane) {
  dane <- zasady.tablica(dane)
  dane <- zasada.1(dane)
  dane <- zasada.2(dane)
  dane <- zasada.3(dane)
  dane <- zasada.4(dane)
  dane
}

#### STAŁE GLOBALNE ###################################################
TUFTE.base.size = 12
TUFTE.label.size = 3.75
A2 <-	c(1.88,1.23,0.729,0.577,0.483,0.419,0.373,0.337,0.308,0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,0.180,
  0.173,0.167,0.162,0.157,0.153)
D3 <- c(0,0,0,0,0,0.076,0.136,0.184,0.223,0.256,0.283,0.307,0.328,0.347,0.363,0.378,0.391,0.403,0.415,0.425,0.434,0.443,0.451,0.459)
D4 <- c(3.267,2.574,2.282,2.114,2.004,1.924,1.864,1.816,1.777,1.744,1.717,1.693,1.672,1.653,1.637,1.622,1.608,1.597,1.585,1.575,1.566,1.557,1.548,1.541)

server <- function(input, output, session) {
  
  #### REACTIVE VALUES ###################################################
  #### OGÓLNA TABLICA zmieniana na bierząco (reakcja na plot_click) #########
  dat <- reactiveValues(dat=NULL)
  # LICZNIK podziału na stage    
  licznik <- reactiveValues(licznik=2)
  
  #### INPUT FILE - wgranie przez użytkownika pliku nadpisuje ############
  #### reactiveValues dat$dat ######
  observeEvent(input$file1,{
    dat$dat <- read.csv(input$file1$datapath, header=TRUE, sep = ";", quote = "\"", dec=",")
    dat$dat <- dat$dat %>%
      mutate(Probka = row_number())
    
    mR.avg.by.stage <- dat$dat %>%
      group_by(Grupa) %>%
      mutate(mR = abs(Pomiar - lag(Pomiar)))
    
    avg.by.stage.limits <- mR.avg.by.stage %>%
      group_by(Grupa) %>%
      #top_n((input$slider.ImR)) %>%
      summarize(mR.mean = mean(mR, na.rm=TRUE),
        mean.by.stage = mean(Pomiar),
        UCL=mean.by.stage+2.66*mR.mean,
        LCL=mean.by.stage-2.66*mR.mean)
    
    avg.by.stage <- mR.avg.by.stage %>%
      group_by(Grupa) %>%
      summarize(poz.label = Probka[row_number()==n()],
        poz.linia.label = poz.label - (poz.label - Probka[row_number()==1])/2,
        x_start_lab=Probka[row_number()==1],
        x_end_lab=poz.label)
    avg.by.stage.I <- merge(avg.by.stage, avg.by.stage.limits, by="Grupa")
    
    avg.by.stage.mR <- mR.avg.by.stage %>%
      group_by(Grupa) %>%
      summarize(mR.mean = mean(mR, na.rm=TRUE),
        UCL=3.267*mR.mean,
        poz.label = Probka[row_number()==n()],
        x_start_lab=Probka[row_number()==2],
        x_end_lab=Probka[row_number()==n()])
    
    ### Testy na danych wejsciowych funkcji plot
    dane <- WE.tests (dat$dat)

    #output$plot.tabela.ImR <- DT::renderDataTable(
    #  tab <- dane %>%
    #    select(Probka, Grupa, Pomiar, rozmiar.grupy, Zasada.1, Zasada.2, Zasada.3, Zasada.4),
    #  style = 'default', filter = 'none', options = list(pageLength = 13), extensions = 'Responsive', rownames = FALSE)
    
    ## rysuj kartę wartości indywidualnych
    output$kartaI <- renderPlot({
      I <- rysuj.I(dane, avg.by.stage.I, input$checkbox.ImR.LCL,input$checkbox.ImR.UCL, input$bound.ImR.LCL, input$bound.ImR.UCL)
      return(I)
    })
    
    ## rysuj kartę ruchomych rozstępów
    output$karta.mR <- renderPlot({
      mR <- rysuj.mR(mR.avg.by.stage, avg.by.stage.mR)
      return(mR)
    })
  })
  
  
  #### PLOT CLICK - podział tabeli reaktywnej dat$dat na stage'y po ######
  #### każdym kliknięciu przez użytkownika. Na koniec rysowanie ##########
  #### wykresu poprzez nadpisanie aktualnego. Wewnątrz sprawdzenie #######
  #### zasad WESTERN ELECTRIC oraz RESTRICTED AREA #######################
  observeEvent(input$plot_click,{
    if ((round(as.numeric(input$plot_click$x)) %in% restricted.area()$Probka) ) {
      return(NULL)
    } else if (input$plot_click$x > max(dat$dat$Probka)) { 
      return(NULL)
    } else if (input$plot_click$x < min(dat$dat$Probka)) { 
    } else {  
      wybrany.punkt <- round(as.numeric(input$plot_click$x))
      Podgrupa <- dat$dat[wybrany.punkt,"Grupa"]
      
      Before <- dat$dat
      Before <- Before %>%
        filter(Grupa == Podgrupa, Probka < wybrany.punkt)#%>%
      #mutate(Grupa = paste(Podgrupa, " - przed"))
      
      Others <- dat$dat
      Others <- Others %>%
        filter(Grupa != Podgrupa)   
      
      After <- dat$dat
      After <- After %>%
        filter(Grupa == Podgrupa, Probka >= wybrany.punkt) %>%
        mutate(Grupa = paste("Proces", licznik$licznik))
      
      Podzielona <- rbind(Before, After, Others)
      Podzielona <- Podzielona %>%
        arrange(Probka)
      dat$dat <- Podzielona
      
      licznik$licznik <- licznik$licznik + 1
      
      mR.avg.by.stage <- dat$dat %>%
        group_by(Grupa) %>%
        mutate(mR = abs(Pomiar - lag(Pomiar)))
      
      avg.by.stage.limits <- mR.avg.by.stage %>%
        group_by(Grupa) %>%
        #top_n((input$slider.ImR)) %>%
        summarize(mR.mean = mean(mR, na.rm=TRUE),
          mean.by.stage = mean(Pomiar),
          UCL=mean.by.stage+2.66*mR.mean,
          LCL=mean.by.stage-2.66*mR.mean)
      
      avg.by.stage <- mR.avg.by.stage %>%
        group_by(Grupa) %>%
        summarize(poz.label = Probka[row_number()==n()],
          poz.linia.label = poz.label - (poz.label - Probka[row_number()==1])/2,
          x_start_lab=Probka[row_number()==1],
          x_end_lab=poz.label)
      avg.by.stage.I <- merge(avg.by.stage, avg.by.stage.limits, by="Grupa")
      
      avg.by.stage.mR <- mR.avg.by.stage %>%
        group_by(Grupa) %>%
        summarize(mR.mean = mean(mR, na.rm=TRUE),
          UCL=3.267*mR.mean,
          poz.label = Probka[row_number()==n()],
          x_start_lab=Probka[row_number()==2],
          x_end_lab=Probka[row_number()==n()])
      
      ### Testy na danych wejsciowych dla funkcji plot
      dane <- WE.tests (dat$dat)
      
      #output$plot.tabela.ImR <- DT::renderDataTable(
      #  tab <- dane %>%
      #    select(Probka, Grupa, Pomiar, rozmiar.grupy, Zasada.1, Zasada.2, Zasada.3, Zasada.4),
      #  style = 'default', filter = 'none', options = list(pageLength = 13), extensions = 'Responsive', rownames = FALSE)
      
      ## rysuj kartę wartości idywidualnych
      output$kartaI <- renderPlot({
        I <- rysuj.I(dane, avg.by.stage.I, input$checkbox.ImR.LCL,input$checkbox.ImR.UCL, input$bound.ImR.LCL, input$bound.ImR.UCL)
        return(I)
      })
      
      ## rysuj kartę ruchomych rozstępów
      output$karta.mR <- renderPlot({
        mR <- rysuj.mR(mR.avg.by.stage, avg.by.stage.mR)
        return(mR)
      })
      
    } #else
  })
  
  #### PLOT RESET - reset tabeli oraz wykresu do stanu po wgraniu pliku ########
  observeEvent(input$resetButton,{
    dane <- tabela.ImR()
    
    #reset zmiennych dat$dat i licznika
    dat$dat <- tabela.ImR()
    licznik$licznik <- 2
    
    ### Testy na danych wejsciowych dla funkcji plot
    dane <- WE.tests (dat$dat)
    
    ## rysuj kartę wartości idywidualnych
    output$kartaI <- renderPlot({
      I <- rysuj.I(dane, avg.by.stage.I(), input$checkbox.ImR.LCL,input$checkbox.ImR.UCL, input$bound.ImR.LCL, input$bound.ImR.UCL)
      return(I)
    })
    
    ## rysuj kartę ruchomych rozstępów
    output$karta.mR <- renderPlot({
      mR <- rysuj.mR(mR.avg.by.stage(), avg.by.stage.mR())
      return(mR)
    })
  }) #### PLOT RESET KONIEC ##################################################
  
  #### TABELA BAZOWA - tabela, ktora sluży do pierwszego uruchomienia ####
  #### funkcji plot (start aplikacji). Tabela zmienia się tylko gdy ######
  #### wgrany jest nowy plik
  tabela.ImR <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tabela.ImR <- read.csv(inFile$datapath, header=TRUE, sep = ";", quote = "\"", dec=",")
    tabela.ImR <- tabela.ImR %>%
      mutate(Probka = row_number())
  })
  
  #### RESTRICTED AREA - dynamiczna tabela zawierająca obszary ###########
  #### zakazane do kliknięcia - plus/minus n = 2 punkty od ###############
  #### krańców każdego stage'a ###########################################
  restricted.area <- reactive ({
    dane <- dat$dat
    dane <- dane %>%
      group_by(Grupa) %>%
      #arrange(Probka) %>%
      filter(row_number() <= 2 | row_number() >= n()-1)
  })  
  
  
  #### RYSOWANIE TABEL #####################################################
  output$plot.tabela.ImR <- DT::renderDataTable(
    #tabela.ImR(),
    dat$dat,
    style = 'default', filter = 'none', options = list(pageLength = 13), extensions = 'Responsive', rownames = FALSE)
  
  output$plot.tabela.XbarR <- DT::renderDataTable(
    tabela.XbarR(), style = 'default', filter = 'none', options = list(pageLength = 13), extensions = 'Responsive', rownames = FALSE)
  ##########################################################################
  
  ## RYSOWANIE HOOVER ######################################################
  output$hover_info.data = renderText({
    if(is.null(input$plot_hover$x)) return("Data: brak")
    if(input$plot_hover$x == 0) return("Data: brak")
    else paste("Data: ",dat$dat$Czas[[input$plot_hover$x]])
  })
  output$hover_info.pomiar = renderText({
    if(is.null(input$plot_hover$x)) return("Pomiar: brak")
    paste("Pomiar: ",dat$dat$Pomiar[[input$plot_hover$x]])
  })  
  
  tabela.XbarR <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)  
    tabela.XbarR <- read.csv2(inFile$datapath, header=TRUE, sep = ";", quote = "\"",dec=",", stringsAsFactors = FALSE)
    tabela.XbarR <- tabela.XbarR %>%
      mutate(Numer_wiersza = row_number())
  })
  
  tabela.XbarR.average.range <- reactive({
    if (is.null(tabela.XbarR()))
      return(NULL)
    dane <- tabela.XbarR()
    tabela.XbarR.range <- dane %>%
      select(-Numer_wiersza) %>%
      group_by(Grupa,Podgrupa) %>%
      summarize(Range = max(Wynik)-min(Wynik), Mean = mean(Wynik)) %>%
      ungroup() %>%
      mutate(Probka = row_number())
  })
  
  avg.by.stage.X <- reactive({
    if (is.null(tabela.XbarR()))
      return(NULL)
    n <- length(unique(tabela.XbarR()$Pomiar)) / length(unique(tabela.XbarR()$Podgrupa))
    X.avg.by.stage <- tabela.XbarR.average.range()
    X.avg.by.stage.limits <- X.avg.by.stage %>%
      group_by(Grupa) %>%
      top_n(input$slider.XbarR) %>%
      summarize(R.mean = mean(Range, na.rm=TRUE),
        mean.by.stage = mean(Mean),
        UCL=mean.by.stage + A2[n-1] * R.mean,
        LCL=mean.by.stage - A2[n-1] * R.mean
      )
    X.avg.by.stage <- X.avg.by.stage %>%
      group_by(Grupa) %>%
      summarize(poz.label = Probka[row_number()==n()],
        poz.grupa.label = poz.label - (Probka[row_number()==n()] - Probka[row_number()==1])/2
        ,x_start_lab=Probka[row_number()==1],
        x_end_lab=Probka[row_number()==n()]
      )
    data <- merge(X.avg.by.stage, X.avg.by.stage.limits, by="Grupa")
  })
  
  avg.by.stage.R <- reactive({
    if (is.null(tabela.XbarR()))
      return(NULL)
    n <- length(unique(tabela.XbarR()$Pomiar)) / length(unique(tabela.XbarR()$Podgrupa))
    R.avg.by.stage <- tabela.XbarR.average.range()
    avg.by.stage <- R.avg.by.stage %>%
      group_by(Grupa) %>%
      summarize(range.by.stage = mean(Range, na.rm=TRUE),
        UCL = D4[n-1] * range.by.stage,
        LCL = D3[n-1] * range.by.stage,
        poz.label = Probka[row_number()==n()]
        ,x_start_lab=Probka[row_number()==1],
        x_end_lab=Probka[row_number()==n()]
      )
  })
  
  #output$sampling_tree <- renderDiagrammeR({
  #  graf <- create_graph()
  #  df <- sampling_tree.data()
  #  #nodes <- read.csv2('c:/R/Shiny/ImR/nodes.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
  #  nodes <- add_nodes_from_df(graph=graf, df, set_type = NULL, select_cols = c('nody', 'nody.x'),
  #                             drop_cols = c('Row'), rename_attrs = NULL, id_col = NULL, type_col = NULL,
  #                             label_col = NULL)
  #  edges <- read.csv2('c:/R/Shiny/ImR/edges.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
  #  nody_df <- create_nodes(nodes,fontname = "Gill Sans MT", shape="circle", width = 1, label="")
  #  brzegi_df <- create_edges(from=edges$from, to=edges$to, size=1)
  #  graf <- create_graph(nodes_df= nody_df,edges_df = brzegi_df)
  #  render_graph(graf)
  #})
  
  output$sampling_tree.table <- renderDataTable({
    dane <- sampling_tree.data()
  })
  
  sampling_tree.data <- reactive ({
    if (is.null(tabela.XbarR()))
      return(NULL)
    dane <- tabela.XbarR()
    a <- as.data.frame(unique(dane$Grupa))
    a <- a %>%
      mutate(nody = unique(dane$Grupa), Row = row_number()) %>%
      select(Row, nody)
    b <- as.data.frame(unique(dane$Podgrupa))
    b <- b %>%
      mutate(nody = unique(dane$Podgrupa), Row = row_number()) %>%
      select(Row, nody)
    c <- as.data.frame(unique(dane$Pomiar))
    c <- c %>%
      mutate(nody = unique(dane$Pomiar), Row = row_number()) %>%
      select(Row, nody)
    dane <- dplyr::full_join(a, b, by = "Row")
    dane <- dplyr::full_join(dane, c, by = "Row")
    #  select(Grupa, Podgrupa)
    dane <- as.data.frame(dane)
    #dane <- avg.by.stage.X()
  })
  
  mR.avg.by.stage <- reactive({
    mR.avg.by.stage <- tabela.ImR() %>%
      group_by(Grupa) %>%
      mutate(mR = abs(Pomiar - lag(Pomiar)))
  })
  
  avg.by.stage.I <- reactive({
    if (is.null(tabela.ImR()))
      return(NULL)
    mR.avg.by.stage <- mR.avg.by.stage()
    
    avg.by.stage.limits <- mR.avg.by.stage %>%
      group_by(Grupa) %>%
      #top_n((input$slider.ImR)) %>%
      summarize(mR.mean = mean(mR, na.rm=TRUE),
        mean.by.stage = mean(Pomiar),
        UCL=mean.by.stage+2.66*mR.mean,
        LCL=mean.by.stage-2.66*mR.mean)
    
    avg.by.stage <- mR.avg.by.stage %>%
      group_by(Grupa) %>%
      summarize(poz.label = Probka[row_number()==n()],
        poz.linia.label = poz.label - (poz.label - Probka[row_number()==1])/2,
        x_start_lab=Probka[row_number()==1],
        x_end_lab=poz.label
      )
    
    data <- merge(avg.by.stage, avg.by.stage.limits, by="Grupa")
  })
  
  avg.by.stage.mR <- reactive({
    if (is.null(tabela.ImR()))
      return(NULL)
    mR.avg.by.stage <- mR.avg.by.stage()
    avg.by.stage <- mR.avg.by.stage %>%
      group_by(Grupa) %>%
      summarize(mR.mean = mean(mR, na.rm=TRUE),
        UCL=3.267*mR.mean,
        poz.label = Probka[row_number()==n()],
        x_start_lab=Probka[row_number()==2],
        x_end_lab=Probka[row_number()==n()]
      )
  })
  
}

