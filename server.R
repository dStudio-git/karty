library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(shinydashboard)
library(extrafont)
library(Rttf2pt1)
library(extrafontdb)
library(knitr)
library(tidyr)
library(Cairo)
library(DT)

server <- function(input, output, session) {
  A2 <-	c(1.88,1.23,0.729,0.577,0.483,0.419,0.373,0.337,0.308,0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,0.180,
          0.173,0.167,0.162,0.157,0.153)
  D3 <- c(0,0,0,0,0,0.076,0.136,0.184,0.223,0.256,0.283,0.307,0.328,0.347,0.363,0.378,0.391,0.403,0.415,0.425,0.434,0.443,0.451,0.459)
  D4 <- c(3.267,2.574,2.282,2.114,2.004,1.924,1.864,1.816,1.777,1.744,1.717,1.693,1.672,1.653,1.637,1.622,1.608,1.597,1.585,1.575,1.566,1.557,1.548,1.541)
  
  tabela.ImR <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tabela.ImR <- read.csv(inFile$datapath, header=TRUE, sep = ";", quote = "\"", dec=",")
    tabela.ImR <- tabela.ImR %>%
      mutate(Probka = row_number()) %>%
      group_by(Linia) # na uzytek top_n()
  })
  
  output$plot.tabela.ImR <- DT::renderDataTable(
    tabela.ImR(),
    style = 'default',
    filter = 'none',
    options = list(pageLength = 13),
    extensions = 'Responsive'
  )
  
  output$plot.tabela.XbarR <- DT::renderDataTable(
    tabela.XbarR(),
    style = 'default',
    filter = 'none',
    options = list(pageLength = 13),
    extensions = 'Responsive'
  )
  
  mR.avg.by.stage <- reactive({
    mR.avg.by.stage <- tabela.ImR() %>%
      group_by(Linia) %>%
      mutate(mR = abs(Pomiar - lag(Pomiar)))
  })
  
  avg.by.stage.I <- reactive({
    if (is.null(tabela.ImR()))
      return(NULL)
    mR.avg.by.stage <- mR.avg.by.stage()
    
    avg.by.stage.limits <- mR.avg.by.stage %>%
      group_by(Linia) %>%
      top_n((input$slider.ImR)) %>%
      summarize(mR.mean = mean(mR, na.rm=TRUE),
                mean.by.stage = mean(Pomiar),
                UCL=mean.by.stage+2.66*mR.mean,
                LCL=mean.by.stage-2.66*mR.mean)
    
    avg.by.stage <- mR.avg.by.stage %>%
      group_by(Linia) %>%
      summarize(poz.label = Probka[row_number()==n()],
                poz.linia.label = poz.label - (poz.label - Probka[row_number()==1])/2,
                x_start_lab=Probka[row_number()==1],
                x_end_lab=poz.label
      )
    
    data <- merge(avg.by.stage, avg.by.stage.limits, by="Linia")
  })
  
  avg.by.stage.mR <- reactive({
    if (is.null(tabela.ImR()))
      return(NULL)
    mR.avg.by.stage <- mR.avg.by.stage()
    avg.by.stage <- mR.avg.by.stage %>%
      group_by(Linia) %>%
      summarize(mR.mean = mean(mR, na.rm=TRUE),
                UCL=3.267*mR.mean,
                poz.label = Probka[row_number()==n()],
                x_start_lab=Probka[row_number()==2],
                x_end_lab=Probka[row_number()==n()]
      )
  })
  
  output$kartaI <- renderPlot({
    if (is.null(tabela.ImR()))
      return(NULL)
    dane <- tabela.ImR()
    avg.by.stage <- avg.by.stage.I()
    if(input$checkbox.ImR.LCL==TRUE){
      avg.by.stage$LCL <- input$bound.ImR.LCL}
    if(input$checkbox.ImR.UCL==TRUE){
      avg.by.stage$UCL <- input$bound.ImR.UCL}
    
    I <- ggplot(data=dane)
    I <- I + geom_line(aes(x=as.numeric(Probka), y=Pomiar, group=Linia), linetype = "solid", colour = "black", size=0.25) 
    I <- I + geom_point(aes(x=as.numeric(Probka), y=Pomiar),size = 2.5, shape = 16, colour = "black")
    I <- I + geom_text(data=avg.by.stage, aes(poz.label-0.5, mean.by.stage, label=round(mean.by.stage, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    I <- I + geom_text(data=avg.by.stage, aes(poz.label-0.5, UCL, label=round(UCL, digits=2)), size = 3.5, color = "black", vjust= 1.5)
    I <- I + geom_text(data=avg.by.stage, aes(poz.label-0.5, LCL, label=round(LCL, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    I <- I + geom_text(data=avg.by.stage, aes(poz.linia.label, max(UCL), label=Linia), size = 4.5, color = "black", vjust=-1)
    I <- I + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=mean.by.stage, xend=as.numeric(x_end_lab), yend=mean.by.stage), size=0.25)
    I <- I + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=LCL, xend=as.numeric(x_end_lab), yend=LCL), size=0.25, linetype = "dashed")
    I <- I + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=UCL, xend=as.numeric(x_end_lab), yend=UCL), size=0.25, linetype = "dashed")
    I <- I + scale_x_continuous(breaks=NULL, labels=NULL) + scale_y_continuous(expand = c(0.2, 0))
    I <- I + theme_tufte(ticks=FALSE)  
    I <- I + theme(axis.title=element_blank())
    return(I)
  })
  
  output$karta.mR <- renderPlot({
    if (is.null(avg.by.stage.mR()))
      return(NULL)
    dane <- mR.avg.by.stage()
    avg.by.stage.mR <- avg.by.stage.mR()
    mR <- ggplot(data=dane)
    mR <- mR + geom_line(aes(x=as.numeric(Probka), y=mR, group=Linia), linetype = "solid", colour = "black", size=0.25, na.rm=TRUE) 
    mR <- mR + geom_point(aes(x=as.numeric(Probka), y=mR),size = 2.5, shape = 16, colour = "black", na.rm=TRUE)
    mR <- mR + geom_segment(data=avg.by.stage.mR, aes(x=as.numeric(x_start_lab), y=mR.mean, xend=as.numeric(x_end_lab), yend=mR.mean), size=0.25)
    mR <- mR + geom_segment(data=avg.by.stage.mR, aes(x=as.numeric(x_start_lab), y=0, xend=as.numeric(x_end_lab), yend=0), size=0.25)
    mR <- mR + geom_segment(data=avg.by.stage.mR, aes(x=as.numeric(x_start_lab), y=UCL, xend=as.numeric(x_end_lab), yend=UCL), size=0.25, linetype = "dashed")
    mR <- mR + geom_text(data=avg.by.stage.mR, aes(poz.label-0.5, mR.mean, label=round(mR.mean, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    mR <- mR + geom_text(data=avg.by.stage.mR, aes(poz.label-0.5, UCL, label=round(UCL, digits=2)), size = 3.5, color = "black", vjust=1.5)
    mR <- mR + scale_x_continuous(breaks=dane$Probka, labels=dane$Probka)
    mR <- mR + theme_tufte(ticks=FALSE)  
    mR <- mR + theme(axis.title=element_blank())
    return(mR)
  })
  
  output$graficznaX <- renderPlot({
    if (is.null(tabela.XbarR()))
      return(NULL)
    dane = tabela.XbarR.average.range()
    avg.by.stage <- avg.by.stage.X()
    if(input$checkbox.XbarR.LCL==TRUE){
      avg.by.stage$LCL <- input$bound.XbarR.LCL}
    if(input$checkbox.XbarR.UCL==TRUE){
      avg.by.stage$UCL <- input$bound.XbarR.UCL}
    
    g <- ggplot(data=dane)
    g <- g + geom_line(aes(x=as.numeric(Probka), y=Mean, group=Grupa), linetype = "solid", colour = "black", size=0.25)
    g <- g + geom_point(aes(x=as.numeric(Probka), y=Mean),size = 2.5, shape = 16, colour = "black")
    g <- g + geom_text(data=avg.by.stage, aes(poz.label-0.5, as.numeric(mean.by.stage), label=round(mean.by.stage, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    g <- g + geom_text(data=avg.by.stage, aes(as.numeric(poz.label)-0.5, as.numeric(UCL), label=round(as.numeric(UCL), digits=2)), size = 3.5, color = "black", vjust= 1.5)
    g <- g + geom_text(data=avg.by.stage, aes(poz.label-0.5, as.numeric(LCL), label=round(LCL, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    g <- g + geom_text(data=avg.by.stage, aes(poz.grupa.label, as.numeric(max(UCL)), label=Grupa), size = 4.5, color = "black", vjust=-1)
    g <- g + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=as.numeric(mean.by.stage), xend=as.numeric(x_end_lab), yend=as.numeric(mean.by.stage)), size=0.25)
    g <- g + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=as.numeric(LCL), xend=as.numeric(x_end_lab), yend=as.numeric(LCL)), size=0.25, linetype = "dashed")
    g <- g + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=as.numeric(UCL), xend=as.numeric(x_end_lab), yend=as.numeric(UCL)), size=0.25, linetype = "dashed")
    g <- g + scale_x_continuous(breaks=NULL, labels=NULL) + scale_y_continuous(expand = c(0.2, 0))
    g <- g + theme_tufte(ticks=FALSE)
    g <- g + theme(axis.title=element_blank())
    return(g)  
  })
  
  output$graficznaR <- renderPlot({
    if (is.null(tabela.XbarR()))
      return(NULL)
    dane = tabela.XbarR.average.range()
    avg.by.stage <- avg.by.stage.R()
    r <- ggplot(data=dane)
    r <- r + geom_line(aes(x=as.numeric(Probka), y=Range, group=Grupa), linetype = "solid", colour = "black", size=0.25)
    r <- r + geom_point(aes(x=as.numeric(Probka), y=Range),size = 2.5, shape = 16, colour = "black")
    r <- r + geom_text(data=avg.by.stage, aes(poz.label-0.5, range.by.stage, label=round(range.by.stage, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    r <- r + geom_text(data=avg.by.stage, aes(poz.label-0.5, UCL, label=round(UCL, digits=2)), size = 3.5, color = "black", vjust= 1.5)
    r <- r + geom_text(data=avg.by.stage, aes(poz.label-0.5, LCL, label=round(LCL, digits=2)), size = 3.5, color = "black", vjust=-0.5)
    r <- r + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=range.by.stage, xend=as.numeric(x_end_lab), yend=range.by.stage), size=0.25)
    r <- r + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=LCL, xend=as.numeric(x_end_lab), yend=LCL), size=0.25, linetype = "dashed")
    r <- r + geom_segment(data=avg.by.stage, aes(x=as.numeric(x_start_lab), y=UCL, xend=as.numeric(x_end_lab), yend=UCL), size=0.25, linetype = "dashed")
    r <- r + scale_x_continuous(breaks=dane$Probka, labels=dane$Probka)
    r <- r + theme_tufte(ticks=FALSE)
    r <- r + theme(axis.title=element_blank())
    return(r)  
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
  
  output$sampling_tree <- renderDiagrammeR({
    graf <- create_graph()
    df <- sampling_tree.data()
    #nodes <- read.csv2('c:/R/Shiny/ImR/nodes.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
    nodes <- add_nodes_from_df(graph=graf, df, set_type = NULL, select_cols = c('nody', 'nody.x'),
                               drop_cols = c('Row'), rename_attrs = NULL, id_col = NULL, type_col = NULL,
                               label_col = NULL)
    edges <- read.csv2('c:/R/Shiny/ImR/edges.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
    nody_df <- create_nodes(nodes,fontname = "Gill Sans MT", shape="circle", width = 1, label="")
    brzegi_df <- create_edges(from=edges$from, to=edges$to, size=1)
    graf <- create_graph(nodes_df= nody_df,edges_df = brzegi_df)
    render_graph(graf)
  })
  
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
}
