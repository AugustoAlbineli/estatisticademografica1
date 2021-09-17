library(shiny)
library(plotrix)
library(readxl)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinyEffects)


###################################################################
#                                       ##  ##                    #          
#                                      #  ##                      #
#  #######  #     #  #     #  #######  #######  #######  #######  #     
#  #        #     #  ###   #  #        #     #  #        #        #
#  #        #     #  #  #  #  #        #     #  #####    #######  #
#  ####     #     #  #   # #  #        #     #  #              #  #
#  #        #     #  #    ##  #        #     #  #              #  #
#  #        #######  #     #  #######  #######  #######  #######  #  
#                               #                                 # 
#                              #                                  #
###################################################################

basedemografica <- read_excel("basedemografica.xlsx")
somamasculino= sum(basedemografica$Masculino)
somafeminino= sum(basedemografica$Feminino)
populacao= somafeminino+somamasculino
freqmedia=sum(basedemografica$`Freq*Media`)
mediapop=(freqmedia/populacao)
mediana=30+4*((10693929/2)-4870146)/808291
idp= (2229504+994613)*100/7469812
mascmais65= sum(basedemografica[14:21,]$Masculino)
femmais65= sum(basedemografica[14:21,]$Feminino)
percmais65= ((mascmais65+femmais65)/populacao)*100
totalmais65= mascmais65+femmais65
mascmenos15= sum(basedemografica[1:3,]$Masculino)
femmenos15= sum(basedemografica[1:3,]$Feminino)
totalmenos15= mascmenos15+femmenos15
percmenos15= (totalmenos15/populacao)*100

tabelasundbarg <- data.frame(
  Estrutura_etária = c("0|-15","15|-50","50+","Total"),
  Progressivas_ou_jovens= c("40%","50%","10%","100%"),
  Estacionarias_ou_adultas= c("33%","50%","17%","100%"),
  Regressivas_ou_velhas= c("20%", "50%","30%","100%")
  )

masc1550= sum(basedemografica[4:10,]$Masculino)
fem1550= sum(basedemografica[4:10,]$Feminino)
total1550= masc1550+fem1550
perc1550= (total1550/populacao)*100

mascm50= sum(basedemografica[11:21,]$Masculino)
femm50= sum(basedemografica[11:21,]$Feminino)
totalm50= mascm50+femm50
percm50= (totalm50/populacao)*100

tabelaexsundbarg <- data.frame(
  Estrutura_etária = c("0|-15","15|-50","50+","Total"),
  Valores= c(totalmenos15,total1550,totalm50, (totalmenos15+total1550+totalm50)),
  Percentual= c(percmenos15,perc1550, percm50,(percmenos15+perc1550+percm50))
)
###################################################################
#                           #     #  #                            #                  
#                           #     #  #                            # 
#                           #     #  #                            #
#                           #     #  #                            #
#                           #     #  #                            #
#                           #######  #                            #
###################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Avaliação 1: Estatística Demográfica 2021/1- UFRGS",
                  titleWidth = 600,
                  dropdownMenu(type = "notifications", # para colocar os nomes de quem fez o app  
                               notificationItem(
                                 text = tags$div("2010",
                                                 style = "display: inline-block; vertical-align: middle;"),
                                 icon = icon("calendar-alt"),
                                 status = "success"
                               ), #notificationItem
                               headerText = "Dados do Censo Demográfico do RS de:"
                               
                  ) #dropdownMenu
                  ),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Introdução", tabName = "intro", icon = icon("info-circle")),
    menuItem("Exercício 1", tabName = "e1", icon = icon("chart-bar")),
    menuItem("Exercício 2", tabName = "e2", icon = icon("chart-bar")),
    menuItem("Exercício 3", tabName = "e3", icon = icon("calculator")),
    menuItem("Exercício 4", tabName = "e4", icon = icon("calculator")),
    menuItem("Exercício 5", tabName = "e5", icon = icon("calculator")),
    menuItem("Exercício 6", tabName = "e6", icon = icon("calculator"))
    )#sidebarMenu
  ), #dashboardSidebar
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              setZoom(id = "caixanomes"),
              setZoom(id = "caixae1"),
              setZoom(id = "caixae2"),
              setZoom(id = "caixae3"),
              setZoom(id = "caixae4"),
              setZoom(id = "caixae5"),
              setZoom(id = "caixae6"),
              
              
              fluidRow(
                valueBoxOutput(width = 12, outputId = "caixanomes"),
                valueBoxOutput(width = 12, outputId = "caixae1"),
                valueBoxOutput(width = 12, outputId = "caixae2"),
                valueBoxOutput(width = 12, outputId = "caixae3"),
                valueBoxOutput(width = 12, outputId = "caixae4"),
                valueBoxOutput(width = 12, outputId = "caixae5"),
                valueBoxOutput(width = 12, outputId = "caixae6")
                
              ),#fluidRow1
        
      ),#tabItem
      
      tabItem(tabName = "e1",
        
        setZoom(id = "caixa1e1"),
        
        fluidRow(
          valueBoxOutput(width = 12, outputId = "caixa1e1")
        ),#fluidRow1
        
        fluidRow(
          box(background = "navy", width=12,
              plotOutput(outputId = "piramide"))
          
        )#fluidRow2
        
      ),#tabItem
      
      tabItem(tabName = "e2",
              
              setZoom(id = "caixa1e2"),
             
              fluidRow(
                valueBoxOutput(width = 12, outputId = "caixa1e2")
              ),#fluidRow1
              
              fluidRow(
                box(background = "navy", width=12,
                    plotlyOutput(outputId = "razao"))
                
              )#fluidRow2
              
      ),#tabItem
      
      tabItem(tabName = "e3",
              
              setZoom(id = "caixa1e3"),
              setZoom(id = "caixafreqmed"),
              setZoom(id = "caixapopulacaototal"),
              setZoom(id = "caixamedia"),
              
              fluidRow(
                valueBoxOutput(width = 12, outputId = "caixa1e3")
              ),#fluidRow1
              
              fluidRow(
              box(
                title = "Explicação", width = 12, background = "navy",
                "Primeiramente, foi calculada a média (coluna Média do intervalo) e a frequência absoluta(soma da população masculina e feminina) de cada intervalo. Por linha/intervalo, esses valores foram multiplicados e colocados na coluna Freq*Media, e após isso, foi feita a soma dessa coluna. Por fim, essa soma foi dividida pela população total, resultado na média da população"
              ),
              
              ),#fluidRow2
              
              fluidRow(
                valueBoxOutput(width = 4, outputId = "caixafreqmed"),
                valueBoxOutput(width = 4, outputId = "caixapopulacaototal"),
                valueBoxOutput(width = 4, outputId = "caixamedia")
              ),#fluidRow3
              
              
              fluidRow(
              box(width=12,
                DT::dataTableOutput("tabela"))
              )#fluidRow4
      
                
             
      ),#tabItem
      
      tabItem(tabName = "e4",
              
              setZoom(id = "caixa1e4"),
              setZoom(id = "intermed"),
              setZoom(id = "lir"),
              setZoom(id = "h"),
              setZoom(id = "n"),
              setZoom(id = "fant"),
              setZoom(id = "fmd"),
              setZoom(id = "mediana"),
              
              fluidRow(
                valueBoxOutput(width = 12, outputId = "caixa1e4")
              ),#fluidRow1
              
              fluidRow(
                box(width=12,
                    uiOutput('formula'))
              ),#fluidRow2
              
              fluidRow(
                valueBoxOutput(width = 6, outputId = "intermed"),
                valueBoxOutput(width = 6, outputId = "lir"),
                valueBoxOutput(width = 6, outputId = "h"),
                valueBoxOutput(width = 6, outputId = "n"),
                valueBoxOutput(width = 12, outputId = "fant"),
                valueBoxOutput(width = 12, outputId = "fmd")
              ),#fluidRow3
              
              fluidRow(
                box(
                  title = "Substituindo os valores na fórmula, chegamos na resposta:", width = 6, background = "navy"),
                valueBoxOutput(width = 6, outputId = "mediana")
              )#fluidRow4
              
              
              
      ),#tabItem
      
      tabItem(tabName = "e5",
              
              setZoom(id = "caixa1e5"),
              setZoom(id = "mascmais65"),
              setZoom(id = "femmais65"),
              setZoom(id = "poptotal"),
              setZoom(id = "percmais65"),
              setZoom(id = "totalmais65"),
              setZoom(id = "percmais652"),
              setZoom(id = "totalmenos15"),
              setZoom(id = "percmenos15"),
              
              fluidRow(
                valueBoxOutput(width = 12, outputId = "caixa1e5")
              ),#fluidRow1
              
              fluidPage(
                h2("1°- 65 anos ou mais"),
                box(
                h3("+7%:Velha"),
                h3("4 à 7%:Transição"),
                h3("-4%:Jovem")),
                
                box(
                  fluidRow(
                    valueBoxOutput(width = 6, outputId = "mascmais65"),
                    valueBoxOutput(width = 6, outputId = "femmais65"),
                    valueBoxOutput(width = 6, outputId = "poptotal"),
                    valueBoxOutput(width = 6, outputId = "percmais65")
                  )#fluidRow2
                ),
                box(width=12,
                  h3("Como o percentual é maior que 7%, a população é classificada como velha.")
                )
                
              ),#fluidPage1
              
              fluidPage(
                h2("2°- Relativamente jovem ou relativamente velha"),
                box(
                  h3("Relativamente jovem: 40 - 45% < 15 anos e 3 - 4% 65 anos e mais"),
                  h3("Relativamente velha: +/- 25% < 15 anos e +/- 10% 65 anos e mais")),
                
                box(
                  fluidRow(
                    valueBoxOutput(width = 6, outputId = "totalmais65"),
                    valueBoxOutput(width = 6, outputId = "percmais652"),
                    valueBoxOutput(width = 6, outputId = "totalmenos15"),
                    valueBoxOutput(width = 6, outputId = "percmenos15")
                  )#fluidRow2
                ),
                box(width=12,
                    h3("O percentual da população acima de 65 anos se aproxima de 10% e o da população abaixo de 15 anos se aproxima de 25%. Logo, a população é classificada como relativamente velha.")
                )
                
              ),#fluidPage2
              
              fluidPage(
                h2("3°- Sundbarg"),
                  fluidRow(
                    box(width=12,
                        DT::dataTableOutput("tabelasundbarg"))
                  ), #fluidRow1
                
                box(width=12,
                    h3("Segue a tabela com os valores e percentuais calculados de cada faixa etária:")
                ),
                
                fluidRow(
                  box(width=12,
                      DT::dataTableOutput("tabelaexsundbarg"))
                ), #fluidRow2
                
                box(width=12,
                    h3("Comparando as duas tabelas, os valores percentuais das faixas etárias calculados se aproximam dos valores da coluna regressiva. Logo,a população pode ser classificada como regressiva, ou seja, as taxas de
mortalidade e natalidade há muito tempo são baixas.")
                )
                
              ),#fluidPage3
              
              fluidPage(
                h2("4°- Whipple"),
                box(
                  h3("15 - 50 anos"),
                  h3("Normal 50%"),
                  h3("Acessiva > 50%"),
                  h3("Recessiva < 50%")),
                
                box(width=12,
                    h3("Utilizando a mesma tabela da classificação Sundbarg, vamos analisar a linha da faixa etária 15 - 50 anos")
                ),
                
                fluidRow(
                  box(width=12,
                      DT::dataTableOutput("tabela1exsundbarg"))
                ), #fluidRow2
                
                box(width=12,
                    h3("Temos que o percentual é de 53.55425%, que é maior que 50, logo, a população é acessiva.")
                )
                
              )#fluidPage4
            
              
      ),#tabItem
      
      tabItem(tabName = "e6",
              
              setZoom(id = "caixa1e6"),
              setZoom(id = "z14"),
              setZoom(id = "s5m"),
              setZoom(id = "qs5"),
              setZoom(id = "idp"),
              
              fluidRow(
                valueBoxOutput(width = 12, outputId = "caixa1e6")
              ),#fluidRow1
              
              fluidRow(
                box(title="Fórmula do Índice de Dependência Potencial", width=12,
                    "IDP=(N°de pessoas 0-14 + N° de pessoas 65+)*100/(N° de pessoas 15-64)")
              ),#fluidRow2
              
              fluidRow(
                valueBoxOutput(width = 4, outputId = "z14"),
                valueBoxOutput(width = 4, outputId = "s5m"),
                valueBoxOutput(width = 4, outputId = "qs5")
              ),#fluidRow3
              
              fluidRow(
                box(
                  title = "Substituindo os valores na fórmula, chegamos na resposta:", width = 6, background = "navy"),
                valueBoxOutput(width = 6, outputId = "idp")
              )#fluidRow4
              
              
      )#tabItem
      
      
    )#tabItems
    
  )#dashboardBody
  
)#dashboardPage


###################################################################
#      #######  #######  #######  #     #  #######  #######       #  
#      #        #        #     #  #     #  #        #     #       #
#      #######  #####    #######  #     #  #####    #######       #
#            #  #        ###       #   #   #        ###           #
#            #  #        #  ##      # #    #        #  ##         #
#      #######  #######  #    ##     #     #######  #    ##       #
###################################################################

server <- function(input, output) {

#####################################################
#                  INTRODUÇÃO                       #
#####################################################
  
output$caixanomes <- renderValueBox({
    infoBox("Alunos:", "Augusto Albineli de Souza, Rodolfo Arnaldo Montecinos de Almeida, Tiago Luigi Guadagnin Radin",color="navy", icon=icon("user-graduate"))
  })


output$caixae1 <- renderValueBox({
  infoBox("Exercício 1:","Elaborar uma pirâmide etária com dados agrupados por idade (grupos de idade: 0 a 4 anos ; 5 a 9 anos , etc.)",color="navy", icon=icon("chart-bar"))
})


output$caixae2 <- renderValueBox({
  infoBox("Exercício 2:","Calcular a razão de sexo por idade para o RS e elaborar um gráfico;",color="navy", icon=icon("chart-bar"))
})


output$caixae3 <- renderValueBox({
  infoBox("Exercício 3:","Calcular a idade média da população do RS (diga qual suposição usou);",color="navy", icon=icon("calculator"))
})


output$caixae4 <- renderValueBox({
  infoBox("Exercício 4:","Calcular a idade mediana da população do RS;",color="navy", icon=icon("calculator"))
})


output$caixae5 <- renderValueBox({
  infoBox("Exercício 5:","Classificar a população total do RS segundo a idade usando todos critérios conhecidos;",color="navy", icon=icon("calculator"))
})

output$caixae6 <- renderValueBox({
  infoBox("Exercício 6:","Calcular o índice de dependência potencial da população total.",color="navy", icon=icon("calculator"))
})


#####################################################
#                  Exercício 1                      #
#####################################################

output$caixa1e1 <- renderValueBox({
  infoBox("Exercício 1:","Elaborar uma pirâmide etária com dados agrupados por idade (grupos de idade: 0 a 4 anos ; 5 a 9 anos , etc.)",color="navy", icon=icon("chart-bar"))
})

output$piramide <- renderPlot({
  xy.pop<-c(3.1,3.5,4.1,4.1,4.1,4.2,3.7,3.4,3.5,3.5,3.1,2.6,2.0,1.5,1.1,0.7,0.4,0.2,0.1,0.0,0.0)
  xx.pop<-c(3.0,3.3,4.0,4.1,4.1,4.2,3.8,3.5,3.7,3.7,3.4,2.9,2.3,1.8,1.4,1.1,0.7,0.4,0.1,0.0,0.0)
  agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
               "75-79","80-84","85-89","90-94","95-99","100+")
  mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
  fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
  par(mar=pyramid.plot(xy.pop,xx.pop,labels=agelabels,
                       main="Pirâmide etária do Rio Grande do Sul(2010)",lxcol=mcol,rxcol=fcol,
                       gap=0.5,show.values=TRUE))
}) 

#####################################################
#                  Exercício 2                      #
#####################################################

output$caixa1e2 <- renderValueBox({
  infoBox("Exercício 2:","Calcular a razão de sexo por idade para o RS e elaborar um gráfico;",color="navy", icon=icon("chart-bar"))
})

output$razao <- renderPlotly({
  graficorazao<- ggplot(basedemografica, aes(x=`Faixa etária`, y=`Razão`,fill=`Razão`))+
    geom_col()+
    labs(x="Faixa etária", y="Razão sexo/idade")+
    theme(axis.text.x = element_text(angle = 45, size = 8))
    ggplotly(graficorazao)
  
})


#####################################################
#                  Exercício 3                      #
#####################################################

output$caixa1e3 <- renderValueBox({
  infoBox("Exercício 3:","Calcular a idade média da população do RS (diga qual suposição usou);",color="navy", icon=icon("calculator"))
})

output$tabela = DT::renderDataTable({
  basedemografica
})

output$caixafreqmed <- renderValueBox({
  valueBox(freqmedia, "Soma Frequência x Média",color="navy"
  )
})

output$caixapopulacaototal <- renderValueBox({
  valueBox(populacao, "População total",color="navy"
  )
})

output$caixamedia <- renderValueBox({
  valueBox(round(mediapop,5), "Média da população do RS",color="navy"
  )
})


#####################################################
#                  Exercício 4                      #
#####################################################

output$caixa1e4 <- renderValueBox({
  infoBox("Exercício 4:","Calcular a idade mediana da população do RS;",color="navy", icon=icon("calculator"))
})

output$formula <- renderUI({
  withMathJax(helpText('Fórmula da mediana:  $$Md= LIR_{md}+h((n/2-F_{ant})/f_{md})$$'))
})

output$intermed <- renderValueBox({
  valueBox("30-34", "Intervalo da mediana",color="navy" )
})

output$lir<- renderValueBox({
  valueBox(30, "LIRmd= Limite inferior real do intervalo que conte a mediana",color="navy")
})

output$h<- renderValueBox({
  valueBox(4, "h= Amplitude dos intervalos",color="navy")
})

output$n<- renderValueBox({
  valueBox(populacao, "n= Tamanho da amostra",color="navy")
})

output$fant<- renderValueBox({
  valueBox(5678437, "Fant= Freqüência absoluta acumulada no intervalo anterior que contém a mediana",color="navy")
})

output$fmd<- renderValueBox({
  valueBox(808291, "Fmd= freqüência absoluta simples no intervalo que contém a mediana.",color="navy")
})

output$mediana<- renderValueBox({
  valueBox(round(mediana,5), "Mediana da população do RS.",color="navy")
})


#####################################################
#                  Exercício 5                      #
#####################################################

output$caixa1e5 <- renderValueBox({
  infoBox("Exercício 5:","Classificar a população total do RS segundo a idade usando todos critérios conhecidos;",color="navy", icon=icon("calculator"))
})

output$mascmais65<- renderValueBox({
  valueBox(mascmais65, "População masculina com 65 anos ou mais.",color="navy")
})

output$femmais65<- renderValueBox({
  valueBox(femmais65, "População feminina com 65 anos ou mais.",color="navy")
})

output$poptotal<- renderValueBox({
  valueBox(populacao, "População total com 65 anos ou mais.",color="navy")
})

output$percmais65<- renderValueBox({
  valueBox(round(percmais65,5), "Percentual da população 65 anos ou mais.",color="navy")
})

output$totalmais65<- renderValueBox({
  valueBox(totalmais65, "População total com 65 anos ou mais.",color="navy")
})

output$percmais652<- renderValueBox({
  valueBox(round(percmais65,5), "Percentual da população 65 anos ou mais.",color="navy")
})

output$totalmenos15<- renderValueBox({
  valueBox(totalmenos15, "População total com  menos de 15 anos.",color="navy")
})

output$percmenos15<- renderValueBox({
  valueBox(round(percmenos15,5), "Percentual da população  com  menos de 15 anos.",color="navy")
})

output$tabelasundbarg = DT::renderDataTable({
 tabelasundbarg
})

output$tabelaexsundbarg = DT::renderDataTable({
  tabelaexsundbarg
})

output$tabela1exsundbarg = DT::renderDataTable({
  tabelaexsundbarg
})

#####################################################
#                  Exercício 6                      #
#####################################################

output$caixa1e6 <- renderValueBox({
  infoBox("Exercício 6:","Calcular o índice de dependência potencial da população total.",color="navy", icon=icon("calculator"))
})

output$z14<- renderValueBox({
  valueBox(2229504, "Nº de pessoas 0 - 14",color="navy")
})

output$s5m<- renderValueBox({
  valueBox(994613, "Nº de pessoas 65+",color="navy")
})

output$qs5<- renderValueBox({
  valueBox(7469812, "Nº de pessoas 15 - 64 anos",color="navy")
})

output$idp<- renderValueBox({
  valueBox(round(idp,5), "IDP da população do RS.",color="navy")
})

}#server

shinyApp(ui = ui, server = server)
