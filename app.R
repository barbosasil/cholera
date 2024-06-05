library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(plotly)
library(gridExtra)
library(lubridate)
library(reshape2)
library(grid)
library(gridExtra)
library(gridtext)
library(DT)
library(png)
library(ggpubr)
library(leaflet)
library(rgdal)
library(raster)
library(rworldmap)
library(sparkline)
library(htmltools)
library(purrr)
library(trendbreaker)
library(zoo)
library(imputeTS)
library(dplyr)
library(stringr)
library(shiny.i18n)
library(lubridate)

# File with translations
i18n <- Translator$new(translation_json_path = "/home/urstudio/cholera/translation.json")

# Change this to en
i18n$set_translation_language("en")

# TODO: Wasn't able to use i18n on choices
choicesEN = c("All" = "All",
              "North America"= "North America",
              "Central America" = "Central America",
              "South America" = "South America",
              "Caribbean and Atlantic Ocean Islands" = "Caribbean and Atlantic Ocean Islands")

choicesES = c("Todas"="All",
              "América del Norte"="North America",
              "América Central"="Central America",
              "América del Sur"="South America",
              "Caribe y las Islas del Océano Atlántico"="Caribbean and Atlantic Ocean Islands")

choicesPT = c("Todas"="All",
              "América do Norte"="North America",
              "América Central"="Central America",
              "América do Sul"="South America",
              "Caribe e Ilhas do Oceano Atlântico"="Caribbean and Atlantic Ocean Islands")  

choicesFR = c("Toute"="All",
              "Amérique du Nord"="North America",
              "Amérique Centrale"="Central America",
              "Amérique du Sud"="South America",
              "Caraïbes et Îles de l'Océan Atlantique"="Caribbean and Atlantic Ocean Islands")  

countriesColumnsList <- list()
countriesColumnsList[["en"]] <- "Country.Territory"
countriesColumnsList[["es"]] <- "country2"
countriesColumnsList[["pt"]] <- "country3"
countriesColumnsList[["fr"]] <- "country4"

countriesColumnsList2 <- countriesColumnsList
countriesColumnsList2[["en"]] <- "country"


options(spinner.color = "grey", spinner.color.background = "#ffffff", spinner.size = 2, shiny.reactlog=TRUE)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(disable = TRUE),#title = "Cholera outbreak"),
                    dashboardSidebar(collapsed = TRUE,disable=TRUE),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "monkeypox.css")
                      ),
                      fluidPage(
                        column(width=5,plotOutput("logo",height = '67', width = '600')),
                        column(width = 4),
                        column(width=3,align="center",
                          h5(radioButtons("idiom", label = NULL, choices = c("English" = "en","Spanish"="es","Portuguese"="pt","French"="fr"),inline = T),style="color:white"),
                          tags$hr(),
                          h5(radioButtons("idiom2",i18n$t("Download presentation"),choices=c("EN","SP","PT","FR"),inline = T),style="color:white"),
                          conditionalPanel(
                            condition = "input.idiom2=='EN'",
                            column(width=12,downloadButton("report1","Presentation (EN)",class = "butt"))
                          ),
                          conditionalPanel(
                            condition = "input.idiom2=='SP'",
                            column(width=12,downloadButton("report2","Presentación (SP)",class = "butt"))
                          ),
                          conditionalPanel(
                            condition = "input.idiom2=='PT'",
                            column(width=12,downloadButton("report3","Presentation (PT)",class = "butt"))
                          ),
                          conditionalPanel(
                            condition = "input.idiom2=='FR'",
                            column(width=12,downloadButton("report4","Presentation (FR)",class = "butt"))
                          )
                        ),
                        style = "background-color:#0099DA;"
                      ),
                      fluidRow(
                        align="left",h1(htmlOutput("title")),
                      ),
                      hr(),
                      fluidRow(
                        h4(column(width = 3,selectInput("country","Country",choices=c("All","Haiti"="HTI","Dominican Republic"="DOM"),selected = 'All'))),
                        conditionalPanel(
                          condition = 'input.country=="HTI"',
                          h4(column(width = 3,selectInput("adm1","Administrative level 1",choices=c("All","Artibonite","Centre","Grand Anse","Nippes","Nord","Nord-Est","Nord-Ouest","Ouest","Sud","Sud-Est","Undetermined"),selected = 'All'))) 
                        ),
                        conditionalPanel(
                          condition = 'input.country=="DOM"',
                          h4(column(width = 3,selectInput("adm2","Administrative level 1",choices=c("All","Azua","Baoruco","Barahona","Dajabon","Distrito Nacional",
                                                                                                    "Duarte","El Seibo","Elias Pina","Espaillat","Hato Mayor",
                                                                                                    "Hermanas Mirabal","Independencia","La Altagracia","La Romana",
                                                                                                    "La Vega","Maria Trinidad Sanchez","Monsenor Nouel","Monte Cristi",
                                                                                                    "Monte Plata","Pedernales","Peravia","Puerto Plata","Samana","San Cristobal",
                                                                                                    "San Jose de Ocoa","San Juan","San Pedro de Macoris","Sanchez Ramirez",
                                                                                                    "Santiago","Santiago Rodriguez","Santo Domingo","Valverde","Undetermined"),selected = 'All'))) 
                        ),
                        h4(column(width = 3,dateInput("date1", i18n$t("Date as of"), value = NULL)))
                      ),
                      fluidRow(
                        h2(htmlOutput("subtitle")),
                        valueBoxOutput("top1", width = 4),
                        valueBoxOutput("top2", width = 4),
                        valueBoxOutput("top3", width = 4)
                      ),
                      hr(),
                      fluidRow(
                        box(width = 6,
                            tabsetPanel(
                              tabPanel(title = htmlOutput("tab1"),
                                radioButtons("mode",label = NULL,choices=c("Day","Epidemiological week"),inline = T),
                                h4(htmlOutput("title_tab1", align = "center")),
                                withSpinner(plotlyOutput("plot1", width = 'auto', height = 600), type = 2)
                              ),
                              tabPanel(title = htmlOutput("tab2"),
                                radioButtons("mode2",label = "Classification",choices=c("Confirmed","Suspected"),inline = T),
                                h4(htmlOutput("title_tab2", align = "center")),
                                withSpinner(plotlyOutput("plot2", width = 'auto', height = 600), type = 2)
                              )
                            )
                        ),
                        box(width = 6,
                            tabsetPanel(
                              tabPanel(title = htmlOutput("tab3"),
                                h4(htmlOutput("title_tab3", align = "center")),
                                withSpinner(div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("table1",height=610)), type = 2)
                              ),
                              tabPanel(title = htmlOutput("tab4"),
                                h4(htmlOutput("title_tab4", align = "center")),
                                column(width = 12,
                                  radioButtons("classif1", i18n$t("Classification"), choices = c(i18n$t("Suspected cases"),i18n$t("Confirmed cases")), inline = T)
                                ),
                                leafletOutput("map1", height = 540)
                              )
                            )
                            
                        )
                      ),
                      h4(htmlOutput("legend")),
                      hr(),
                      h3(textOutput("title_disc")),
                      h5(textOutput("disclaimer")),
                      fluidRow(
                        h2(),style = "background-color:#0099DA;"
                      )
                    )
)

date_format_lang <- function(originaldate, language) {
  
  switch(
    language,
    "en" = Sys.setlocale("LC_TIME", "en_US.UTF-8"),
    "es" = Sys.setlocale("LC_TIME", "es_US.UTF8"),
    "pt" = Sys.setlocale("LC_TIME", "pt_BR.UTF8"),
    "fr" = Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
  )
  
  format(originaldate, "%d %B %Y")
  
}


server <- function(input, output, session) {
  

  observeEvent(input$idiom, {
    i18n$set_translation_language(input$idiom)
    chosen <- switch(
      input$idiom,
      "en" = choicesEN,
      "es" = choicesES,
      "pt" = choicesPT,
      "fr" = choicesFR
    )
    updateDateInput(
      session,
      "date1",
      label = i18n$t("Date as of")
    )
    updateSelectInput(
      session,
      "country",
      label=i18n$t("Country"),
      choices=c(i18n$t("All"),"Haiti"="HTI","Dominican Republic"="DOM"),
      selected = i18n$t("All")
    )
    updateSelectInput(
      session,
      "adm1",
      label=i18n$t("Administrative level 1"),
      choices=c(i18n$t("All"),"Artibonite","Centre","Grand Anse","Nippes","Nord","Nord-Est","Nord-Ouest","Ouest","Sud","Sud-Est","Undetermined"),
      selected = i18n$t('All')
    )
    updateSelectInput(
      session,
      "adm2",
      label=i18n$t("Administrative level 1"),
      choices=c(i18n$t("All"),"Azua","Baoruco","Barahona","Dajabon","Distrito Nacional",
                "Duarte","El Seibo","Elias Pina","Espaillat","Hato Mayor",
                "Hermanas Mirabal","Independencia","La Altagracia","La Romana",
                "La Vega","Maria Trinidad Sanchez","Monsenor Nouel","Monte Cristi",
                "Monte Plata","Pedernales","Peravia","Puerto Plata","Samana","San Cristobal",
                "San Jose de Ocoa","San Juan","San Pedro de Macoris","Sanchez Ramirez",
                "Santiago","Santiago Rodriguez","Santo Domingo","Valverde","Undetermined"),
      selected = i18n$t('All')
    )
    updateRadioButtons(
      session,
      "classif1",
      label=i18n$t("Classification"),
      choices = c(i18n$t("Suspected cases"),i18n$t("Confirmed cases")),
      inline = T
    )
    updateRadioButtons(
      session,
      "mode",
      label=NULL,
      choices=c(i18n$t("Day"),i18n$t("Epidemiological week")),
      inline = T
    )
    updateRadioButtons(
      session,
      "mode2",
      label = i18n$t("Classification"),
      choices=c(i18n$t("Confirmed"),i18n$t("Suspected")),
      inline = T
    )
  })
  
  date=data.frame(dateweek=seq.Date(from=as.Date("2019-12-01"),to=Sys.Date(),by="day"))
  date$ew=paste0(epiweek(date$dateweek),".",epiyear(date$dateweek))
  date$semana=weekdays(date$dateweek)
  date=date[which(date$semana=="Monday" | date$semana=="segunda" | date$semana=="lunes" | date$semana=="lundi"),c("dateweek","ew")]
  
  db = read.csv("/home/urstudio/cholera/database.csv",encoding = "UTF-8")
  db$date=as.Date(db$date)
  
  db1 = read.csv("/home/urstudio/cholera/database_epicurve.csv")
  db1$date=as.Date(db1$Date)
  db1=db1[with(db1,order(date,decreasing = F)),]
  db1$cum=cumsum(db1$Suspected)
  db1$ew=paste0(epiweek(db1$date),".",epiyear(db1$date))
  db1=merge(db1,date,by="ew",all.x=T)
  
  
  db2 = read.csv("/home/urstudio/cholera/database_age.csv")
  db2$date=as.Date(db2$date)
  
  db3 = read.csv("/home/urstudio/cholera/database_age_conf.csv")
  db3$date=as.Date(db3$date)
  
  # database <- reactive({
  #   data = read.csv("/home/urstudio/cholera/database.csv",encoding = "UTF-8")
  #   data$date=as.Date(data$date)
  # })
  
  output$logo <- renderPlot({
    x=switch(input$idiom,
      "en" = "/home/urstudio/logo/paho_white_en.png",
      "es" = "/home/urstudio/logo/paho_white_es.png",
      "pt" = "/home/urstudio/logo/paho_white_pt.png",
      "fr" = "/home/urstudio/logo/paho_white_fr.png"
    )
    ggplot() +
      background_image(readPNG(x)) +
      theme(panel.background = element_rect(fill = "#0099DA", colour = "#0099DA"),
            plot.margin = margin(0, 0, 0, 0, "cm"))
  })
  
  output$tab1 <- renderUI({
    i18n$set_translation_language(input$idiom)
    HTML(i18n$t("Epicurve"))
  })
  
  output$tab2 <- renderUI({
    i18n$set_translation_language(input$idiom)
    HTML(i18n$t("Demographic Characteristics"))
  })
  
  output$tab3 <- renderUI({
    i18n$set_translation_language(input$idiom)
    HTML(i18n$t("Table"))
  })
  
  output$tab4 <- renderUI({
    i18n$set_translation_language(input$idiom)
    HTML(i18n$t("Map"))
  })
  
  output$title <- renderUI({
    i18n$set_translation_language(input$idiom)
    htmlText = paste("<div style='margin-left: 0.05px'!important;>",i18n$t("Cholera resurgence in Hispaniola"),"</div>")
    HTML(htmlText)
  })
  
  output$subtitle <- renderUI({
    i18n$set_translation_language(input$idiom)
    HTML("<div style='margin-left: 20px !important;'>",i18n$t("Official sources"),"<sup>1,2</sup></div>")
  })
  
  output$title_disc <- renderText({
    i18n$set_translation_language(input$idiom)
    paste0(i18n$t("Disclaimer"))
  })
  
  output$title_tab1 <- renderUI({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db1$date, na.rm = T)) max(db1$date, na.rm = T) else input$date1
    
    htmlText = paste0("<div class='dash-title'>",
                      i18n$t("Suspected cholera cases by date."),
                      " <br/> ",
                      "Haiti, ",i18n$t("As of"),
                      " ",
                      date_format_lang(displayDate, input$idiom),
                      "<sup>3,4</sup>"
    )
    HTML(htmlText)
  })
  
  output$title_tab2 <- renderUI({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db2$date, na.rm = T)) max(db2$date, na.rm = T) else input$date1
    
    title=switch (input$mode2,
      "Confirmed" = i18n$t("Confirmed cholera cases by age group and sex."),
      "Suspected" = i18n$t("Suspected cholera cases by age group and sex.")
    )
    
    htmlText = paste0("<div class='dash-title'>",
                      title,
                      " <br/> ",
                      "Haiti, ",i18n$t("As of"),
                      " ",
                      date_format_lang(displayDate, input$idiom),
                      "<sup>3,4</sup>"
    )
    HTML(htmlText)
  })
  
  output$title_tab3 <- renderUI({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1
    
    htmlText = paste0("<div class='dash-title'>",
                      i18n$t("Confirmed and suspected cholera cases and deaths by administrative level 1."),
                      " <br/> ",
                      i18n$t("As of"),
                      " ",
                      date_format_lang(displayDate, input$idiom),
                      "<sup>4</sup>"
    )
    HTML(htmlText)
  })
  
  output$title_tab4 <- renderUI({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1
    
    htmlText = paste0("<div class='dash-title'>",
                      ifelse(input$classif1==i18n$t("Suspected cases"),i18n$t("Suspected cholera cases and deaths by administrative level 1."),
                             i18n$t("Confirmed cholera cases and deaths by administrative level 1.")),
                      " <br/> ",
                      i18n$t("As of"),
                      " ",
                      date_format_lang(displayDate, input$idiom),
                      "<sup>3,4</sup>"
    )
    HTML(htmlText)
  })
  
  output$legend <- renderUI({
    i18n$set_translation_language(input$idiom)
    ctimeUpdate = date_format_lang(file.info("/home/urstudio/cholera/database.csv")$ctime - 3600, input$idiom)
    switch(
      input$idiom,
      "en" = HTML("   ", "<strong>Notes:</strong> 1. Haiti Ministère de la Santé Publique et de la Population. The data is subject to change.",
                  "2. These cases include those in the Port-au-Prince prison. 
                  3. This graph/map includes only cases from communities related to the outbreak in Haiti. Cases among inmates at the prison in Port-au-Prince are not included.
                  4. The data on the dashboard is refreshed once a day between 09:30-10:00 GTM-5.",
                  "<br>Data updated as of", ctimeUpdate),
      "es" = HTML("   ", "<strong>Notas:</strong> 1. Haiti Ministère de la Santé Publique et de la Population. Los datos están sujetos a cambios.",
                  "2. Estos números de casos incluyen los registrados en el establecimiento penitenciario de Puerto Príncipe. 
                  3. En esta figura/mapa se representan exclusivamente los casos relacionados al brote comunitario registrado en Haití. 
                  No se incluye los casos registrados en el establecimiento penitenciario de Puerto Príncipe.
                  4. Los datos en este tablero se actualizan una vez por día entre las 09:30 y las 10:00 GTM-5.",
                  "<br>Datos actualizados hasta ", ctimeUpdate),
      "pt" = HTML("   ", "<strong>Notas:</strong> 1. Haiti Ministère de la Santé Publique et de la Population. Os dados estão sujeitos a mudanças.",
                  "2. Estes números de casos incluem os registrados nos estabelecimentos penitenciários de Porto Príncipe.
                  3. Nesta figura/mapa são representados exclusivamente os casos relacionados ao surto comunitário registrado no Haiti. 
                  4. Os dados do painel são atualizados uma vez por dia entre 9h30-10h00 GTM-5.",
                  "<br>Dados atualizados até ", ctimeUpdate),
      "fr" = HTML("   ", "<strong>Notes:</strong> 1. Haiti Ministère de la Santé Publique et de la Population. Les données sont susceptibles d'être modifiées. 
                  2. Ces numéros de cas incluent ceux enregistrés à la prison de Port-au-Prince. 
                  3. Cette figure/carte représente exclusivement les cas liés à l'épidémie communautaire enregistrés en Haïti. Les cas enregistrés à la prison de Port-au-Prince ne sont pas inclus. 
                  4. Les données de ce tableau de bord sont mises à jour une fois par jour entre 09h30 et 10h00 GTM-5.",
                  "<br>Données mises à jour au ", ctimeUpdate)
    )
    
  })
  
  output$disclaimer <- renderText({
    i18n$set_translation_language(input$idiom)
    switch(
      input$idiom,
      "en" = HTML("The PAHO/WHO Cholera Outbreak 2022 Dashboard is a platform which aims to share information about cholera in the Americas.
              The PAHO/WHO Cholera Outbreak 2022 Dashboard is for general information only. It is subject to change without notice. While every reasonable effort has been made to make the information on the PAHO/WHO Cholera Outbreak 2022 Dashboard 
              as timely and accurate as possible, WHO makes no claims, promises or guarantees about the effectiveness, completeness and accuracy of the contents of the PAHO/WHO Cholera Outbreak 2022 Dashboard, and expressly disclaims any liability 
              for damages as a result of the use and/or application of the PAHO/WHO Cholera Outbreak 2022 Dashboard, errors and/or omissions in the content. 
            The responsibility for the interpretation and use of the content lies with the reader. PAHO/WHO reserves the right to make updates and changes to posted content without notice and accepts no liability for any errors or omissions in this regard. 
            PAHO/WHO assumes no responsibility or liability for any consequence resulting directly or indirectly from any action or inaction readers take based on or made in reliance on the information and material available on the PAHO/WHO Cholera Outbreak 2022 Dashboard. 
            While every reasonable effort has been made to use appropriate language and pictures on the PAHO/WHO Cholera Outbreak 2022 Dashboard, PAHO/WHO expressly disclaims any responsibility for inadvertent 
            offensive or insensitive, perceived or actual, language or pictures. PAHO/WHO will take no responsibility for or be liable for the PAHO/WHO Cholera Outbreak 2022 Dashboard being temporarily unavailable in the event of technical or other issues. 
            The designations employed and the presentation of content on the PAHO/WHO Cholera Outbreak 2022 Dashboard, including names of the events, maps and other illustrative materials, do not imply the expression of any opinion whatsoever on the part 
            of PAHO/WHO concerning the legal status of any country, territory, city or area, or of its authorities, or concerning the delineation of frontiers and borders. Grey areas on maps represent approximate border lines for which there may not yet be full agreement."),
      "es" = HTML("El Tablero de brote de cólera de la OPS/OMS (en adelante, Tablero) es una plataforma cuyo propósito es compartir 
                  información sobre cólera en las Américas. El Tablero es solo para información general y está sujeto a cambios sin 
                  previo aviso. Si bien se han hecho todos los esfuerzos razonables para que la información del Tablero sea lo más 
                  oportuna y precisa posible, la OPS/OMS no afirma, promete ni garantiza la efectividad, integridad y precisión del 
                  contenido del Tablero de la OPS/OMS, y renuncia expresamente a cualquier responsabilidad por daños y perjuicios 
                  como resultado del uso del Tablero, los errores u omisiones en el contenido. La responsabilidad de la 
                  interpretación y uso del contenido recae en el lector. La OPS/OMS se reserva el derecho de realizar actualizaciones 
                  y cambios al contenido publicado sin previo aviso y no acepta responsabilidad por cualquier error u omisión al 
                  respecto. La OPS/OMS no asume ninguna responsabilidad por las consecuencias que resulten directa o indirectamente 
                  de cualquier acción o inacción que los lectores tomen con base en la información y el material disponible en el 
                  Tablero. Si bien se han realizado todos los esfuerzos razonables para utilizar el lenguaje y las imágenes 
                  apropiados en el Tablero, la OPS/OMS renuncia expresamente a cualquier responsabilidad por lenguaje o imágenes 
                  inadvertidamente ofensivos o insensibles, percibidos o reales. La OPS/OMS no se responsabilizará ni será 
                  responsable de que el Tablero no esté disponible temporalmente en caso de problemas técnicos o de otro tipo. 
                  Las denominaciones empleadas y la presentación del contenido del Tablero, incluidos los nombres de los eventos, 
                  mapas y otros materiales ilustrativos, no implican la expresión de opinión alguna por parte de la OPS/OMS sobre 
                  el estatus legal de cualquier país, territorio, ciudad o área, o de sus autoridades, o sobre la delimitación 
                  geográfica y de fronteras. Las áreas grises en los mapas representan líneas de frontera aproximadas para las 
                  cuales es posible que aún no haya un acuerdo total."),
      "pt" = HTML("O Painel da OPAS/OMS sobre surto de cólera na Região das Américas é uma plataforma que visa compartilhar 
                  informações sobre a cólera nas Américas. O Painel da OPAS/OMS sobre surto de cólera na Região das Américas 
                  é apenas para informação geral. Ele está sujeito a alteração sem aviso prévio. Embora todos os esforços razoáveis 
                  tenham sido feitos para tornar as informações do Painel da OPAS/OMS sobre surto de cólera na Região das Américas 
                  tão oportunas e precisas quanto possível, a OPAS/OMS não faz reivindicações, promessas ou garantias sobre a 
                  efetividade, integridade e precisão do conteúdo do Painel da OPAS/OMS sobre surto de cólera na Região das Américas, 
                  e se isenta expressamente de qualquer responsabilidade por danos resultantes do uso e/ou aplicação do Painel 
                  da OPAS/OMS sobre surto de cólera na Região das Américas, erros e/ou omissões no conteúdo. A responsabilidade 
                  pela interpretação e uso do conteúdo é do leitor. A OPAS/OMS reserva-se o direito de fazer atualizações e 
                  alterações no conteúdo postado sem aviso prévio e não se responsabiliza por quaisquer erros ou omissões a 
                  esse respeito. A OPAS/OMS não assume nenhuma responsabilidade ou obrigação por qualquer consequência resultante 
                  direta ou indiretamente de qualquer ação ou inação que os leitores tomem com base ou em conformidade com as 
                  informações e materiais disponíveis no Painel da OPAS/OMS sobre surto de cólera na Região das Américas. 
                  Embora todos os esforços razoáveis tenham sido feitos para usar linguagem e imagens apropriadas no Painel da 
                  OPAS/OMS sobre surto de cólera na Região das Américas, a OPAS/OMS se isenta expressamente de qualquer 
                  responsabilidade por linguagem ou imagens inadvertidamente ofensivas ou insensíveis, percebidas ou reais. 
                  A OPAS/OMS não se responsabilizará ou será responsabilizada pelo fato de o Painel da OPAS/OMS sobre surto de 
                  cólera na Região das Américas ficar temporariamente indisponível no caso de problemas técnicos ou outros. 
                  As designações empregadas e a apresentação do conteúdo do Painel OPAS/OMS sobre surto de cólera na Região das 
                  Américas, incluindo nomes dos eventos, mapas e outros materiais ilustrativos, não implicam a expressão de 
                  qualquer opinião por parte da OPAS/OMS sobre a situação jurídica de qualquer país, território, cidade ou área, 
                  ou de suas autoridades, ou sobre a delimitação de limites e fronteiras. As áreas cinzentas nos mapas 
                  representam linhas de fronteira aproximadas para as quais pode ainda não haver um acordo total."),
      "fr" = HTML("Le Tableau de Bord de l'OPS/OMS sur le Choléra dans la Région des Amériques est une plateforme qui vise 
                  à partager des informations sur le choléra dans les Amériques. Le Tableau de Bord de l'OPS/OMS sur le 
                  Choléra dans la Région des Amériques est uniquement destiné à des informations générales. Il est susceptible 
                  d'être modifié sans préavis. Bien que tous les efforts raisonnables aient été faits pour rendre les informations 
                  du Tableau de Bord de l'OPS/OMS sur le Choléra dans la Région des Amériques aussi opportunes et précises que 
                  possible, l'OMS ne fait aucune affirmation, ne fait aucune promesse et ne donne aucune garantie quant à 
                  l'efficacité, l'exhaustivité et l'exactitude du contenu du Tableau de Bord de l'OPS/OMS sur le Choléra dans 
                  la Région des Amériques, et décline expressément toute responsabilité pour les dommages résultant de 
                  l'utilisation et/ou de l'application du Tableau de Bord de l'OPS/OMS sur le Choléra dans la Région des Amériques, 
                  des erreurs et/ou des omissions dans le contenu. La responsabilité de l'interprétation et de l'utilisation du 
                  contenu incombe au lecteur. L'OPS/OMS se réserve le droit d'apporter des mises à jour et des modifications au 
                  contenu affiché sans préavis et n'accepte aucune responsabilité pour toute erreur ou omission à cet égard. 
                  L'OPS/OMS n'assume aucune responsabilité pour toute conséquence résultant directement ou indirectement de 
                  toute action ou inaction des lecteurs basée sur les informations et le matériel disponibles sur le Tableau 
                  de Bord de l'OPS/OMS sur le Choléra dans la Région des Amériques. Bien que tous les efforts raisonnables 
                  aient été faits pour utiliser un langage et des images appropriés sur le Tableau de Bord de l'OPS/OMS sur 
                  le Choléra dans la Région des Amériques, l'OPS/OMS décline expressément toute responsabilité en cas de langage 
                  ou d'images offensants ou insensibles, perçus ou réels, utilisés par inadvertance. L'OPS/OMS ne sera pas 
                  responsable de l'indisponibilité temporaire du Tableau de Bord de l'OPS/OMS sur le Choléra dans la Région 
                  des Amériques en cas de problèmes techniques ou autres. Les désignations employées et la présentation du 
                  contenu du Tableau de Bord de l'OPS/OMS sur le Choléra dans la Région des Amériques, y compris les noms 
                  des événements, les cartes et autres matériels d'illustration, n'impliquent pas l'expression d'une quelconque 
                  opinion de la part de l'OPS/OMS concernant le statut juridique d'un pays, d'un territoire, d'une ville ou 
                  d'une zone, ou de ses autorités, ou concernant le tracé des frontières et des limites. Les zones grises sur 
                  les cartes représentent des lignes frontalières approximatives pour lesquelles il se peut qu'il n'y ait pas 
                  encore d'accord complet.")
    )
    
  })
  
  output$top1 <- renderValueBox({
    i18n$set_translation_language(input$idiom)
    
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1
    valueBox(
      prettyNum(
        sum(if (input$country == i18n$t("All")) {
          db[which(db$date == displayDate & (db$ISOCODE=="HTI" | db$ISOCODE=="DOM")),]$Confirmed.cases
        } else if(input$country != i18n$t("All") & (input$adm1==i18n$t("All") | input$adm2==i18n$t("All"))) {
          db[which(db$date == displayDate & db$ISOCODE==input$country),]$Confirmed.cases
        } else if(input$country=="HTI" & input$adm1!=i18n$t("All")){
          db[which(db$date == displayDate & db$Admin1 == input$adm1),]$Confirmed.cases
        }else if(input$country=="DOM" & input$adm2!=i18n$t("All")){
          db[which(db$date == displayDate & db$Admin1 == input$adm2),]$Confirmed.cases
        },
        na.rm = T
        ),
        big.mark = ifelse(input$idiom=="en",",","."),
        decimal.mark = ifelse(input$idiom=="en",".",","),
        scientific = FALSE
      )
      ,
      i18n$t("Confirmed cases"),
      color = "blue",
      icon = icon("disease")
    )
  })
  
  output$top2 <- renderValueBox({
    i18n$set_translation_language(input$idiom)
    #db=database()
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1
    valueBox(
      prettyNum(
        sum(if (input$country == i18n$t("All")) {
          db[which(db$date == displayDate & (db$ISOCODE=="HTI" | db$ISOCODE=="DOM")),]$Confirmed.deaths
        } else if(input$country != i18n$t("All") & (input$adm1==i18n$t("All") | input$adm2==i18n$t("All"))) {
          db[which(db$date == displayDate & db$ISOCODE==input$country),]$Confirmed.deaths
        } else if(input$country=="HTI" & input$adm1!=i18n$t("All")){
          db[which(db$date == displayDate & db$Admin1 == input$adm1),]$Confirmed.deaths
        }else if(input$country=="DOM" & input$adm2!=i18n$t("All")){
          db[which(db$date == displayDate & db$Admin1 == input$adm2),]$Confirmed.deaths
        },
        na.rm = T
        ),
        big.mark = ifelse(input$idiom=="en",",","."),
        decimal.mark = ifelse(input$idiom=="en",".",","),
        scientific = FALSE
      )
      ,
      i18n$t("Deaths"),
      color = "light-blue",
      icon = icon("cross")
    )
  })
  
  output$top3 <- renderValueBox({
    i18n$set_translation_language(input$idiom)
    #db=database()
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1
    valueBox(
      prettyNum(
        sum(if (input$country == i18n$t("All")) {
          db[which(db$date == displayDate & (db$ISOCODE=="HTI" | db$ISOCODE=="DOM")),]$Suspected.cases
        } else if(input$country != i18n$t("All") & (input$adm1==i18n$t("All") | input$adm2==i18n$t("All"))) {
          db[which(db$date == displayDate & db$ISOCODE==input$country),]$Suspected.cases
        } else if(input$country=="HTI" & input$adm1!=i18n$t("All")){
          db[which(db$date == displayDate & db$Admin1 == input$adm1),]$Suspected.cases
        }else if(input$country=="DOM" & input$adm2!=i18n$t("All")){
          db[which(db$date == displayDate & db$Admin1 == input$adm2),]$Suspected.cases
        },
        na.rm = T
        ),
        big.mark = ifelse(input$idiom=="en",",","."),
        decimal.mark = ifelse(input$idiom=="en",".",","),
        scientific = FALSE
      )
      ,
      i18n$t("Suspected cases"),
      color = "purple",
      icon = icon("envelope")
    )
  })
  
  output$plot1 <- renderPlotly({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db1$date, na.rm = T)) max(db1$date, na.rm = T) else input$date1
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
      
    xTitle1 = i18n$t("Date of onset of symptoms")
    xTitle2 = i18n$t("Epidemiological week")
    yTitle = i18n$t("Number of cases")
    y2Title = i18n$t("Total of cases")
    
    ay <- list(
      tickfont = list(size=11.7),
      titlefont=list(size=14.6),
      overlaying = "y",
      nticks = 10,
      side = "right",
      title = y2Title,
      automargin = T,
      showgrid = F,
      showline= T, 
      linewidth=1.1, 
      linecolor='black'
    )
    
    if(input$mode==i18n$t("Day")){
      plot_ly(db1, x = ~date, y = ~Suspected,type = 'bar',name = i18n$t("Suspected")) %>% 
        add_lines(y=~cum,name=i18n$t("Cumulative"),yaxis="y2") %>% 
        layout(xaxis = list(title=xTitle1,showgrid = FALSE),yaxis = list(title = yTitle,showgrid = FALSE,showline= T, linewidth=1.1, linecolor='black'), barmode = 'stack') %>% 
        layout(title = NULL, yaxis2 = ay,xaxis = list(title=xTitle1)) %>% 
        layout(legend = list(orientation = "h",y=-0.15)) %>%
        layout(xaxis = list(type = 'date', autotick = T), margin = mrg, font = list(family = "Calibri", size = 12)) %>% 
        layout(yaxis = list(tickformat = ".0f")) %>% 
        layout(yaxis2 = list(tickformat = ".0f",overlaying = "y",side = "right",rangemode = "tozero"))
    } else {
      
      df1=aggregate(db1$Suspected,list(db1$dateweek),sum,na.rm=T)
      names(df1)=c("dateweek","Suspected")
      df1$cum=cumsum(df1$Suspected)
      
      plot_ly(df1, x = ~dateweek, y = ~Suspected,type = 'bar',name = i18n$t("Suspected")) %>% 
        add_lines(y=~cum,name=i18n$t("Cumulative"),yaxis="y2") %>% 
        layout(xaxis = list(title=xTitle2,showgrid = FALSE),yaxis = list(title = yTitle,showgrid = FALSE,showline= T, linewidth=1.1, linecolor='black'), barmode = 'stack') %>% 
        layout(title = NULL, yaxis2 = ay,xaxis = list(title=xTitle2)) %>% 
        layout(legend = list(orientation = "h",y=-0.15)) %>%
        layout(xaxis = list(type = 'date', tickformat = "EW%U"), margin = mrg, font = list(family = "Calibri", size = 12)) %>% 
        layout(yaxis = list(tickformat = ".0f")) %>% 
        layout(yaxis2 = list(tickformat = ".0f",overlaying = "y",side = "right",rangemode = "tozero"))
    }
    
    
    
    # plot_ly(db1[which(db1$date <= displayDate),], x = ~date, y = ~Suspected, type = 'bar') %>%
    #   layout(xaxis = list(title = xTitle, showgrid = FALSE, dtick = "tick1"),
    #          yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
    #   layout(legend = list(orientation = "h",y=-0.15)) %>%
    #   layout(xaxis = list(type = 'date', autotick = T), margin = mrg, font = list(family = "Calibri", size = 12))
    
  })
  
  output$plot2 <- renderPlotly({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db2$date, na.rm = T)) max(db2$date, na.rm = T) else input$date1
    mrg <- list(l = 50, r = 50,b = 50, t = 10,pad = 5)
    
    df=switch (input$mode2,
      "Confirmed" = db3,
      "Confirmado" = db3,
      "Confirmés" = db3,
      
      "Suspected" = db2,
      "Sospechoso" = db2,
      "Suspeito" = db2,
      "Suspect" = db2
    )
    
    df$Agegroup=factor(df$Agegroup,levels=c("<1y","1-4y","5-9y","10-14y","15-19y",
                                              "20-29y","30-39y","40-49y","50-59y",
                                              "60-69y","70-79y","80y+"))
    
    df=melt(df,id=c("date","Agegroup"))
    names(df)=c("date","agegroup","gender","cases")
    
    male = i18n$t("Male")
    female = i18n$t("Female")
    levelsready = c(male,female)
    df$gender=factor(df$gender,levels=c("Male","Female"),labels=levelsready)
    
    xTitle = i18n$t("Number of cases")
    yTitle = i18n$t("Age group")
    
    plot_ly(data=df[which(df$date == displayDate),], x = ~ifelse(gender==i18n$t("Female"),-cases,cases), y = ~agegroup, color=~gender, type = 'bar',colors="Set2",hoverinfo = 'y+text+name', text = ~cases) %>%
      layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T),
             yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'overlay') %>%
      layout(legend = list(orientation = "h")) %>%
      layout(xaxis = list(tickmode = 'array', tickvals = c(-2000,-1000,-800,-600,-400,-200,-100,-50, 0, 50, 100, 200, 400, 600,800,1000,2000),
                          ticktext = c("2000","1000","800","600","400","200","100","50", "0", "50", "100", "200", "400", "600","800","1000","2000")), margin = mrg, font = list(family = "Calibri", size = 12))
    
  })
  
  output$table1 <- DT::renderDataTable({
    i18n$set_translation_language(input$idiom)
    #db=database()
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1
    
    tab = db[which(db$date == displayDate),]
    tab=tab[,c("Country.Territory","Admin1","Confirmed.cases", "Confirmed.deaths", "Suspected.cases")]
    
    tab=tab[which(tab$Admin1!="TOTAL"),]
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #ACBED4;}"
    
    if (input$idiom == "en") {
      header.names <- c("Country","Adm Level 1" ,"Confirmed cases","Confirmed deaths","Suspected cases")
    } else if (input$idiom == "es") {
      header.names <- c("País","Adm nivel 1" ,"Casos confirmados","Defunciones confirmadas","Casos sospechosos")
    } else if (input$idiom == "pt") {
      header.names <- c("País","Adm nível 1" ,"Casos confirmados","Óbitos confirmados","Casos suspeitos")
    } else if (input$idiom == "fr") {
      header.names <- c("Pays","Adm niveau 1" ,"Cas confirmés","Décès confirmés","Cas suspects")
    }
    
    # The container parameter allows us to design the header of the table using CSS
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    if(input$idiom=="en"){
      my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons',
                            caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: left;',i18n$t("Undetermined: Suspect, probable or confirmed case without preliminary information on geographic location."))) %>%
        formatStyle(columns = c(3:5),
                    width = '100px',
                    fontFamily = "Calibri",
                    fontSize = "14px",
                    borderRightWidth = "1px",
                    borderRightStyle = "solid",
                    borderRightColor = "white",
                    borderBottomColor = "#ffffff",
                    borderBottomStyle = "solid",
                    borderBottomWidth = "1px",
                    borderCollapse = "collapse",
                    verticalAlign = "middle",
                    textAlign = "center",
                    wordWrap = "break-word") %>%
        formatRound(columns = c(3:5),
                    digits = 0,
                    mark = ",",
                    dec.mark = ".") %>% 
        formatStyle(columns = c(1:2),
                    width = '100px',
                    fontFamily = "Calibri",
                    fontSize = "14px",
                    borderRightWidth = "1px",
                    borderRightStyle = "solid",
                    borderRightColor = "white",
                    borderBottomColor = "#ffffff",
                    borderBottomStyle = "solid",
                    borderBottomWidth = "1px",
                    borderCollapse = "collapse",
                    verticalAlign = "middle",
                    textAlign = "left",
                    wordWrap = "break-word")
    } else {
      my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons',
                            caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: left;',i18n$t("Undetermined: Suspect, probable or confirmed case without preliminary information on geographic location."))) %>%
        formatStyle(columns = c(3:5),
                    width = '100px',
                    fontFamily = "Calibri",
                    fontSize = "14px",
                    borderRightWidth = "1px",
                    borderRightStyle = "solid",
                    borderRightColor = "white",
                    borderBottomColor = "#ffffff",
                    borderBottomStyle = "solid",
                    borderBottomWidth = "1px",
                    borderCollapse = "collapse",
                    verticalAlign = "middle",
                    textAlign = "center",
                    wordWrap = "break-word") %>%
        formatRound(columns = c(3:5),
                    digits = 0,
                    mark = ".",
                    dec.mark = ",") %>% 
        formatStyle(columns = c(1:2),
                    width = '100px',
                    fontFamily = "Calibri",
                    fontSize = "14px",
                    borderRightWidth = "1px",
                    borderRightStyle = "solid",
                    borderRightColor = "white",
                    borderBottomColor = "#ffffff",
                    borderBottomStyle = "solid",
                    borderBottomWidth = "1px",
                    borderCollapse = "collapse",
                    verticalAlign = "middle",
                    textAlign = "left",
                    wordWrap = "break-word")
    }
    
    
    
    
    path <- "/home/urstudio/cholera/datatable/" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    my.table$dependencies <- c(my.table$dependencies, list(dep))
    
    print(my.table)
  })
  
  output$map1 <- renderLeaflet({
    i18n$set_translation_language(input$idiom)
    displayDate <- if (input$date1 > max(db$date, na.rm = T)) max(db$date, na.rm = T) else input$date1

    wmap <- shapefile("/home/urstudio/cholera/shapefile/AMERICAS_ADM1_Haiti.shp")
    
    wmap@data = left_join(wmap@data, db[which(db$date==displayDate),c("ISOCODE","Confirmed.cases","Confirmed.deaths","Suspected.cases","Suspected.deaths")], by = c("ADM1_ISO_C"="ISOCODE"))
    
    pal1 <- colorNumeric(palette = "YlOrBr", domain = c(1, max(wmap@data$Suspected.cases, na.rm = T)), na.color = "white")

    wmapData = c()
    if (input$classif1 == i18n$t("Confirmed cases")) {
      wmapData = wmap@data$Confirmed.cases
    } else if (input$classif1 == i18n$t("Confirmed deaths"))  {
      wmapData = wmap@data$Confirmed.deaths
    } else if (input$classif1 == i18n$t("Suspected cases")){
      wmapData = wmap@data$Suspected.cases
    } else if (input$classif1 == i18n$t("Suspected deaths")){
      wmapData = wmap@data$Suspected.deaths
    }

    leaflet(wmap, options = leafletOptions(zoomControl = TRUE)) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(weight = 1, opacity = 1.0, color = 'grey', fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~pal1(wmapData),
                  popup = paste0("<strong>","Adm1",": </strong>", wmap@data$ADM1_ISO_N,
                                 "<br><strong>",i18n$t("Date as of"),": </strong>", date_format_lang(displayDate,input$idiom), # TODO Check with Silvano: ifelse(input$date1 >= max(db$date, na.rm = T) # Is using >= all others cases: >
                                 "<br><strong>",i18n$t("Total confirmed cases"), ": </strong>", wmap@data$Confirmed.cases,
                                 "<br><strong>",i18n$t("Total confirmed deaths"), ": </strong>", wmap@data$Confirmed.deaths,
                                 "<br><strong>",i18n$t("Total suspected cases"), ": </strong>", wmap@data$Suspected.cases,
                                 "<br><strong>",i18n$t("Total suspected deaths"), ": </strong>", wmap@data$Suspected.deaths),
                  label = wmap@data$ADM1_ISO_N,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
      addLegend(pal = pal1, values = wmapData, position = "bottomright", title = "Number of cases", opacity = 1) %>%
      setView(-72,19 , zoom = 8)
  })
  
  output$report1 <- downloadHandler(
    filename = function() {
      "cholera_en.pptx" # default file name use by browser, it could be different
    },
    content = function(file) {
      file.copy(file.path("/home/urstudio/cholera", "report_cholera_en.pptx"), file)
    }
  )
  
  output$report2 <- downloadHandler(
    filename = function() {
      "colera_sp.pptx" # default file name use by browser, it could be different
    },
    content = function(file) {
      file.copy(file.path("/home/urstudio/cholera", "report_cholera_sp.pptx"), file)
    }
  )
  
  output$report3 <- downloadHandler(
    filename = function() {
      "colera_pt.pptx" # default file name use by browser, it could be different
    },
    content = function(file) {
      file.copy(file.path("/home/urstudio/cholera", "report_cholera_pt.pptx"), file)
    }
  )
  
  output$report4 <- downloadHandler(
    filename = function() {
      "cholera_fr.pptx" # default file name use by browser, it could be different
    },
    content = function(file) {
      file.copy(file.path("/home/urstudio/cholera/", "report_cholera_fr.pptx"), file)
    }
  )

}


shinyApp(ui = ui, server = server)
