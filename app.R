
library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(DT)
library(plotly)
source('snd_theme.R')

read_dengue <- function(file = NULL){
  read_rds(file) %>% 
    #select(-`...2`, -`...3`, -`...5`, -`...6`, -`...7`, -`...9`, -`...10`, -`...11`, -`...15`, -`...16`, -`...17`, -`...18`, -`...19`, -`...22`, -`...23`, -`...24`, -`...33`) %>% 
    filter(str_detect(ESTADO, 'SONORA') | 
             is.na(ESTADO), 
           !`IDENTIFICADOR DE CASO` %in% c(1984821, 1977989, 1961235, 1959125, 1968280, 1985673, 1961400, 1988613, 1992459, 1993378, 1997080, 1996905)) %>% 
    mutate(ESTATUS = case_when(
      `IDENTIFICADOR DE CASO` %in% c(1387455, 1387451, 1388556, 1983715) ~ 'CONFIRMADO',			
      `IDENTIFICADOR DE CASO` == 1844308 ~ 'DESCARTADO',
      T ~ ESTATUS
    ), MUNICIPIO = str_remove(str_squish(MUNICIPIO), pattern = ' SON'),
    LOCALIDAD = str_squish(LOCALIDAD),
    Semana = paste(year(floor_date(as.Date.numeric(`FECHA DE INICIO`, origin = '1899-12-30'), 'week') + 3), str_pad(epiweek(as.Date.numeric(`FECHA DE INICIO`, origin = '1899-12-30')), pad = '0', side = 'left', width = 2), sep = '-'), 
    MUNICIPIO = case_when(
      MUNICIPIO == 'GUAYMAS' & LOCALIDAD == 'BAHIA DE LOBOS' ~ 'SAN IGNACIO RÍO MUERTO',
      MUNICIPIO == 'SAN IGNACIO RIO MUERTO' ~ 'SAN IGNACIO RÍO MUERTO',
      MUNICIPIO == 'GUAYMAS' & LOCALIDAD == 'SAN IGNACIO RIO MUERTO (COLONIA MILITAR)' ~ 'SAN IGNACIO RÍO MUERTO',
      MUNICIPIO == 'SAN LUIS RIO COLORADO' ~ 'SAN LUIS RÍO COLORADO',
      T ~ MUNICIPIO
    )) %>% 
    mutate(LOCALIDAD = case_when(
      MUNICIPIO == 'SAN IGNACIO RÍO MUERTO' & LOCALIDAD == 'BAHIA DE LOBOS' ~ 'BAHÍA DE LOBOS',
      MUNICIPIO == 'SAN IGNACIO RÍO MUERTO' & LOCALIDAD == 'SAN IGNACIO RIO MUERTO (COLONIA MILITAR)' ~ 'SAN IGNACIO RÍO MUERTO',
      
      MUNICIPIO == 'CAJEME' & LOCALIDAD == 'Cajeme' ~ 'CIUDAD OBREGÓN',
      
      
      MUNICIPIO == 'GUAYMAS' & LOCALIDAD == 'FÁTIMA' ~ 'HEROICA GUAYMAS',
      MUNICIPIO == 'SAN LUIS RÍO COLORADO' & LOCALIDAD %in% c('San Luis Rio Colorado',
                                                              'SONORA') ~ 'SAN LUIS RÍO COLORADO',
      T ~ LOCALIDAD
    )) %>% filter(!is.na(`IDENTIFICADOR DE CASO`)) %>% 
    mutate(`FECHA DE CAPTURA` = as.Date.numeric(`FECHA DE CAPTURA`, origin = '1899-12-30'), 
           `FECHA DE INICIO` = as.Date.numeric(`FECHA DE INICIO`, origin = '1899-12-30'))
}


#dengue <- bind_rows(
#  read_dengue('Report16_2025.rds'),
#  read_dengue('Report16.rds')
#)

lista <- read_rds('denguexsemana.rds')

epi_table <- function(df = NA){
  df %>% 
    count(Semana, ESTATUS) %>% 
    pivot_wider(names_from = ESTATUS, values_from = n, values_fill = 0) %>% 
    full_join(
      tibble(Semana= NA, CONFIRMADO = NA, DESCARTADO = NA, PROBABLE = NA)
      ) %>% 
    filter(!is.na(Semana))  %>% 
    replace(is.na(.), 0) %>% 
    mutate(Positividad = round(CONFIRMADO/(CONFIRMADO + DESCARTADO), 4)) %>% 
    mutate(Estimados = round(CONFIRMADO + PROBABLE*Positividad, 0)) %>% 
    select(Semana, PROBABLE, DESCARTADO, CONFIRMADO, Positividad, Estimados) %>% 
    right_join(
      tibble(Semana = c(str_c(2025, str_pad(seq(1,53), width = 2, pad = '0', side = 'left'), sep = '-'), 
                        str_c(2026, str_pad(seq(1,epiweek(Sys.Date())), width = 2, pad = '0', side = 'left'), sep = '-')))
    ) %>%
    replace(is.na(.), 0) %>% 
    arrange(Semana)
  }
  


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "snd.css"
    )
  ),
  
  div(
    class = "snd-header-ribbon",
    
    div(
      class = "snd-header-ribbon-inner",
      
      div(
        class = "snd-header-logo",
        
        tags$img(
          src = "img/gobierno-mexico.png",
          class = "snd-header-logo gobierno"
        ),
        
#        div(class = "snd-header-divider"),
        
        tags$img(
          src = "img/secretaria-salud.png",
          class = "snd-header-logo salud"
        ),
        
#        div(class = "snd-header-divider"),
        
        tags$img(
          src = "img/snsp.png",
          class = "snd-header-logo snsp"
        )
      ),
      
      div(
        class = "snd-header-title",
        h1(paste("Dengue en tiempo real (actualización", format(Sys.time(), '%d de %B de %Y a las %H:%M:%S', tz = 'MST'))),
        p("Sistema de información epidemiológica")
      )
    )
  ),
  #includeCSS("www/snd.css"),
  
    # Application title
   # titlePanel("Dengue en tiempo real"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Municipio", 
                        label = "Selecciona un municipio:", 
                        choices = c('Seleccionar', unique(lista$MUNICIPIO)),
                        selected = 'Seleccionar'),
            selectInput(inputId = "Localidad", 
                        label = "Selecciona una localidad:", 
                        choices = 'Seleccionar'),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
            title = "Casos por semana epidemiológica",
            br(),
            DTOutput("probables"),
            br(),
            DTOutput("confirmados"),
            br(),
            DTOutput("table")
          ),
            tabPanel(
              title = "Curva epidémica",
              br(),
              plotlyOutput("cases", height = "550px"), 
              br(),
              plotlyOutput("estimates", height = "550px")
            ),
            tabPanel(
              title = "Inicio de síntomas vs notificación",
              br(),
              plotlyOutput("sintvsnot") ## Estatal
            ),
          tabPanel(
            title = "Priorización de localidades",
            br(),
            DTOutput("priorizacion") ## Estatal
          )
            
#            tabPanel(
#              title = "Casos estimados",
#            ),
            )
        )
    )
)


dt <- bind_rows(
  read_dengue('Report16_2025.rds'),
  read_dengue('Report16.rds')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dengue <- reactive({
    if (input$Municipio != 'Seleccionar' & input$Localidad != 'Seleccionar') {
       dt %>% 
        filter(MUNICIPIO == input$Municipio, LOCALIDAD == input$Localidad)
    } else if (input$Municipio != 'Seleccionar') {
      dt %>% 
       filter(MUNICIPIO == input$Municipio)
    } else {
      dt 
    }
  })
  
  dengue_last <- reactive({
    dt %>%
      filter(`FECHA DE INICIO` >= Sys.Date() - 22)
  })
  
#  dengue_tot <- reactive({
#      read_dengue('Report16_2025.rds')
#  })
  
  
  ## Tabla confirmados últimos 21 días ##
  output$confirmados <- renderDT({
    dengue_last() %>% 
      filter(ESTATUS == 'CONFIRMADO') %>%
      count(MUNICIPIO) %>% 
      arrange(desc(n)) %>% 
      rename(Casos = n) %>% 
      datatable(extensions = 'Buttons', caption = 'Distribución de casos de dengue en los últimos 21 días',
                rownames = F,
                options = list(dom = 'Bfrtip',
                               buttons = c('excel'),
                               pageLength = -1
                )
      )
  })
  

  ## Tabla probables últimos 21 días ##
  output$probables <- renderDT({
    dengue_last() %>% 
      count(MUNICIPIO, Semana) %>%
      arrange(Semana) %>% 
      pivot_wider(values_from = n, names_from = Semana, values_fill = 0) %>% 
      replace(is.na(.), 0) %>% 
      datatable(extensions = 'Buttons', caption = 'Casos probables por Municipio en los últimos 21 días', 
                rownames = F,
                options = list(dom = 'Bfrtip',
                               buttons = c('excel'),
                               pageLength = -1
                )
      )
  })
  
  
    ## Tabla No.1 ##
  output$table <- renderDT({
    epi_table(dengue()) %>%
      datatable(extensions = 'Buttons', caption = 'Distribución de casos de dengue por semana epidemiológica',
                rownames = F,
                options = list(dom = 'Bfrtip',
                               buttons = c('excel'),
                               pageLength = -1
                               )
                )
  })

  # Curva epidémica  
  output$cases <- renderPlotly({
    epi_table(dengue()) %>%
      as_tibble() %>% 
      filter(str_detect(Semana, pattern = format(Sys.Date(), '%Y'))) %>% 
      plot_ly() %>% 
      add_bars(x = ~Semana, y = ~CONFIRMADO, name = 'Confirmados', marker = list(color = '#611232')) %>% 
      add_bars(x = ~Semana, y = ~DESCARTADO, name = 'Descartados', marker = list(color = '#2F6B3C')) %>% 
      add_bars(x = ~Semana, y = ~PROBABLE, name = 'Probables', marker = list(color = '#B38E5D')) %>%
      layout(yaxis = list(title = 'Casos',
                          showgrid = F,
                          ticks="outside", tickformat = ",d",
                          zerolinecolor = '#000000'),
             barmode = 'stack') %>% 
      snd_theme(
        title = 'DENGUE: Distribución de casos por semana epidemiológica',
        x_title = 'Semana epidemiológica',
        y_title = 'Casos'
      )
  })
  
  # Curva casos estimados  
  output$estimates <- renderPlotly({
    epi_table(dengue()) %>%
      as_tibble() %>% 
      filter(str_detect(Semana, pattern = format(Sys.Date(), '%Y'))) %>% 
      plot_ly() %>% 
      add_bars(x = ~Semana, y = ~Estimados, name = 'Estimados', marker = list(color = '#611232')) %>% 
      add_lines(x = ~Semana, y = ~Positividad, name = 'Positividad', marker = list(color = '#B38E5D'), yaxis = "y2") %>% 
      layout(
        yaxis2 = list(
          title = "Positividad", 
          side = "right", 
          showgrid = F, 
          range = c(0, 1), 
          ticks="outside",
          zerolinecolor = '#000000', 
          overlaying = "y"),
        barmode = 'stack')%>% 
      snd_theme(
        title = 'DENGUE: Casos estimados y positividad',
        x_title = 'Semana epidemiológica',
        y_title = 'Casos estimados'
      )
  })
    
  output$sintvsnot <- renderPlotly({
    full_join(
      dengue() %>%
        filter(`FECHA DE INICIO` >= Sys.Date() - 32) %>% 
        count(Fecha = `FECHA DE INICIO`) %>% 
        rename('Inicio de síntomas' = n),
      dengue() %>%
        filter(as.Date.character(format(`FECHA DE CAPTURA`, '%Y-%m-%d')) >= Sys.Date() - 32) %>% 
        count(Fecha = as.Date.character(format(`FECHA DE CAPTURA`, '%Y-%m-%d')))  %>% 
        rename('Fecha de captura' = n)
    ) %>% 
      right_join(
        tibble(
          Fecha = seq.Date(from = Sys.Date() - 32, to = Sys.Date() - 1, by = 'day')
        )
      ) %>% 
      arrange(Fecha) %>% 
      mutate(Fecha = as.factor(Fecha)) %>% 
      plot_ly() %>% #x = ~Fecha, y = ~`Inicio de síntomas`, name = 'Inicio de síntomas', mode = 'lines', line = list(color = '#611232')) %>% 
      add_lines(x = ~Fecha, y = ~`Inicio de síntomas`, name = 'Inicio de síntomas', mode = 'lines', line = list(color = '#611232')) %>% 
      add_lines(x = ~Fecha, y = ~`Fecha de captura`, name = 'Fecha de captura', mode = 'lines', line = list(color = '#B38E5D')) %>% 
      #layout() %>% 
      snd_theme(
        title = 'DENGUE: Fecha de inicio de síntomas vs fecha de captura',
        x_title = 'Fecha',
        y_title = 'Casos'
      )
  })
  
  
  
  ## testing -----

  output$priorizacion <- renderDT({
  x <- right_join(
    read_rds('conjunto_de_datos_iter_26CSV20.rds') %>% 
      filter(NOM_MUN != 'Total de la entidad Sonora', !NOM_LOC %in% c('Localidades de una vivienda', 'Localidades de dos viviendas', 'Total del Municipio')) %>% 
      mutate(quit = case_when(
        NOM_MUN == 'Hermosillo' & NOM_LOC == 'Hermosillo' & POBTOT == 32 ~ 1,
        T ~ 0
      )) %>% 
      filter(quit == 0) %>%
      mutate(MUNICIPIO = str_to_upper(NOM_MUN),
             LOCALIDAD = str_to_upper(NOM_LOC),
             Población = POBTOT) %>% 
      select(MUNICIPIO, LOCALIDAD, Población),
    left_join(
      dengue_last() %>% 
        #  bind_rows(prev %>% select(-FEC_CAPTURA, -FEC_INI_SIGNOS_SINT)) %>% 
        count(JURISDICCIÓN, MUNICIPIO, LOCALIDAD, ESTATUS) %>% 
        filter(!is.na(ESTATUS)) %>% 
        pivot_wider(names_from = ESTATUS, values_from = n, values_fill = 0) %>% 
        filter(CONFIRMADO >= 1),
      dengue_last() %>% 
        count(JURISDICCIÓN, MUNICIPIO, LOCALIDAD, `DIAGNOSTICO PROBABLE`) %>% 
        filter(!is.na(`DIAGNOSTICO PROBABLE`), !str_detect(`DIAGNOSTICO PROBABLE`, pattern = 'OTROS')) %>% 
        pivot_wider(names_from = `DIAGNOSTICO PROBABLE`, values_from = n, values_fill = 0)
    )
  ) %>% 
    replace(is.numeric(is.na(.)), 0) %>% 
    mutate('Factor poblacional' = case_when(
      #Población <= 500 ~ 1,
      #Población %in% 501:1000 ~ 1.25,
      #Población %in% 1001:2500 ~ 1.5,
      #Población %in% 2501:5000 ~ 1.75,
      #Población %in% 5001:10000 ~ 2,
      #Población %in% 10001:25000 ~ 2.5,
      #Población %in% 25001:100000 ~ 3,
      #Población > 100000 ~ 4
      Población <= 500 ~ 0.2,
      Población %in% 501:1000 ~ 0.4,
      Población %in% 1001:2500 ~ 0.6,
      Población %in% 2501:5000 ~ 0.8,
      Población %in% 5001:10000 ~ 1,
      Población %in% 10001:25000 ~ 1.6,
      Población %in% 25001:100000 ~ 2.3,
      Población > 100000 ~ 3
    ), 'Confirmados + probables' = CONFIRMADO + PROBABLE,
    'DCSA + DG' = `DENGUE CON SIGNOS DE ALARMA` + `DENGUE GRAVE`) %>% 
    mutate('INCIDENCIA ULTIMOS 21 DIAS' = `Confirmados + probables`/Población*1000,
           '(%) DCSA + DG' = `DCSA + DG`/(`Confirmados + probables` + DESCARTADO),
           '(%) Positividad' = CONFIRMADO/(CONFIRMADO + DESCARTADO)) %>% 
    mutate('Valor de priorizacion' = ((((CONFIRMADO*2) + PROBABLE)*`INCIDENCIA ULTIMOS 21 DIAS`) + 
                                        ((`(%) DCSA + DG`*100) + (`(%) Positividad`*100)))*`Factor poblacional`) %>% 
    filter(`Confirmados + probables` >= 1) %>% 
    arrange(desc(`Valor de priorizacion`)) %>% 
    select(JURISDICCIÓN, MUNICIPIO, LOCALIDAD, Población, PROBABLE, CONFIRMADO, DESCARTADO, `DENGUE NO GRAVE`, `DENGUE CON SIGNOS DE ALARMA`, `DENGUE GRAVE`, `Factor poblacional`, `Confirmados + probables`, `DCSA + DG`, `INCIDENCIA ULTIMOS 21 DIAS`, `(%) DCSA + DG`, `(%) Positividad`, `Valor de priorizacion`) %>% 
    filter(Población > 21) 
    
    x %>% 
      mutate(Posición = seq(1, nrow(x))) %>% 
      select(Posición, JURISDICCIÓN, MUNICIPIO, LOCALIDAD, Población, PROBABLE, CONFIRMADO, DESCARTADO, `DENGUE NO GRAVE`, `DENGUE CON SIGNOS DE ALARMA`, `DENGUE GRAVE`, `Factor poblacional`, `Confirmados + probables`, `DCSA + DG`, `INCIDENCIA ULTIMOS 21 DIAS`, `(%) DCSA + DG`, `(%) Positividad`, `Valor de priorizacion`) %>% 
      datatable(extensions = 'Buttons', caption = 'Priorización por localidades', 
                rownames = F,
                options = list(dom = 'Bfrtip',
                               buttons = c('excel'), 
                               pageLength = -1
                )
    ) %>% 
      formatStyle('Valor de priorizacion', target = 'row', backgroundColor = styleInterval(cuts = c(100, 300, 1000), values = c('darkgreen', 'yellow', 'orange', 'darkred')))
  })

#  x %>% 
#    mutate(Posición = seq(1, nrow(x))) %>% 
#    select(Posición, JURISDICCIÓN, MUNICIPIO, LOCALIDAD, Población, PROBABLE, CONFIRMADO, DESCARTADO, `DENGUE NO GRAVE`, `DENGUE CON SIGNOS DE ALARMA`, `DENGUE GRAVE`, `Factor poblacional`, `Confirmados + probables`, `DCSA + DG`, `INCIDENCIA ULTIMOS 21 DIAS`, `(%) DCSA + DG`, `(%) Positividad`, `Valor de priorizacion`) %>% 
#    openxlsx::write.xlsx('priorizacion_dengue.xlsx', overwrite = T)
  
  ## finish testing ----
  
    
    observeEvent(input$Municipio, {
      localidad_f <- lista[lista$MUNICIPIO == input$Municipio, ]
      updateSelectInput(session, "Localidad", choices = c('Seleccionar', unique(localidad_f$LOCALIDAD)))
    })
    
    session$allowReconnect(TRUE)   

}

# Run the application 
#shinylive::export("./", "./docs")
shinyApp(ui, server)
