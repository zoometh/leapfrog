library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(raster)
library(DT)
library(shinyWidgets)
library(htmlwidgets)
library(dplyr)
library(base64enc)


###################################################
##### cartographie chronologique pour CIMO ########
###################################################
# auteurs:
#   - application: Thomas Huet <thomashuet7@gmail.com>
#   - base de données: Didier Binder <didier.binder@cepam.cnrs.fr>
# date:
#   - aout 2021
# institution:
#   - CEPAM-CNRS, UMR 7264 <https://www.cepam.cnrs.fr/>
##################################################

# se place dans le dossier courant
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("leapfrog_setup.R") # prépare les données

# si public dégrade les coordonnées
usage.public <- T
if(usage.public){
  hover.coord.precis <- 2 # arrondi au centième
  maxZoom <- 14 # nb: le + précis est 18
} else {
  hover.coord.precis <- 6
  maxZoom <- 18
}

# la liste des cultures
dropdownButton <- function(label = "", status = c("default", "primary", 
                                                  "success", "info", 
                                                  "warning", "danger"), ...,
                           width = NULL) {
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button",
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}
# logo
b64 <- base64enc::dataURI(file = "leapfrog_logo.png", 
                          mime = getwd())

############################################### ui #################################################
ui <- navbarPage("Leapfrog - cartographie dynamique",
                 tabPanel(title = "cartographie",
                          fluidPage(
                            tags$style(".checkbox { /* checkbox is a div class*/
                                       /* line-height: 10px;  */
                                       font-size:10px;
                                       height: 15px; /*Desired height*/
                                       background-color: #f2f2f2;
                                       }
                                       input[type='checkbox']{ /* les cases elles-memes */
                                       width: 12px; /*Desired width*/
                                       height: 15px; /*Desired height*/
                                       /* line-height: 20px;*/
                                       }
                                       span {
                                            margin-left: 5px;  /*set the margin, so boxes don't overlap labels*/
                                            /* line-height: -15px; */
                                       }
                                       .leaflet .legend {
                                        line-height: 10px;
                                        font-size: 10px;
                                       }
                                       .leaflet .legend i{
                                        border-radius: 50%;
                                        width: 10px;
                                        height: 10px;
                                        margin-top: 4px;
                                       }
                                       "),
                            fluidRow(
                              column(12,
                                     helpText("modifier le 'slider' pour filtrer les entités sur les périodes |
                                              cocher/decocher 'cultures' pour afficher/cacher les cultures"),
                                     leafletOutput("map", width = "100%", height = 700),
                                     # le logo
                                     absolutePanel(top = 45, left = 25,
                                                   img(src=b64, width="35%", align='left')),
                                     # slider
                                     absolutePanel(top = 40, left = 130,
                                                   sliderTextInput("range", label="periodes", # lper[1], lper[length(lper)],
                                                                   choices = lper, selected = range(lper), grid = TRUE, width='550px')),
                                     # fiabilite resume
                                     absolutePanel(top = 60, right = 240,
                                                   checkboxGroupInput("valeur_resume", "fiab.resume",
                                                                      choices=sort(val.resum, decreasing = TRUE),
                                                                      selected=sort(val.resum, decreasing = TRUE),
                                                                      width = "50px")),
                                     # cultures
                                     absolutePanel(top = 60, right = 110,
                                                   tags$style(".container {width: 100%; height: 500px; overflow-y: scroll; }"),
                                                   dropdownButton(
                                                     label = "cultures", status = "default", width = 40,
                                                     tags$div(
                                                       class = "container",
                                                       checkboxGroupInput("cults", "cultures",
                                                                          choices=names(lcul_col),
                                                                          selected=names(lcul_col),
                                                                          width = "100px"))
                                                   )),
                                     absolutePanel(bottom = 10, left = 150,
                                                   textOutput("out"),
                                                   tags$head(tags$style(HTML("#out {font-size: 14px;}")))
                                     )
                              )),
                            fluidRow(
                              helpText("cliquer sur la carte pour obtenir les coordonnées long / lat (WGS84) du curseur"),
                              htmlOutput("nb.dat"),
                              column(12,
                                     div(DTOutput("tbl"), style = "font-size:70%"))
                            )
                          )),
                 tabPanel("légende cultures",
                          fluidPage(
                            fluidRow(
                              fluidRow(div(DTOutput("lgcult"), style = "font-size:70%"))
                            )
                          )
                 ),
                 tabPanel("légende périodes",
                          fluidPage(
                            fluidRow(
                              fluidRow(div(DTOutput("lgperiod"), style = "font-size:70%"))
                            )
                          )
                 ),
                 tabPanel("données",
                          fluidPage(
                            fluidRow(
                              fluidRow(div(DTOutput("hot"), style = "font-size:70%"))
                            )
                          )
                 )
)

############################################### server #################################################
server <- function(input, output, session) {
  df.tot[,"site"] <- as.character(df.tot[,"site"])
  # table dans l'onglet des données
  output$hot <- renderDataTable({
    datatable(
      df.tot[, refcols],
      rownames = FALSE,
      width = "100%",
      editable=FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = 800,
        pageLength = 100
      )
    )
  })
  # légende dans l'onglet des cultures
  output$lgcult <- renderDataTable({
    datatable(
      cultures,
      rownames = FALSE,
      width = "100%",
      editable=FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = 800,
        pageLength = 100
      )
    ) %>%
      formatStyle(
        "code_aspect",
        backgroundColor = styleEqual(cultures$code_aspect, 
                                     toupper(cultures$hexa))
      )
  })
  # légende dans l'onglet des periodes
  output$lgperiod <- renderDataTable({
    datatable(
      t(periodes_df),
      rownames = TRUE,
      editable=FALSE,
      caption = "cal BC",
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '100px', targets = "_all")),
        pageLength = 100
      )) %>%
      formatStyle(
        columns = colnames(.),
        fontSize = '100%'
      )
  })
  output$nb.dat <- renderUI({
    n.entites <- nrow(data_map())
    n.cultures <- length(unique(data_map()$culture))
    HTML(paste0("Donnees dans la zone d'etude et avec les filtres: ",
                "<b>",n.entites,"</b> entites ",
                "<b>",n.cultures,"</b> cultures"))
  })
  # petite table filtrée sous la carte
  output$tbl <- renderDataTable({
    datatable(
      data_map(),
      rownames = FALSE,
      extensions = c("Scroller","Buttons"),
      style = "bootstrap",
      class = "compact",
      width = "100%",
      editable = TRUE,
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 300,
        scroller = TRUE,
        dom = 'tp'
      )
    )
  })
  # filtre les entités sur leurs coordonnées, leur chrono, leur fiabilité
  in_bounding_box <- function(data.f, lat, long, tpq, taq, bounds, tpq.per, taq.per) {
    data.f %>%
      dplyr::filter(
        lat >= bounds$south &
          lat <= bounds$north &
          long <= bounds$east &
          long >= bounds$west &
          ((taq.per > input$range[1] & tpq.per < input$range[1]) |
             (tpq.per < input$range[2] & taq.per > input$range[2]) |
             (tpq.per >= input$range[1] & taq.per <= input$range[2]) |
             (tpq.per <= input$range[1] & taq.per >= input$range[2])) &
          valeur_resume %in% input$valeur_resume &
          culture %in% input$cults
      )
  }
  # calcule l'extension de la carte (dynamique)
  data_map <- reactive({
    if (is.null(input$map_bounds)) {
      data.f <- df.tot[ , !(names(df.tot) %in% drops.columns)]
      data.f
    }
    else {
      data.f <- df.tot[ , !(names(df.tot) %in% drops.columns)]
      bounds <- input$map_bounds
      in_bounding_box(data.f, df.tot$y, df.tot$x, df.tot$tpq, df.tot$taq, bounds, df.tot$tpq.per, df.tot$taq.per)
    }
  })
  # reactive
  filteredData <- reactive({
    tpq.taq.val <- (
      df.tot.sp$taq.per > input$range[1] & df.tot.sp$tpq.per < input$range[1]) |
      (df.tot.sp$tpq.per < input$range[2] & df.tot.sp$taq.per > input$range[2]) |
      (df.tot.sp$tpq.per >= input$range[1] & df.tot.sp$taq.per <= input$range[2]) |
      (df.tot.sp$tpq.per <= input$range[1] & df.tot.sp$taq.per >= input$range[2])
    cult.select <- df.tot.sp$culture %in% input$cults
    fiab.resume.val <- df.tot.sp$valeur_resume %in% input$valeur_resume
    inside <- c(df.tot.sp$y >= input$map_bounds$south &
                  df.tot.sp$y <= input$map_bounds$north &
                  df.tot.sp$x <= input$map_bounds$east &
                  df.tot.sp$x >= input$map_bounds$west)
    dyna.df_ <- df.tot.sp[tpq.taq.val & fiab.resume.val & cult.select & inside, ]
    dyna.df_
  })
  output$map <- renderLeaflet({
    # fond de carte (non dynamique)
    leaflet(df.tot.sp, options = leafletOptions(zoomControl = FALSE,
                                                maxZoom = maxZoom)) %>%
      htmlwidgets::onRender("function(el, x) {
      L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      addTiles() %>%
      # addMapPane("buff.j1_", zIndex = 410) %>% # BUFFERS
      addMapPane("sites_", zIndex = 420) %>%
      fitBounds(~min(x),~min(y),~max(x),~max(y)) %>%
      # obtient les coord.
      onRender(
        "function(el,x){
                    this.on('click', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lng, lat];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                }"
      )
  })
  # les coordonnees cliquées
  output$out <- renderText({
    if(!is.null(input$hover_coordinates)) {
      paste0(round(input$hover_coordinates[1], hover.coord.precis), ",",
             round(input$hover_coordinates[2], hover.coord.precis))
    }
  })
  observe({
    if (nrow(filteredData()) > 0) {
      legende <- unique(filteredData()@data[c("culture", "couleur")])
      legende_ord <- legende[match(names(lcul_col), legende$culture),] # reordonne sur liste Didier
      legende_ord <- legende_ord[complete.cases(legende_ord),]
      proxy.sites <- leafletProxy("map", data = filteredData()) %>%
      #   addProviderTiles(providers$Esri.WorldImagery, group='Ortho') %>%
      #   addProviderTiles(providers$OpenStreetMap, group = 'OSM') %>%
      #   addProviderTiles(providers$Stamen.TerrainBackground, group='Terrain')
      # addTiles(group = 'OSM') %>%
        addProviderTiles(providers$Esri.WorldImagery, group='Ortho') %>%
        addProviderTiles(providers$OpenStreetMap, group = 'OSM') %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group='Terrain') %>%
        addWMS(group = "Clim",
               baseUrl = "http://54.155.109.226:8080/geoserver/ows",
               layers = "Beck_KG_V1_present_0p0083",
               options = WMSTileOptions(
                 transparent = TRUE,
                 format = "image/png",
                 info_format = "text/html")
        )
      proxy.sites %>% clearControls() %>% clearShapes() %>% clearMarkers() %>%
        addLayersControl(
          baseGroups = c('Ortho', 'OSM', 'Terrain')) %>%
        addScaleBar(position = "bottomright")
        # addScaleBar("map", position = "bottomright")
      proxy.sites %>% #
        addCircleMarkers(lng = ~x,
                         lat = ~y,
                         weight = ~contour,
                         radius = 3,
                         popup = ~lbl,
                         fillColor = ~couleur,
                         fillOpacity = ~alpha,
                         color = ~couleur,
                         opacity = 1,
                         label = ~site,
                         group = df.tot.sp$culture,
                         options = pathOptions(pane = "sites_"))  %>%
        addLayersControl(
          baseGroups = c('Ortho', 'OSM', 'Terrain', 'Clim')) %>%
        addLegend("bottomleft",
                  colors = as.vector(legende_ord$couleur),
                  labels = as.vector(legende_ord$culture),
                  title = "cultures",
                  opacity = 1) %>%
        addScaleBar(position = "bottomright")
        # addScaleBar("map", position = "bottomright")
    }
  })
}

shinyApp(ui, server)
