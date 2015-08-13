library(shiny)
#options(unzip = "internal"); devtools::install_github("rstudio/leaflet")
library(leaflet)
library(DT)

if (!file.exists("R/v1.RData")) {
    if (all.equal(dir("TopoJSON/"),
                  c("LocalMunicipalities2011.json",
                    "Province_New_SANeighbours.json",
                    "Wards2011.json"))) {
        source("data_mapping.R")
    } else if (!all.equal(dir("TopoJSON/"),
                          c("LocalMunicipalities2011.json",
                            "Province_New_SANeighbours.json",
                            "Wards2011.json"))) {
        stop("check your working directory is correct.
             Can't find TopoJSON or .RData source files")
    }
    } else {
        load("R/v1.RData")
}

#source("R/topoJSON_string_style.R")
#town_density <- topoJSON_property_extract(
#    topoJSON_string = town_tj, property_name = "DENSITY"
#)

#saveRDS(object = town_density, file = "R/town_density")
town_density <- readRDS(file = "R/town_density")

# ward_density <- topoJSON_property_extract(
#     topoJSON_string = ward_tj, property_name = "DENSITY"
# )
# province_density <- topoJSON_property_extract(
#     topoJSON_string = province_tj, property_name = "DENSITY"
# )



ui <- navbarPage(
    title = "South African Demographics",
    tabPanel(
# Add CSS to give floating Panels a style
        tags$head(tags$style(".floater { background-color: white; 
                             padding: 8px; opacity: 0.7; border-radius: 6px; 
                             box-shadow: 0 0 15px rgba(0,0,0,0.2); }")),
        
        title = "Map",
        leafletOutput(outputId = 'map1', width = "100%", height = "650px"),
        absolutePanel(
            right=25,top=80,width =260, class="floater",
            checkboxInput(inputId = "enable_hover", label = "select to access info on hover rather than on click"),
#          selectInput(inputId = 'enable_hover',label=NULL,choices = c('access info on hover','access info on click'), multiple = F),
            conditionalPanel(
                condition = "input.enable_hover == true",
#            condition = "input.enable_hover == 'hover'",
                h3("SA Population Density"),
                uiOutput('hoverInfo')
            )
        ),
        absolutePanel(
            right = 25, top = 330, width = 200, class="floater",
            radioButtons( 
                inputId='select_map_level',
                label=p(h4(strong('Select a level:')),
                        em('At high zoom, Wards'),
                        br(),em('are displayed')),
                choices=c('Province','Municipality'),
                selected='Municipality',
                inline=F
            )
        ),
        absolutePanel( # Zoom level
            left=20,top=720,width =120,
            textOutput(outputId='message3',container=span,inline=T)
        ),
        absolutePanel( # Click coordinates
            left=140,top=720,width =300,
            textOutput(outputId='message1',container=span,inline=T)
        ),
        absolutePanel( # Province
            left=440,top=720,width =340,
            textOutput(outputId='message_slice2',container=span,inline=T)
        ),
        absolutePanel( # Municipality
            left=780,top=720,width =130,
            textOutput(outputId='message_slice1',container=span,inline=T)
        ),
        absolutePanel( # spd@data$ID)
            left=910,top=720,width =35,
            textOutput(outputId='message_shp_id',container=span,inline=T)
        ),
        absolutePanel( # gis$mouse_events
            left=945,top=720,width =20,
            textOutput(outputId='message_events',container=span,inline=T)
        ),
        absolutePanel( # Boundary Coordinates
            left=965,top=720,width =600,
            textOutput(outputId='message4',container=span,inline=T)
        )
    ),

    navbarMenu(title = "Data",
               tabPanel(title = "Provincial Data",
                        DT::dataTableOutput(outputId = "table_province")
               ),
               tabPanel("Municipal Data",
                        DT::dataTableOutput(outputId = "table_town")
               ),
               tabPanel("Ward Data",
                        DT::dataTableOutput(outputId = "table_ward")
               )
    )
    
#     tabPanel(
#         title="Shape Data",
#         selectInput(
#             inputId='select_data_level', 
#             label=h4(
#                 strong('Select a level:'),
#                 br(),
#                 span('(Ward, Municipal or Provincial)')
#             ),
#             choices=c('Ward','Municipal','Provincial'),
#             selected='Municipality'
#         ),
#         DT::dataTableOutput(outputId='table')
#     )
)


server <- function(session, input, output) {
    
    gis <- reactiveValues(tj=town_tj)
    gis <- reactiveValues(shp=town_tj_spd)
    gis <- reactiveValues(id=NULL)
    gis <- reactiveValues(binpal=NULL)
    gis <- reactiveValues(slice1=NULL)
    gis <- reactiveValues(slice2=NULL)
    gis <- reactiveValues(shp_mouseover_id=NULL)
    gis <- reactiveValues(mouse_events=0)
    gis <- reactiveValues(shp_click_id=NULL)
    
    observeEvent(input$map1_topojson_mouseover, {
        gis$mouse_events <- 1
    })
#    observeEvent(input$map1_topojson_click, {
#        gis$mouse_events <- 1
#    })
#    observeEvent(input$map1_shape_click, {
#        gis$mouse_events <- 1
#    })
    observeEvent(input$map1_topojson_mouseout, {
        gis$mouse_events <- 0
    })
    
    observe(if (input$select_map_level == 'Ward'){label="event3"
    gis$tj <- gis$tj <- town_tj
    })
    observe(if (input$select_map_level == 'Municipality'){
        gis$tj <- town_tj
    })
    observe(if (input$select_map_level == 'Province'){
        gis$tj <- province_tj
    })
    observe(if (input$select_map_level == 'Ward' & 
                ((!is.null(gis$shp_mouseover_id)) | (!is.null(input$map1_topojson_mouseover)) |
                 (!is.null(gis$shp_click_id)))){
        gis$shp <- subset(ward_tj_spd,
                          subset = ward_tj_spd@data$MUNICNAME == gis$slice2)
    })
    observe(if (input$select_map_level == 'Municipality'){
        gis$shp <- town_tj_spd
    })
    observe(if (input$select_map_level == 'Province'){
        gis$shp <- province_tj_spd
    })
#    observe(if (!is.null(gis$shp)){
#        gis$bbox_spd <- bbox(gis$shp)
#    })
    observeEvent(input$map1_topojson_mouseover, label="event4",{
        if (input$select_map_level == 'Ward'){
            gis$id <- input$map1_topojson_mouseover$properties$WARD
        }
    })
    observeEvent(input$map1_topojson_mouseover, label="event5", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- input$map1_topojson_mouseover$properties$MAP_TITLE
        }
    })
    observeEvent(input$map1_topojson_mouseover,label="event6", {
        if (input$select_map_level == 'Province'){
            gis$id <- input$map1_topojson_mouseover$properties$PROVINCE
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event7",{
        if (input$select_map_level == 'Ward'){
            gis$id <- gis$shp@data$WARD[gis$shp@data$ID == gis$shp_mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event8", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- gis$shp@data$MAP_TITLE[gis$shp@data$ID == gis$shp_mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event9",{
        if (input$select_map_level == 'Province'){
            gis$id <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$sh_mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseout, label="event10",{
        gis$id <- NULL
    })
    observe(if (input$select_map_level == 'Ward'){label="event11"
    gis$binpal <- ward_binpal
    })
    observe(if (input$select_map_level == 'Municipality'){label="event12"
    gis$binpal <- town_binpal
    })
    observe(if (input$select_map_level == 'Province'){label="event13"
    gis$binpal <- province_binpal
    })
    observeEvent(input$map1_topojson_mouseover, label="event14",{
        gis$slice1 <- input$map1_topojson_mouseover$properties$PROVINCE
    })
    observeEvent(input$map1_shape_click, label="event14a",{
        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == input$map1_shape_click$id]
    })
    observeEvent(input$map1_topojson_mouseover, label="event14b",{
        gis$slice2 <- input$map1_topojson_mouseover$properties$MAP_TITLE
    })

#    observeEvent(input$map1_topojson_mouseout, label="event15",{
#        gis$slice2 <- NULL
#    })
    observeEvent(input$map1_shape_mouseover, label="event16a",{
        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$shp_mouseover_id]
    })
    observe(if(!is.null(gis$shp_mouseover_id)){ 
        label="event16b"
        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$shp_mouseover_id]
    })
    observe(if((!is.null(gis$shp_mouseover_id) & input$select_map_level == 'Municipality')){ 
        label="event16c"
        gis$slice2 <- town_tj_spd@data$MAP_TITLE[town_tj_spd@data$ID == gis$shp_mouseover_id]
    })
    observe(if((!is.null(input$map1_shape_click$id)) & input$select_map_level == 'Municipality'){ 
        label="event16d"
        gis$slice2 <- gis$shp@data$MAP_TITLE[gis$shp@data$ID == input$map1_shape_click$id]
    })
#     observeEvent(input$map1_topojson_click, label="event16e", {
#         observe(if( input$select_map_level == 'Municipality'){
#             gis$slice2 <- input$map1_topojson_click$properties$MAP_TITLE
#         })
#     })
#    observeEvent(input$map1_topojson_click, label="event16f", {
#        observe(if( input$select_map_level == 'Ward'){
#            gis$slice2 <- input$map1_topojson_click$properties$MUNICNAME
#        })
#    })
    observeEvent(input$map1_topojson_click, label="event16g", {
        gis$slice1 <- input$map1_topojson_click$properties$PROVINCE
    })
    
#    observeEvent(input$map1_shape_mouseout, label="event17",{
#        gis$slice1 <- NULL
#    })
    observeEvent(input$map1_shape_mouseover, label="event18a",{
        gis$shp_mouseover_id <- input$map1_shape_mouseover$id
    })
    observeEvent(input$map1_topojson_click, label="event18aa", {
        gis$shp_click_id <- input$map1_shape_click$id
    })
    observeEvent(input$map1_shape_click, label="event18ab",{
        gis$shp_click_id <- input$map1_shape_click$id
    })
    observeEvent(input$map1_shape_mouseout, label="event18b",{
        gis$shp_mouseover_id <- NULL
    })
#    observeEvent(input$map1_topojson_mouseover, label="event18c",{
#        gis$shp_mouseover_id <- input$map1_topojson_mouseover$properties$ID
#    })
#    observe(if (is.null(input$map1_shape_mouseover$id) & gis$mouse_events == 0) {    
#        gis$shp_mouseover_id <- NULL
#        label="event18d"
#    })
    
    output$message_slice1 <- renderText(gis$slice1)
    output$message_slice2 <- renderText(gis$slice2)
    output$message_shp_id <- renderText(gis$shp_mouseover_id)
    output$message_id <- renderText(gis$id)
    output$message_events <- renderText(gis$mouse_events)
    
    v <- reactiveValues(msg1=NULL) # Click Latitude
    v <- reactiveValues(msg2=NULL) # Click Longitude
    v <- reactiveValues(msg3=NULL) # Zoom level
    v <- reactiveValues(lng1 = -23)
    v <- reactiveValues(lat1 = 43) 
    v <- reactiveValues(lng2 = -35)
    v <- reactiveValues(lat2 = 10)
    v <- reactiveValues(msg5=NULL) # Ward/Municipality/Province Name of mouseover object
    v <- reactiveValues(msg6=NULL) # Population Density of mouseover object
    v <- reactiveValues(msg7=NULL) # Municipality name in Ward level mouseover object
    
    observeEvent(input$map1_click, {
        v$msg1 <- input$map1_click$lat
    })
    observeEvent(input$map1_topojson_click, {
        v$msg1 <- input$map1_topojson_click$lat
    })
    observeEvent(input$map1_shape_click,{
        v$msg1 <  input$map1_shape_click$lat
        
    })
    observeEvent(input$map1_click, {
        v$msg2 <- input$map1_click$lng
    })
    observeEvent(input$map1_topojson_click, {
        v$msg2 <- input$map1_topojson_click$lng
    })
    observeEvent(input$map1_shape_click,{
        v$msg2 <  input$map1_shape_click$lng
        
    })
    observeEvent(input$map1_zoom, label="zoom_message", {
        v$msg3 <- input$map1_zoom
    })
    observeEvent(input$map1_bounds, {
        v$lng1 <- input$map1_bounds[4]
    })
    observeEvent(input$map1_bounds, {
        v$lat1 <- input$map1_bounds[3]
    })
    observeEvent(input$map1_bounds, {
        v$lng2 <- input$map1_bounds[2]
    })
    observeEvent(input$map1_bounds, {
        v$lat2 <- input$map1_bounds[1]
    })
    output$message1 <- renderText(if (!is.null(v$msg1)) {
        paste("Click coordinates: long",round(v$msg2,3),
              "lat",round(v$msg1,3))
    }) # Click Coordinates
    
    output$message3 <- renderText(paste("Zoom level is",v$msg3)) # Zoom level
    output$message4 <- renderText(
        paste(
            "View Bounds: long",substr(paste(v$lng1),start=1,stop=6),
            "lat",substr(paste(v$lat1),start=1,stop=6),"(bottomleft/SW); ",
            "long",substr(paste(v$lng2),start=1,stop=6),
            "lat",substr(paste(v$lat2),start = 1, stop = 6),"(topright/NE)"
        ) # Boundary Coordinates
    )
    
#    observe(if (input$map1_zoom > 8) {
    observeEvent(input$map1_zoom, label="event20",{
        if ((v$msg3 > 8) & (!is.null(gis$slice2))) {
            label="event20"
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality', 'Ward'),
                selected = 'Ward'
            )
        }
    })
    
#    observe(if (input$map1_zoom <= 8) {
    observeEvent(input$map1_zoom ,label="event21", {
        if (v$msg3 <= 8) {#& (!is.null(v$msg3gis$slice1))) {
            label="event21"
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality'),
                selected = 'Municipality'
            )
        }
    })
    
    observe({#if (is.null(v$msg3)) {
        output$map1 <-renderLeaflet({
            leaflet() %>%
                setView(zoom=6,lng=26,lat=-29) %>%
                addTiles() %>%
                addLegend(
                    position = "bottomright", 
                    pal = town_binpal,
                    opacity = 1,
                    labFormat = labelFormat(big.mark = " "),
                    values = town_density
                ) 
        })
    })
    
    observe(if (input$enable_hover == TRUE){
        
        observeEvent(input$select_map_level,{
            label = 'proxy_map_event'
            proxy <- leafletProxy(
                "map1"
            )
            proxy %>%
                clearShapes() %>%
                clearTopoJSON() %>%
#            clearGroup('gis_tj') %>%
#            clearGroup('gis_shp') %>%
                addTopoJSON(
                    group = 'gis_tj',
                    stroke=T,dashArray=3,weight=2,color="white", topojson = gis$tj, #topojson = province_tj,
                    opacity=1,fill=T,smoothFactor = 0.5
                )
        })
        observe(if(input$enable_hover == TRUE & input$select_map_level == 'Ward'){ label="event19b"
        proxy <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
#            clearGroup('gis_shp') %>%
            addPolygons(
                group='gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.5,
                fillColor = gis$binpal(gis$shp@data$DENSITY)
            ) 
        })
        
        
        observeEvent(gis$shp_mouseover_id,label="event22", {
            if (input$select_map_level == 'Ward') {
                proxy <- leafletProxy(
                    "map1", data = subset(
                        gis$shp, 
                        gis$shp@data$ID == gis$shp_mouseover_id
                    )
                )
            proxy %>%
                clearGroup('single') %>%
#                clearGroup('gis_shp') %>%
                addPolygons(group = 'single',
                            stroke=T,weight=3,color="#555555",opacity=1,
                            smoothFactor=1,fill=F
                )
            }
        })
        
        observeEvent(input$map1_topojson_mouseover,label="event23", {
            if (input$select_map_level == 'Municipality') {
                proxy <- leafletProxy(
                    "map1", data = subset(
                        town_tj_spd,
                        town_tj_spd@data$MAP_TITLE == gis$slice2
                    )
                )
                proxy %>%
                    clearGroup('single') %>%
#                    hideGroup('town_slice') %>%
                    addPolygons(
                        group = 'single',
                        stroke=T,weight=3,color="#555555",opacity=1,
                        smoothFactor=1,fill = F
                    )
            }
        })
        
        observeEvent(input$map1_topojson_mouseout,label="event24", {
            proxy <- leafletProxy(mapId = 'map1')
            proxy %>%
                clearGroup('single')
        })
        
        observeEvent(input$map1_topojson_mouseover,label="event25", {
            if (input$select_map_level == 'Province') {
                proxy <- leafletProxy(
                    "map1", data = subset(
                        province_tj_spd,
                        province_tj_spd@data$PROVINCE == gis$slice1)
                )
                proxy %>%
                    clearGroup('single') %>%
                    addPolygons(
                        group = 'single',
                        stroke=T,weight=3,color="#555555",opacity=1,
                        smoothFactor=1,fill = F,fillOpacity = 0
                    )
            }
        })
    })
    
#    observe(if (input$enable_hover == FALSE){
    observe(if (input$enable_hover==FALSE & input$select_map_level == 'Province') {
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
#            clearGroup('gis_tj') %>%
#            clearGroup('gis_shp') %>%
            addPolygons(
#                group = 'gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.7,
                fillColor = gis$binpal(gis$shp@data$DENSITY),
                popup = ~paste0(gis$shp@data$PROVINCE,
                                "<br />",
                                "<strong>Density: </strong>",
                                round(gis$shp@data$DENSITY, digits = 1),
                                " people/km",
                                "<sup>2</sup>"),
                options = popupOptions()
            )
    })
    observe(if (input$enable_hover==FALSE & input$select_map_level == 'Municipality') {
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
#            clearGroup('gis_tj') %>%
#            clearGroup('gis_shp') %>%
            addPolygons(
#                group = 'gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.7,
                fillColor = gis$binpal(gis$shp@data$DENSITY),
                popup = ~paste0(gis$shp@data$MUNICNAME,
                                "<br />",
                                "<strong>Density: </strong>",
                                round(gis$shp@data$DENSITY, digits = 1),
                                " people/km",
                                "<sup>2</sup>"),
                options = popupOptions()
            )
    })
    observe(if (input$enable_hover == FALSE & input$select_map_level == 'Ward') {
        label = 'proxy_map_event'
        proxy <- leafletProxy(
            "map1"
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
#                clearGroup('gis_tj') %>%
#                clearGroup('gis_shp') %>%
            addTopoJSON(
#               group = 'gis_tj',
                stroke=T,dashArray=3,weight=2,color="white", topojson = gis$tj,
                opacity=1,fill=T,smoothFactor = 0.5
            )
    })
    
    observe(if (input$enable_hover == FALSE & gis$mouse_events == 1 & input$select_map_level == 'Ward'){
        label="event19b"
        proxy <- leafletProxy(
            "map1", data = gis$shp
        )
        proxy %>%
            clearShapes() %>%
#                clearGroup('gis_shp') %>%
            addPolygons(
#                    group='gis_shp',
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.5,
                fillColor = gis$binpal(gis$shp@data$DENSITY),
                popup = ~paste0(gis$shp@data$MUNICNAME,
                                "<br />",
                                "<strong> Ward </strong>",
                                gis$shp@data$WARDNO,
                                "<br />",
                                "<strong>Density: </strong>",
                                round(gis$shp@data$DENSITY, digits = 1),
                                " people/km",
                                "<sup>2</sup>"),
                options = popupOptions()
            )
    })
    
#      observe(if( input$enable_hover == FALSE){
#        observeEvent(gis$shp_mouseover_id,label="event22", {
#          proxy <- leafletProxy(
#            "map1", data = subset(gis$shp,gis$shp@data$ID == input$map1_shape_mouseover$id)
#          )
#          proxy %>%
#            clearGroup('single') %>%
# #           clearGroup('gis_shp') %>%
#            addPolygons(group = 'single',
#                        stroke=T,weight=3,color="#555555",opacity=1,
#                        smoothFactor=1,fill=F
#            )
#        })
#      })
    
    output$hoverInfo <- renderUI({
        if (is.null(gis$shp_mouseover_id) & gis$mouse_events == 0) {
            return(
                div(
                    paste("Hover over a", input$select_map_level)
                ))
        } else if (input$select_map_level == 'Ward'){
            return(
                div(
                    strong(
                        sub(
                            pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                            replacement="",
                            x=gis$shp@data$MUNICNAME[gis$shp@data$ID == gis$shp_mouseover_id])),
                    br(),
                    "Ward",
                    gis$shp@data$WARDNO[gis$shp@data$ID == gis$shp_mouseover_id],
                    br(),
                    span(round(gis$shp@data$DENSITY[gis$shp@data$ID == gis$shp_mouseover_id],1), HTML("people/km<sup>2</sup>"))
                )
            )
        } else if (input$select_map_level == 'Municipality'){
            return(
                div(
                    strong(gis$id),
                    br(),
                    span(round(gis$shp@data$DENSITY[gis$shp@data$MAP_TITLE == gis$id],1), HTML("people/km<sup>2</sup>"))
                )
            )
        } else if (input$select_map_level == 'Province'){
            return(
                div(
                    strong(gis$id),
                    br(),
                    span(round(gis$shp@data$DENSITY[gis$shp@data$PROVINCE == gis$id],1), HTML("people/km<sup>2</sup>"))
                )
            )
        }
    })
    
#    observe({
#        if (input$select_data_level=='Ward') {
            output$table_ward <- DT::renderDataTable(
                ward_tj_list$objects$Wards2011$geometries$properties, server=T,
                options=list(pageLength = 50)
            )
#        }
#    })
#    observe({
#        if (input$select_data_level=='Municipal') {
            output$table_town <- DT::renderDataTable(
                town_tj_list$objects$LocalMunicipalities2011$geometries$properties,
                server=T,options=list(pageLength=50)
            )
#        }
#    })
#    observe({
#        if (input$select_data_level=='Provincial') {
            output$table_province <- DT::renderDataTable(
                province_tj_list$objects$Province_New_SANeighbours$geometries$properties,
                server=T
            )
#        }
#    })
}

shinyApp(ui,server)
