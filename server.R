server <- function(input, output, session) {
  #TEMPERATURE TAB =========================================================================================================
  #VALUE BOX OF DAY OVER 30 DEGREES BY YEAR --------------------------------------------------------------------------------
  output$DaysOver30<- renderValueBox({
    
    df_temp_filtered <- df_temp %>%
      filter(year == input$year_temp, station_name == input$station) %>%
      filter(max_temp >= 30)
    
    valueBox(
      value = count(df_temp_filtered),
      subtitle = paste("Days ≥ 30°C for ", input$station, " ,", input$year_temp),
      icon = icon("temperature-arrow-up"),
      color = "orange"
    )
  })
  
  #YOY CHANGE IN DAYS OVER 30  --------------------------------------------------------------------------------------------
  output$YoYDaysOver30 <- renderValueBox({
    
    selected_year <- as.numeric(input$year_temp)

    current_period <- df_temp %>%
      filter(year == input$year_temp, station_name == input$station) %>%
      filter(max_temp >= 30)

    
    prev_period <- df_temp%>%
      filter(year == selected_year -1, station_name == input$station) %>%
      filter(max_temp >= 30)
    
    
    if(nrow(prev_period) == 0 || nrow(current_period) == 0){
      valueBox(
        value = icon("minus"),
        subtitle = paste0("No data for ", input$year),
        icon = icon("xmark"),
        color = "teal"
      )
    } else {
      YoY_growth <- ((count(current_period) - count(prev_period)) / count(prev_period)) * 100
      
      valueBox(
        value = if (YoY_growth > 0) paste0("+",round(YoY_growth, digits = 2), "%") else paste0(round(YoY_growth, digits = 2), "%"),
        subtitle = paste0(
          if (YoY_growth > 0) "Days ≥ 30°C up" else if (YoY_growth == 0) "No change in days ≥ 30°C" else "Days ≥ 30°C down",
          " from ", selected_year - 1, " - ", input$station
        ),
        icon = if (YoY_growth > 0) icon("arrow-up") else if (YoY_growth == 0) icon("minus") else icon("arrow-down"),
        color = "teal"
      )
    }
  })
  
  #VALUE BOX CHANGE IN VOLUME PROCESSED PER PERSON FROM 2013 TO CURRENT----------------------------------------------
  output$ChangeSince2013<- renderValueBox({
    
    annual_heat_days <- df_temp %>%
      filter(
        station_name == input$station,
        year >= 2013
      ) %>%
      group_by(year) %>%
      summarise(
        Heat_Days = sum(max_temp >= 30, na.rm = TRUE),
        .groups = "drop"
      )
    
    model <- lm(Heat_Days ~ year, data = annual_heat_days)
    
    # Annual trend
    trend <- coef(model)[2]
      
      valueBox(
        value = if (trend >= 0) paste0("+", round(trend, 2), " days/year") else paste0(round(trend, 2), " days/year"),
        subtitle = paste0("Annual increase in days ≥ 30°C, ", min(annual_heat_days$year), "–", max(annual_heat_days$year), " - ", input$station),
        icon = if (trend > 0) icon("arrow-up") else if (trend == 0) icon("minus") else icon("arrow-down"),
        color = "orange"
      )
  })
  
  #MONLTHY TEMP OF SELECTED YEAR AND STATION -------------------------------------------------------------
  output$MonthlyTempPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_MonthlyTempPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_temp_filtered <- df_temp%>%
      filter(year == input$year_temp, station_name == input$station) %>%
      mutate(month = as.integer(month)) %>%
      group_by(month) %>%
      summarise(
        Heat_Days = sum(max_temp >= 30, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(
        month = 1:12,
        fill = list(Heat_Days = 0)
      )
    
    plot <- ggplot(df_temp_filtered,aes(x = month,
                                        y = Heat_Days,
                                        text = paste0(
                                          "<b>", month.abb[month], "</b><br>",
                                          "<b>Days ≥ 30°C:</b> ", Heat_Days
                                        )
                                      )
      ) +
      
      geom_col(fill = "#76ACA9", width = 0.7) +
      
      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb,
        limits = c(0.5, 12.5),
        expand = c(0, 0)
      ) +
      
      scale_y_continuous(
        breaks = scales::pretty_breaks(),
        expand = expansion(mult = c(0, 0.1))
      ) +
    
      labs(
        x = "Month", 
        y = "Number of Days ≥ 30°C"
      ) +
      
      theme(axis.title = element_text(size = if (is_narrow) 10 else 16),
            axis.text = element_text(size = if (is_narrow) 8 else 12),
            axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  #DYNAMIC YEAR BOX FOR MONTHLY TEMP PLOT ---------------------------------------------------------------------------------
  output$MonthlyTempPlotBox <- renderUI({
    box(
      title = paste0("Monthly Number of Days ≥ 30°C - ", input$station, ", ", input$year_temp),
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("MonthlyTempPlot", height = 500)
    )
  })
  
  #ALL STATIONS 2013-CURRENT NUMBER OF HOT DAYS EACH YEAR ---------------------------------------------------------------------
  output$AllCitiesPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_AllCitiesPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_hot_days <- df_temp %>%
      filter(!is.na(max_temp)) %>%
      group_by(station_name, year) %>%
      summarise(
        days = sum(max_temp >= 30),
        .groups = "drop"
      ) %>%
      complete(
        station_name,
        year = seq(min(year), max(year), by = 1),
        fill = list(days = 0)
      )
    
   plot <-  ggplot(df_hot_days, aes(x = year, y = days, fill = days, text = paste0("<b>Hot Days: </b>", days))) +
      
      geom_col(width = 0.3) +
      
      facet_wrap(
        ~ station_name,
        ncol = 1,
        scales = "free_y",
        strip.position = "top"
      )+
      
      scale_fill_gradientn(
        colours = c("#FDE6A2", "#F5B66B", "#E9784F", "#C92332"),
        name = "Days ≥ 30°C"
      ) +
      
      scale_x_continuous(
        breaks = if (is_narrow) seq(min(df_hot_days$year), max(df_hot_days$year), by = 4) else seq(min(df_hot_days$year), max(df_hot_days$year), by = 1)
      ) +
     
     scale_y_continuous(
       expand = expansion(mult = c(0, 0.1)),
       breaks = seq(0, max(df_hot_days$days), by = 20)
     )+
      
      labs(
        x = "Year",
        y = "Number of Days"
      ) +
      
      theme_minimal() +
      
      theme(
        strip.background = element_rect(fill = "grey90", colour = "grey70"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        legend.position = if (is_narrow) "bottom" else "right",
        legend.title = element_text(size = if (is_narrow) 8 else 12),
        legend.text = element_text(size = if (is_narrow) 8 else 10),
        axis.title = element_text(size = if (is_narrow) 8 else 14),
        axis.text = element_text(size = if (is_narrow) 8 else 12),
        axis.text.x = element_text(angle = if (is_narrow) 30 else 0)
      )
   
   ggplotly(plot, tooltip = "text") %>%
     layout(
       margin = list(t = 30, r = 0, b = 50, l = 70),
       hoverlabel = list(
         bgcolor = "#004B55",
         bordercolor = "#00363D",
         font = list(
           color = "white",
           size = 13,
           family = "Arial"
         ),
         align = "left"
       )
     ) %>%
     config(displayModeBar = FALSE)
  })
  
  #WILDFIRE TAB ============================================================================================================
  #VALUE BOX OF TOTAL WILDFIRES BY YEAR  -----------------------------------------------------------------------------------
  output$NumberFires <- renderValueBox({
    
    df_fire_filtered <- df_wildfires %>%
      filter(WILDFIRE_YEAR == input$year_fire)
    
    valueBox(
      value = paste0(length(unique(df_fire_filtered$`Fire Number`))),
      subtitle = paste0("Total wildfires for ", input$year_fire, " season"),
      icon = icon("fire"),
      color = "orange"
    )
  })
  
  #VALUE BOX OF TOTAL HECTARS BURNED BY YEAR  ------------------------------------------------------------------------------
  output$TotalHectares <- renderValueBox({
    
    total <- df_wildfires %>%
      filter(WILDFIRE_YEAR == input$year_fire) %>%
      summarise(total = sum(FIRE_SIZE_HA, na.rm = TRUE)) %>%
      pull(total)
    
    valueBox(
      value = paste0(format(round(total), big.mark = ","), " Hectares"),
      subtitle = paste("Total recorded fire size for", input$year_fire, " season"),
      icon = icon("ruler-horizontal"),
      color = "teal"
    )
  })
  
  #VALUE BOX OF LARGEST FIRE BY YEAR  --------------------------------------------------------------------------------------
  output$LargestFire <- renderValueBox({
    
    df_wildfires_filtered <- df_wildfires %>%
      filter(WILDFIRE_YEAR == input$year_fire) %>%
      filter(FIRE_SIZE_HA == max(FIRE_SIZE_HA))
    
    valueBox(
      value = paste0(format(round(df_wildfires_filtered$FIRE_SIZE_HA), big.mark = ","), " Hectares"),
      subtitle = paste("Largest wildfire —", df_wildfires_filtered$GEOGRAPHIC, "for ", input$year_fire),
      icon = icon("fire-flame-curved"),
      color = "orange"
    )
  })
  
  #VALUE BOX OF TOTAL FIRES OF NOTE BY YEAR  -------------------------------------------------------------------------------
  output$FiresOfNote <- renderValueBox({
    
    total <- df_wildfires %>%
      filter(WILDFIRE_YEAR == input$year_fire, WAS_FIRE_OF_NOTE_IND == "Y") 
    
    valueBox(
      value = nrow(total),
      subtitle = paste("Wildfires of note for ", input$year_fire, " season"),
      icon = icon("hashtag"),
      color = "teal"
    )
  })
  
  #MAP OF FIRES FILTERED BY YEAR ---------------------------------------------------------------------------------------
  output$fireMapPlot <- renderLeaflet({
    
    plot_width <- session$clientData$output_FireMapPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    operation_colors <- c(
      "Wildfire" = "#004B55",
      "Wildfire of Note" = "#F6BC1A",
      "Thompson Okanagan Tourism Region" = "#D11B4A"
    )
    
    df_fire_filtered <- df_wildfires %>%
      filter(WILDFIRE_YEAR == input$year_fire)
    
    m_base <- leaflet(data = df_TO_boundary)
    m_tiles <- addTiles(m_base)
    m_bound <- addPolygons(m_tiles,
                           data = df_TO_boundary, #add Thompson Okanagan region bounds
                           fill = FALSE,
                           color = "#D11B4A",
                           weight = 2,
                           opacity = 1
    )
    
    m_markers <- addCircleMarkers(m_bound,
                                    data = df_fire_filtered,
                                    lng = ~LONGITUDE,
                                    lat = ~LATITUDE,
                                    radius = 6,
                                    color = "#000",
                                    weight = 1,
                                    fillColor = ~ifelse(WAS_FIRE_OF_NOTE_IND == "Y", "#F6BC1A", "#004B55"),
                                    stroke = TRUE,
                                    fillOpacity = 0.9,
                                    label = ~paste0(GEOGRAPHIC),
                                    popup = ~paste0("<b>", GEOGRAPHIC,"</b><br>",
                                                    "<b>Ignited: </b>", format(`IGNITION DATE`, "%b %d, %Y"), "<br>",
                                                    "<b>Fire Size (ha): </b>", format(FIRE_SIZE_HA, big.mark = ","))
                                    
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(operation_colors),
        labels = names(operation_colors),
        title = "",
        opacity = 1
      )

    setView(m_markers, lng = -118.196086, lat = 50.998195, zoom = 6)
  })
  
  #DYNAMIC OUTPUT BOX FOR FIRE MAP PLOT -----------------------------------------------------------------------------
  output$fireMapPlotBox <- renderUI({
    box(
      title = paste0("Wildfire map - ", input$year_fire),
      solidHeader = TRUE,
      width = 12,
      leafletOutput("fireMapPlot", height = 500)
    )
  })
  
  #MONTHLY FIRE START/SEASONAL TIMING LINE CHART ---------------------------------------------------------------------
  output$MonthlyFirePlot <- renderPlotly({
    
    plot_width <- session$clientData$output_MonthlyFirePlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    fire_months <- c(
      "Apr", "May", "Jun", "Jul", "Aug", "Sep",
      "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"
    )
    
    df <-   df_wildfires %>%
      filter(WILDFIRE_YEAR == input$year_fire) %>%
      mutate(
        IgnitionDate = as.Date(`IGNITION DATE`),
        Month = format(IgnitionDate, "%b")
      ) %>%
      filter(!is.na(IgnitionDate)) %>%
      count(Month) %>%
      tidyr::complete(
        Month = fire_months,
        fill = list(n = 0)
      ) %>%
      mutate(Month = factor(Month, levels = fire_months))
    
    plot <- ggplot(df, aes(x = Month, 
                           y = n,
                           text = paste0("<b>" , Month, "</b><br>",
                                         "<b>Wildfires Ignited: ", n),
                           group = 1)) + 
      
      geom_line(color = "#F6BC1A", linewidth = 1) +
      
      scale_x_discrete(
        limits = fire_months,
        drop = FALSE,
        expand = c(0, 0)
      ) +
      
      labs(
        x = "Month", 
        y = "Number of Wildfires"
      ) +
      
      theme(axis.title = element_text(size = if (is_narrow) 10 else 16),
            axis.text = element_text(size = if (is_narrow) 8 else 12),
            axis.text.x = element_text(angle = if (is_narrow) 60 else 0))

    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        # margin = list(t = 30, r = 0, b = 50, l = 70),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #DYNAMIC OUTPUT BOX FOR MONTHLY FIRE PLOT ----------------------------------------------------------------------------
  output$MonthlyFirePlotBox <- renderUI({
    box(
      title = paste0("Seasonal Timing of Wildfire Activity - ", input$year_fire),
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("MonthlyFirePlot", height = 500)
    )
  })
  
  #DROUGHT TAB ========================================================================================================= 
  #VALUE BOX OF HIGHEST DROUGHT LEVEL THIS WEEK ----------------------------------------------------------------------
  output$CurrentHighest <- renderValueBox({
    
    valueBox(
      value = max(df_drought$DroughtLevel),
      subtitle = paste0("Highest drought level for latest reporting: ", format(max(df_drought$Date_Modified), "%b %m, %Y")),
      icon = icon("hand-holding-droplet"),
      color = "orange"
    )
  })
  
  #VALUE BOX OF BASINS EXPERIENCING SEVERE DROUGHT -------------------------------------------------------------------
  output$Level3 <- renderValueBox({
    
    df_drought_hist_filtered <- df_drought_hist %>%
      filter(DroughtLevel >= 3, DroughtLevel < 6)
    
    valueBox(
      value = paste0(length(unique(df_drought_hist_filtered$BasinName)), "/", length(unique(df_drought_hist$BasinName))),
      subtitle = paste0("Basins reaching drought level 3 or higher for ", format(mean.Date(df_drought_hist_filtered$Start_Date), "%Y")),
      icon = icon("sun-plant-wilt"),
      color = "teal"
    )
  })
  
  #VALUE BOX OF HIGHEST DROUGHT LEVEL REACHED -----------------------------------------------------------------------
  output$HighestLevel <- renderValueBox({
    
    df_drought_hist_filtered <- df_drought_hist %>%
      filter(DroughtLevel < 6)
    
    valueBox(
      value = max(df_drought_hist_filtered$DroughtLevel),
      subtitle = paste0("Highest drought level reached for ", format(mean.Date(df_drought_hist_filtered$Start_Date), "%Y")),
      icon = icon("droplet-slash"),
      color = "orange"
    )
  })
  
  #DROUGHT MAP OF THOMPSON OKANAGAN REGION ---------------------------------------------------------------------------
  output$DroughtPlot <- renderLeaflet({
    
    pal <- colorFactor(
      palette = c(
        "#E5EFBD",  # 0
        "#F0E918",  # 1
        "#F5C361",  # 2
        "#E38A2A",  # 3
        "#D71B22",  # 4
        "#800A0C"   # 5
      ),
      domain = c(0, 1, 2, 3, 4, 5)
    )
    
    m_base <- leaflet(data = df_drought)
    
    m_tiles <- addTiles(m_base)
    
    m_ploy <-  addPolygons(
      m_tiles,
      fillColor = ~pal(DroughtLevel),
      fillOpacity = 1,
      weight = 1.5,
      color = "black",
      popup = ~paste0(
        "<b>", BasinName, "</b><br>",
        "Current Drought Level: ", DroughtLevel
      )
    ) %>%      
      addPolygons( # Add tourism region
        data = df_TO_boundary,
        fill = FALSE,
        color = "#004B55",
        weight = 2,
        opacity = 1
      )
    
    m_legend <- addLegend(
      map = m_ploy,
      position = "bottomright",
      colors = "#004B55",
      labels = "Thompson Okanagan Tourism Region",
      opacity = 1
    )%>%
      addLegend(
        "bottomright",
        pal = pal,
        values = c(0, 1, 2, 3, 4, 5),
        title = "Drought Level",
        opacity = 1
      )
    
    setView(m_legend, lng = -120.34, lat = 50.98, zoom = 6)
  })
  
  #DROUGHT TABLE -----------------------------------------------------------------------------------------------------
  output$DroughtTable <- renderUI({
    
    tags$table(
      class = "table table-striped table-sm",
      tags$thead(
        tags$tr(
          tags$th("Basin"),
          tags$th("Drought Level"),
          tags$th("Latest Update")
        )
      ),
      
      tags$tbody(
        lapply(seq_len(nrow(df_drought)), function(i) {
          
          tags$tr(
            tags$td(df_drought$BasinName[i]),
            tags$td(df_drought$DroughtLevel[i]),
            tags$td(format(df_drought$Date_Modified[i], "%b %m, %Y"))
            
          )
        })
      )
    )
  })
  
  #DROUGHT HISTORY HEATMAP TABLE -- Data needs to be manually updated in the google sheet ----------------------------
  output$DroughtHistPlot <- renderPlot({
    
    plot_width <- session$clientData$output_DroughtHistPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df <- df_drought_hist %>%
      mutate(
        Date_Label = format(Start_Date, "%d-%b")
      )
    
    df$BasinName <- factor(
      df$BasinName,
      levels = rev(unique(df$BasinName))
    )
    
    # Keep dates in chronological order
    df$Date_Label <- factor(
      df$Date_Label,
      levels = unique(df$Date_Label)
    )
    
    ggplot(
      df,
      aes(
        x = Date_Label,
        y = BasinName,
        fill = factor(DroughtLevel)
      )
    ) +
      
      # Heatmap cells
      geom_tile(
        color = "black",
        linewidth = 0.25
      ) +
      
      # Numbers inside cells
      geom_text(
        aes(
          label = ifelse(
            DroughtLevel == 6,
            "*",
            DroughtLevel
          )
        ),
        size = if (is_narrow) 0 else 6
      ) +
      
      # Drought colours
      scale_fill_manual(
        values = c(
          "0" = "#E5EFBD",
          "1" = "#F0E918",
          "2" = "#F5C361",
          "3" = "#E38A2A",
          "4" = "#D71B22",
          "5" = "#800A0C",
          "6" = "#BDBDBD" #NA
        ),
        
        labels = c(
          "0" = "0",
          "1" = "1",
          "2" = "2",
          "3" = "3",
          "4" = "4",
          "5" = "5",
          "6" = "Not updated"
        ),
        name = "Drought Levels"
      ) +
      
      labs(
        x = "",
        y = ""
      )+
      
      theme_minimal() +
      
      scale_x_discrete(
        position = "top",
        expand = c(0, 0),
        guide = guide_axis(
          angle = if (is_narrow) 90 else 45,
          check.overlap = TRUE
        )
      ) +
      
      scale_y_discrete(expand = c(0, 0)) +
      
      theme(
        axis.text.x = element_text(
          hjust = 0.5,
          size = if (is_narrow) 10 else 16
        ),
        
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        
        axis.text.y = element_text(
          size = if (is_narrow) 10 else 14,
          hjust = 1
        ),
        
        panel.grid = element_blank(),
        
        legend.position = if (is_narrow) "bottom" else "top",
        legend.direction = "horizontal",
        legend.location = "plot",
        legend.justification = "left",
        plot.margin = margin(t = 5, r = 10, b = 5, l = 0),
        
        legend.title = element_text(
          size = if (is_narrow) 8 else 16
        ),
        
        legend.text = element_text(
          size = if (is_narrow) 8 else 14
        )
      ) 
  })
  
  #OVERVIEW PAGE =======================================================================================================
  
  #CLICKABLE LINK FOR DISPOSAL RATE TAB --------------------------------------------------------------------------------
  observeEvent(input$drought_link, {
    updateTabItems(session, "tabs", "drought")
  })
  
  observeEvent(input$fire_link, {
    updateTabItems(session, "tabs", "fire")
  })
  
  observeEvent(input$temp_link, {
    updateTabItems(session, "tabs", "temperature")
  })

}
