#UI FOR CLIMATE ACTION SHINY DASHBOARD============================================================================
#THEME -----------------------------------------------------------------------------------------------------------
TOTA_theme <- create_theme( #Set colours
  adminlte_color(
    light_blue = "#004B55",
    yellow = "#BCB49E",
    orange = "#F6BC1A",
    teal = "#76ACA9",
    fuchsia = "#D11B4A"
  )
)

#FOOTER ---------------------------------------------------------------------------------------------------------
footer_TOTA <- tags$footer( 
  tags$style(HTML("
    @media (max-width: 600px) {
      #footer-logo img {
        min-width: 60px;
      }
      #footer-text p {
        font-size: 12px !important;
      }
      #footer-credit p {
        font-size: 11px !important;
      }
    }
  ")),
  div(
    div(
      tags$img(
        src = "side_bar_logo.png",
        width = "80%",
        style = "display: block; margin: 0 auto;"
      ),
      id = "footer-logo",
      style = "width: 15%; display: flex; align-items: center; justify-content: center;"
    ),
    
    div(
      p("Thompson Okanagan Tourism Association",
        style = "font-weight: bold; margin-bottom: 5px;"
      ),
      
      tags$a(
        "www.totabc.org",
        href = "https://www.totabc.org/",
        target = "_blank",
        style ="color: white;"
      ),
      
      p("2280-D Leckie Road, Kelowna,",
        style = "margin-bottom: 2px;"
      ),
      
      p("British Columbia, V1X 6G6",
        style = "margin-bottom: 0;"
      ),
      
      id = "footer-text",
      style = "width: 60%; color: white; display: flex; flex-direction: column; justify-content: center; padding-left: 5%;"
    ),
    
    div(
      tags$p(
        tags$a(
          "Created by Alexis Samp",
          href = "https://ca.linkedin.com/in/alexis-samp-b89678342",
          target = "_blank",
          style ="color: white;"
        ),
        style = "color: white; width: 100%; font-size: 14px; text-align: center; text-decoration: underline;"
      ),
      id = "footer-credit"
    ),
    style = "width: 100%; background-color: #004B55; display: flex; align-items: center; padding: 20px 5%; box-sizing: border-box; color: white;"
  ),
  style = "position: relative; bottom: 0; left: 0; width: 100%; z-index: 9999;"
)

#HEADER ----------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Climate Action Dashboard",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://tota-insto-hub.share.connect.posit.cloud/",
        icon("globe"),
        " TOTA INSTO HUB",
        style = "
        color: white;
        font-size: 18px;
        padding: 15px;
        text-decoration: underline;",
      )
    )             
  ),
#SIDE BAR / NAV --------------------------------------------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("readme")),
      # menuItem("Ev Charging", tabName = "ev", icon = icon("charging-station")),
      menuItem("Temperature", tabName= "temperature", icon = icon("temperature-high")),
      menuItem("Wildfires", tabName = "fire", icon = icon("fire-flame-curved")),
      menuItem("Drought", tabName = "drought", icon = icon("droplet-slash")),
      menuItem("Back to Hub", tabName = NULL, icon = icon("globe"), href = "https://tota-insto-hub.share.connect.posit.cloud/", newtab = FALSE)
    ),
    
    tags$img(
      src = "side_bar_logo.png",
      width = "85%",
      style = "display: block; margin: 0 auto; padding-top: 40px;"
    ),
    
    tags$img(
      src = "UN_side_bar_logo.png",
      width = "85%",
      style = "display: block; margin: 0 auto; padding-top: 40px;"
    )
  ),

#OVERVIEW TAB-----------------------------------------------------------------------------------------------------
  dashboardBody(
    use_theme(TOTA_theme),
    
    tabItems(
      
      tabItem(
        tabName = "overview",
        
        tags$img(
          src = "banner.png",
          width = "100%",
          style = "position: relative;"
        ),
        p("Destination BC/Andrew Strain"),
        
        fluidRow(
          width = "100%",
          
          column(
            width = 12,
            h2("About this Dashboard"),
            p("The purpose of this dashboard is to support TOTA's climate action reporting for the ",
              tags$a(
                href = "https://www.untourism.int/observatories/thompson-okanagan",
                "UN Tourism International Network of Sustainable Tourism Observatories (INSTO)",
                target = "_blank",
                style = "color: #76ACA9; text-decoration: underline;"
              ),
              " and contribute to ongoing efforts to better understand the relationship between
               climate change and tourism in the Thompson Okanagan Region."),
            br(),
            style = "font-size: 18px;"
          ),
          
          column(
            tags$head(tags$style(
              HTML(
                ".explore-link {
                    display: block;
                    font-size: 18px;
                    font-weight: 600;
                    margin-top: 10px;
                    margin-bottom: 10px;
                    color: #004B55;
                    text-decoration: none;
               }
               .explore-link:hover {
                  text-decoration: underline;
                  cursor: pointer;
               }"
              )
            )),
            
            width = 6,
            h2("What is Climate Action?"),
            
            p("Climate action involves measuring and reducing greenhouse gas emissions and strengthening the capacity to adapt to 
              climate-related impacts such as drought, extreme heat, wildfires, and flooding. In the Thompson-Okanagan, climate 
              action is particularly relevant to understanding how climate change affects tourism infrastructure, outdoor recreation,
              water availability, and the long-term resilience of tourism businesses and communities. Monitoring these indicators 
              supports informed decision-making and helps destinations identify opportunities to reduce emissions and build climate resilience."),
            br(),
            
            h2("What Can Be Explored?"),
            
            actionLink(
              "temp_link",
              strong("Temperature - How is temperature in the region changing over time?"),
              class = "explore-link"
            ),
            p("Explore changing temperature patterns and the number of hot days."),
            
            actionLink(
              "fire_link",
              strong("Wildfire - How many wildfires are happening in the region?"),
              class = "explore-link"
            ),
            p("Explore wildfires in the region and seasonal timing of wildfire activity."),
            
            actionLink(
              "drought_link",
              strong("Drought - How dry is the region?"),
              class = "explore-link"
            ),
            p("View current and historical drought levels of the region."),
            br()
          ),
          
          column(
            width = 6,
            tags$img(
              src = "region_map_bg.png",
              width = "70%",
              style = "display: block; margin: 0 auto;"
            ),
            p("The scope of the dashboard is the Thompson Okanagan Tourism Region,", br(), "British Columbia, Canada", style = "text-align: center; font-size: 14px;"),
          ),
          
          tags$style(
            HTML(
              ".explore-link {
                  color: #42817A;
                  font-size: 20px;
                  font-weight: bold;
                  text-decoration: none;
              }
              .explore-link:hover {
                  text-decoration: underline;
              }"
            )
          ),
          style = "width: 100%; padding: 20px; font-size: 18px;"
        )
      ), 
      
  #TEMPERATURE TAB -------------------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "temperature",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #MonthlyTempPlot {height: 350px !important; }
            #AllCititesPlot {height: 350px !important; }
          }"
        )),
        
        fluidRow(
          box(
            title = "Temperature - How is temperature in the region changing over time?",
            solidHeader = TRUE,
            width = 12,
            p("Tracking days above 30°C, provides insight into changing temperature patterns across the Thompson-Okanagan region.
              These conditions are important for understanding how climate change may affect tourism, particularly outdoor recreation, 
              hiking, camping, lake activities, and the comfort and safety of visitors. Prolonged periods of extreme heat can influence 
              visitor experiences and increase pressure on water resources."),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 12,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = paste0(format(max(df_temp$date), "%B %d, %Y")),
            subtitle = "Updated by Environment and Climate Change Canada."
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_temp",
                "Select Year:",
                choices = sort(unique(df_temp$year)),
                selected = max(df_temp$year)
              )
            )
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "station",
                "Select Station:",
                choices = sort(unique(df_temp$station_name)),
                selected = "Kelowna"
              )
            )
          ),
        
          valueBoxOutput("DaysOver30", width = 4),
          valueBoxOutput("YoYDaysOver30", width = 4),
          valueBoxOutput("ChangeSince2013", width = 4),
          
          uiOutput("MonthlyTempPlotBox"),
          
          box(
            title = "Annual Days ≥ 30°C",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("AllCitiesPlot", height = 600)
          ),
          
          box(
            title = "References & Data",
            solidHeader = TRUE,
            width = 12,
            p("Weather Stats. Kelowna Historical Number of Days with a Temperature ≥ 30°C. https://kelowna.weatherstats.ca/metrics/count_temp_30.html"),
            p("Data Source: Government of Canada. Environment and Climate Change Canada. https://climate.weather.gc.ca/historical_data/search_historic_data_e.html")
          )
        )
      ),

  #WILDFIRE TAB -------------------------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "fire",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #FireMapPlot { height: 350px !important; }
            #MonthlyFirePlot {height: 350px !important; }
          }"
        )),
        
        fluidRow(
          
          box(
            title = "Wildfire - How many wildfires are happening in the region?",
            solidHeader = TRUE,
            width = 12,
            p("Wildfires are an important indicator of climate-related risk and resilience within the Thompson-Okanagan Tourism Region. 
              This dashboard brings together wildfire data to help monitor the frequency, timing, and extent of wildfire activity across the region.
              Wildfires can affect tourism through smoke, evacuations, infrastructure damage, road closures, and disruptions to outdoor recreation 
              and visitor experiences. By tracking wildfire activity, the dashboard provides valuable context for understanding 
              climate-related pressures on tourism destinations and communities, supporting informed decision-making, emergency preparedness, and long-term tourism resilience."),
            p(),
            h3("Learn More"),
            p(),
            
            p("Explore the current state of wildfires in the province."),
            tags$a(
              href = "https://wildfiresituation.nrs.gov.bc.ca/dashboard",
              tags$button(
                type = "button",
                "B.C. Wildfire Dashboard"
              )
            ),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "",
            value = "",
            width = 12,
            color = "light-blue",
            icon = icon("info-circle"),
            subtitle = "B.C.'s wildfire year officially starts on April 1 and runs through March 31 of the following calendar year. 
              Wildfires in B.C. can burn across multiple years, including through the winter. These are called overwintering fires. 
              For example, a fire might ignite in summer 2025, smoulder underground through winter, and become active again in 2026."
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_fire",
                "Select Wildfire Year:",
              choices = sort(unique(df_wildfires$WILDFIRE_YEAR)),
              selected = max(df_wildfires$WILDFIRE_YEAR)
              )
            )
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 6,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = "Data is updated every April by BC Wildfire Service."
          ),
          
          valueBoxOutput("NumberFires", width = 3), 
          valueBoxOutput("TotalHectares", width = 3),
          valueBoxOutput("LargestFire", width =3),
          valueBoxOutput("FiresOfNote", width = 3),
          
          uiOutput("fireMapPlotBox"),
          uiOutput("MonthlyFirePlotBox"),
          
          box(
            title = "References & Data",
            solidHeader = TRUE,
            width = 12,
            p("Data source: Government of British Columbia. BC Wildfire Fire Incident Locations - Historical.
                Retrieved from https://catalogue.data.gov.bc.ca/dataset/e2dadc60-292f-4d98-b42b-56ca9e4fe694"),
            p("Government of British Columbia. B.C. Wildfire Dashboard. https://wildfiresituation.nrs.gov.bc.ca/dashboard")
          )
        )
      ),
  
  #DROUGHT TAB ---------------------------------------------------------------------------------------------      
      tabItem(
        tabName = "drought",
        
        tags$style(HTML("
              @media (max-width: 600px) {
                #DroughtPlot { height: 350px !important; }
                #DroughtHistPlot {height: 350px !important; }
              }"
        )),
        
        fluidRow(
          box(
            title = "Drought - How dry is the region?",
            solidHeader = TRUE,
            width = 12,
            p("This dashboard brings together current and historical drought data from basins across the Thompson-Okanagan Tourism Region 
                  to support climate action and regional resilience. Drought conditions provide insight into changing water availability and 
                  the potential impacts of climate-related pressures on communities, ecosystems, and tourism. The B.C. Drought Level classification 
                  combines information about water supply and environmental conditions to indicate drought severity, helping identify emerging risks 
                  and inform sustainable water management and climate adaptation planning."),
            style = "font-size: 18px;"
          ),
          
          valueBoxOutput("CurrentHighest", width =4),
          valueBoxOutput("Level3", width = 4),
          valueBoxOutput("HighestLevel", width = 4),
          
          infoBox(
            title = "Data Freshness",
            width = 12,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = format(max(df_drought$Date_Modified), "%b %d, %Y"),
            subtitle = "Data is updated throughout the drought season by GeoBC."
          ),
          
          box(
            title = "Current Drought Levels within the Thompson Okanagan Tourism Region",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("DroughtPlot", height = 500)
          ),
          
          box(
            title = "Drought Scale",
            solidHeader = TRUE,
            width = 12,
            div(
              style = "text-align: center;",
              tags$img(
                src = "drought_scale.png",
                style = "width: 75%;"
              )
            ),
            footer = ("Government of British Columbia. B.C. Drought Information Portal. Available at: https://droughtportal.gov.bc.ca/")
          ),
          
          box(
            title = paste0(
              max(format(df_drought_hist$Start_Date, "%Y"), na.rm = TRUE),
              " Drought Levels at a Glance"
            ),
            solidHeader = TRUE,
            width = 12,
            plotOutput("DroughtHistPlot", height = 500),
            footer = "Updated manually by TOTA."
          ),
          
          box(
            title = "References & Data",
            solidHeader = TRUE,
            width = 12,
            p("Data source: Province of British Columbia. B.C. Drought Information Portal (British Columbia Drought Levels).
                  Retrieved from https://droughtportal.gov.bc.ca/datasets/f1842161d9c2454a98f9fc3b45d5d92e_27/explore?location=54.149730%2C-126.557374%2C5"),
            p("Data source: Government of British Columbia. BC Drought Levels Time Lapse 2025. Drought Information Portal. Retrieved from https://drought-information-portal-bcgov03.hub.arcgis.com/pages/historical-drought-levels")
          )
        )
      )
    ),
    footer_TOTA
  )
)