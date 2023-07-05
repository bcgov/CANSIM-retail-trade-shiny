# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


## load libraries  ----
library(tidyverse)
library(shiny)
library(rsconnect)
library(plotly)
library(lubridate)
library(janitor)
library(cansim)

## static vector names ----
province_names <- data.frame(
  geo = c("Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick",
          "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta",  "British Columbia"),
  vector = c("v1446859483", "v1446859543", "v1446859574", "v1446859605", "v1446859636",
             "v1446859667", "v1446859789", "v1446859881", "v1446859942", "v1446859973", "v1446860064"))

sector_names <-   data.frame(
  sector = c("All Retail Trade",
             "Motor vehicle and parts dealers",
             "Furniture, home furnishings stores, electronics and appliances retailers",
             "Building material and garden equipment and supplies dealers",
             "Food and beverage retailers",
             "Health and personal care retailers",
             "Gasoline stations and fuel vendors",
             "Clothing and clothing accessories retailers",
             "Sporting goods, hobby, musical instrument, book retailers and news dealers",
             "General merchandise retailers",
             "Miscellaneous retailers"),
  vector = c("v1446860063", "v1446860065", "v1446860078", "v1446860071", "v1446860072", "v1446860084",
             "v1446860085", "v1446860087", "v1446860091", "v1446860083", "v1446860092"))

## chart theme/functions ----
source("scripts/chart_theme.R")
source("scripts/functions.R")

## get cansim data ----
provinces <- cansim::get_cansim_vector_for_latest_periods(vectors = c("v1446859483", "v1446859543", "v1446859574",
                                                                      "v1446859605", "v1446859636", "v1446859667",
                                                                      "v1446859789", "v1446859881", "v1446859942",
                                                                      "v1446859973", "v1446860064"), periods = 61) %>%
  mutate(REF_DATE = ymd(REF_DATE, truncated = 2)) %>%
  janitor::clean_names() %>%
  left_join(province_names, by = c("vector")) %>%
  group_by(geo) %>%
  get_mom_stats() %>%
  ungroup() %>%
  select(ref_date, geo, value, mom_pct)

sectors <- cansim::get_cansim_vector_for_latest_periods(vectors = c("v1446860063", "v1446860065",
                                                                    "v1446860078", "v1446860071",
                                                                    "v1446860072", "v1446860084",
                                                                    "v1446860085", "v1446860087",
                                                                    "v1446860091", "v1446860083",
                                                                    "v1446860092"),
                                                       periods = 61) %>%
  mutate(REF_DATE = ymd(REF_DATE, truncated = 2)) %>%
  janitor::clean_names() %>%
  left_join(sector_names, by = c("vector")) %>%
  group_by(vector) %>%
  get_yoy_stats() %>%
  ungroup() %>%
  select(ref_date, sector, value, yoy_pct)

latestDate <- provinces %>% summarise(date = max(ref_date)) %>%
  mutate(date = paste(month(date, label = TRUE, abbr = TRUE), year(date), sep = " ")) %>%
  pull()

# UI demonstrating column layouts
ui <- function(req) {
  fluidPage(title = "BC Retail Sales",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    column(width = 12,
           style = "background-color:#003366; border-bottom:2px solid #fcba19;position:fixed;z-index:10000",
           tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px;
           width:100%;",
             tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
               a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                 img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                 onclick="gtag"
               ),
               h1("Retail Sales for British Columbia", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
             )
           )
    ),
    # column(width=12,
    #        style = "margin-top:100px",
    #         tags$fieldset(
    #               tags$legend(h2("Some heading here")),
    #               p("Some text in a paragraph here.",
    #               style="font-size:14px; color:#494949"),
    #               br()
    #         )
    # ),
    ## Make changes to this column
    column(width = 12,
           style = "margin-top:100px",
           sidebarLayout(
             sidebarPanel(
               width = 3, ## Define width in style instead
               #style="background-color:#F2F2F2;position:fixed;width:300px",
               tags$fieldset(
                 tags$legend(h3("Date")),
                 uiOutput(outputId = "date")
                 ),
               tags$fieldset(
                 tags$legend(h3("Geography")),
                 uiOutput(outputId = "geo")
               ),
               tags$fieldset(
                 tags$legend(h3("Sector")),
                 uiOutput(outputId = "sector")
               ),
               tags$fieldset(
                 tags$legend(h3("Percent Change")),
                 h3(uiOutput(outputId = "pct_chg"))
                 ),
               br(),
               tags$fieldset(
                 tags$legend(h4("Additional information")),
                 uiOutput(outputId = "info")
                 ),
               br(),
               br()
               ),
           mainPanel(
             width = 9,
             #style = "margin-left:320px",
             tabsetPanel(id = "tabs",
               tabPanel("Provinces",
                        tags$fieldset(
                          style = "margin-top:20px;",
                          tags$legend(h3("Month-Over-Month Change in All Retail Trade Sales")),
                          plotlyOutput(outputId = "provinces_mom")
                         ),
                        tags$fieldset(
                       #   style = "margin-top:20px;",
                          tags$legend(h3("Value of All Retail Trade Sales")),
                          plotlyOutput(outputId = "provinces_value")
                        )
                       ),
               tabPanel("Sectors",
                        tags$fieldset(
                          style = "margin-top:20px;",
                          tags$legend(h3("Year-Over-Year Change in Retail Sales in BC")),
                          plotlyOutput(outputId = "sectors_yoy")
                        ),
                        tags$fieldset(
                        #  style = "margin-top:20px;",
                          tags$legend(h3("Value of Retail Sales in BC")),
                          plotlyOutput(outputId = "sectors_value")
                        )

                        ),
               type = "tabs"
             )
           )
        )
    ), ## End of column to make changes to
    column(width = 12,
           style = "background-color:#003366; border-top:2px solid #fcba19;",

            tags$footer(class="footer",
              tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                )
              )
             )
    )
  )
)}

## Define server logic ----
server <- function(input, output, session) {

    get_inputs <- reactive({

    req(input$tabs)

    if(input$tabs == "Provinces") {
      date <- provinces %>%
        filter(as.Date(ref_date) %in% c((max(ref_date) - years(1)):max(ref_date))) %>%
        select(ref_date) %>%
        unique() %>%
        arrange(desc(ref_date)) %>%
        pull()

      names(date) <- paste(month(date, label = TRUE), year(date))

      geos <- provinces %>%
        select(geo) %>%
        unique() %>%
        pull()

      sectors <- c("All Retail Trade")

      change <- "Monthly"
      adjustment <- "seasonally adjusted"

    }

    else if(input$tabs == "Sectors") {

      date <- sectors %>%
        filter(as.Date(ref_date) %in% c((max(ref_date) - years(1)):max(ref_date))) %>%
        select(ref_date) %>%
        unique() %>%
        arrange(desc(ref_date)) %>%
        pull()

      names(date) <- paste(month(date, label = TRUE), year(date))

      geos <- "British Columbia"

      sectors <- sectors %>%
        select(sector) %>%
        unique() %>%
        pull()

      change <- "Yearly"
      adjustment <- "unadjusted"
    }

    list(date = date,
         geos = geos,
         sectors = sectors,
         change = change,
         adjustment = adjustment)

  })

  output$date <- renderUI({

    selectInput(
      inputId = "date",
      label = NULL,
      choices = get_inputs()$date,
      selectize = FALSE,
      size = 3
    )

  })

  output$geo <- renderUI({

    selectInput(
      inputId = "geo",
      label = NULL,
      choices = get_inputs()$geos,
      selected = "British Columbia"
    )

  })

  output$sector <- renderUI({

    selectInput(
      inputId = "sector",
      label = NULL,
      choices = get_inputs()$sectors,
      selected = "All Retail Trade"
    )

  })

  output$info <- renderUI({

    HTML(paste0("All numbers and figures on this tab are based on <b>",
                get_inputs()$adjustment, "</b> estimates.",
                "<br><br> Sources: <a href='https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2010005601'>Table 20-10-0056-01</a>",
                " <br>", "Code for this app: <a href = 'https://github.com/bcgov/CANSIM-retail-trade-shiny'>Github</a>",
                " <br>", "Last Reference Date: ", latestDate))
  })

  get_data <- reactive({

    req(input$tabs)
    req(input$date)
    req(input$geo)
    req(input$sector)

    if(input$tabs == "Provinces"){

      pct_chg <- provinces %>%
        filter(ref_date == input$date,
               geo == input$geo) %>%
        pull(mom_pct)

      prov_bar_chart <- provinces %>%
        filter(ref_date == input$date) %>%
        mutate(order = case_when(geo == "British Columbia" ~ 1,
                                 geo == "Canada" ~ 2,
                                 TRUE ~ rank(desc(mom_pct)) + 2),
               geo = str_replace_all(geo, " ", "\n")) %>%
        arrange(order) %>%
        mutate(geo = factor(geo, levels = geo))

      prov_line_chart <- provinces %>%
            filter(geo == input$geo) %>%
            mutate(value = value/1000000)

      sect_bar_chart <- NULL

      sect_line_chart <- NULL

    }

    else if(input$tabs == "Sectors"){

      pct_chg <- sectors %>%
        filter(ref_date == input$date,
               sector == input$sector) %>%
        pull(yoy_pct)

      prov_bar_chart <- NULL

      prov_line_chart <- NULL

      sect_bar_chart <- sectors %>%
        filter(ref_date == input$date) %>%
        mutate(order = case_when(sector == "All Retail Trade" ~ 1,
                                 TRUE ~ rank(desc(yoy_pct)) + 1),
               sector = str_replace_all(sector, " ", "\n")) %>%
        arrange(order) %>%
        mutate(sector = factor(sector, levels = sector))

      sect_line_chart <- sectors %>%
        filter(sector == input$sector) %>%
        mutate(value = value/1000000)

    }

    list(pct_chg = pct_chg,
         prov_bar_chart = prov_bar_chart,
         prov_line_chart = prov_line_chart,
         sect_bar_chart = sect_bar_chart,
         sect_line_chart = sect_line_chart)

  })

  output$pct_chg <- renderUI({

    val <- get_data()$pct_chg %>%
      round_half_up(digits = 1)

    icon <- ifelse(val > 0, "arrow-alt-circle-up", "arrow-alt-circle-down")

    tagList(
      HTML(paste0(get_inputs()$change,": ",val, "%")),
      icon(icon)
    )

  })

  output$provinces_mom <- renderPlotly({

    if(is.null(get_data()$prov_bar_chart)) {
      NULL
    }

    else{
      p <- ggplot(get_data()$prov_bar_chart,
                  aes(x = geo, y = mom_pct)) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 2.5) +
        labs(x = NULL,
             y = "% Change",
             title = "By Province") +
        bcstats_chart_theme
       # theme_classic() +
        #theme(axis.line.x = element_blank())

      p <- ggplotly(p)
    }

  })

  output$provinces_value <- renderPlotly({

    if(is.null(get_data()$prov_line_chart)){
      NULL
    }

    else{

      p <- ggplot(get_data()$prov_line_chart,
                  aes(x = ref_date, y = value)) +
        geom_line() +
        labs(x = NULL,
             y = "Billions of Dollars",
             title = paste("For",input$geo)) +
        bcstats_chart_theme +
        scale_x_date(limits = c(max(get_data()$prov_line_chart$ref_date) - years(5),
                                max(get_data()$prov_line_chart$ref_date) + months(3)),
                     expand = c(0,0),
                     date_breaks = "6 months",
                     date_labels = "%b\n%Y" )

      p <- ggplotly(p)

    }


  })

  output$sectors_yoy <- renderPlotly({

    if(is.null(get_data()$sect_bar_chart)) {
      NULL
    }

    else{
      p <- ggplot(get_data()$sect_bar_chart,
                  aes(x = sector, y = yoy_pct)) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 1.5) +
        labs(x = NULL,
             y = "% Change",
             title = "By Sector") +
        bcstats_chart_theme
        # theme_classic() +
        # theme(axis.line.x = element_blank())

      p <- ggplotly(p)

    }
  })

  output$sectors_value <- renderPlotly({

    if(is.null(get_data()$sect_line_chart)){
      NULL
    }

    else{

      p <- ggplot(get_data()$sect_line_chart,
                  aes(x = ref_date, y = value)) +
        geom_line() +
        labs(x = NULL,
             y = "Billions of Dollars",
             title = paste("For",input$sector)) +
        bcstats_chart_theme +
        scale_x_date(limits = c(max(get_data()$sect_line_chart$ref_date) - years(5),
                                max(get_data()$sect_line_chart$ref_date) + months(3)),
                     expand = c(0,0),
                     date_breaks = "6 months",
                     date_labels = "%b\n%Y" )

      p <- ggplotly(p)

    }

  })

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)
