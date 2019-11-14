# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#
#   http://rstudio.github.io/shinydashboard/get_started.html
#
# To deploy an update, update code and data, then load >library(rsconnect), set working
# directory to app.R directory and >deployApp(appName = "retailSalesApp", appId = 958258)

#####
# METADATA for app
 # dataVersion <- "Retail Trade"
 # updateDate <- "Nov 2019"

## load libraries  ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')
if (!require('DT')) install.packages('DT')
library(plotly)
#if (!require('GAlogger')) devtools::install_github("bnosac/GAlogger")

# ga_set_tracking_id("UA-150850915-1")
# ga_set_approval(consent = TRUE)
# ga_collect_pageview(page = "/retailSalesApp")

## read data ----
#data1 <- readRDS("data/data1.rds")  ## by single-year intervals

provinces <- readRDS("data/provinces.rds")

dates <- provinces %>%
  filter(as.Date(ref_date) %in% c((latest_month - years(1)):latest_month)) %>%
  select(ref_date) %>%
  unique() %>%
  arrange(desc(ref_date)) %>%
  pull()

names(dates) <- paste(month(dates, label = TRUE), year(dates))

geos <- provinces %>%
  select(geo) %>%
  unique() %>%
  pull()

# UI demonstrating column layouts
ui <- fluidPage(title = "BC Retail Sales",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    column(width = 12,
           style = "background-color:#003366; border-bottom:2px solid #fcba19;",
           tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px; width:100%;",
             tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
               a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                 img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                 onclick="gtag"
               )#,
               #h1("British Columbia - Retail Sales", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
             )
           )
    ),
    column(width=12,
            tags$fieldset(
                  tags$legend(h2("British Columbia - Retail Sales")),
                  p("Some text in a paragraph here.",
                  style="font-size:14px; color:#494949"),
                  br()
            )
    ),
    ## Make changes to this column
    column(width = 12,
           sidebarLayout(
             sidebarPanel(
               style="background-color:#F2F2F2;",
               width = 2,
               tags$fieldset(
                 tags$legend(h3("Date selection")),
                 selectInput(
                   inputId = "date",
                   label = "Date",
                   choices = dates
                   )
                 ),
               tags$fieldset(
                 tags$legend(h3("Monthly Change")),
                 h3(uiOutput(outputId = "BC_mom"))
                 ),
               br(),
               tags$fieldset(
                 tags$legend(h3("Yearly Change")),
                 h3(uiOutput(outputId = "BC_yoy"))
                 ),
               br(),
               br(),
               br()
               ),
           mainPanel(
             width = 10,
             tabsetPanel(
               tabPanel("Provinces",
                        tags$fieldset(
                          style = "margin-top:20px;",
                          tags$legend(h3("Month-Over-Month Change in Retail Sales")),
                          plotlyOutput(outputId = "provinces_mom")
                         ),
                        tags$fieldset(
                          style = "margin-top:20px;",
                          tags$legend(h3("Value of Retail Sales")),
                          selectInput(
                            inputId = "geo",
                            label = "Geography",
                            choices = geos,
                            selected = "British Columbia"
                          ),
                          plotlyOutput(outputId = "provinces_value")
                        )
                       ),
               tabPanel("Sectors"

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
)

## Define server logic ----
server <- function(input, output, session) {

  output$BC_mom <- renderUI({

    val <- provinces %>%
      filter(ref_date == input$date,
             geo == input$geo) %>%
      pull(mom_pct) %>%
      round_half_up(digits = 1) %>%
      paste0("%")

    icon <- ifelse(val > 0, "arrow-alt-circle-up", "arrow-alt-circle-down")

    tagList(
      HTML(val),
      icon(icon)
    )

  })
  ## Should make a reactive for filtering the data so only done once and not
  ## in each render

  ## Does it make sense to do yearly change on seasonally adjusted data?
  output$BC_yoy <- renderUI({

    val <- provinces %>%
      filter(ref_date == input$date,
             geo == input$geo) %>%
      pull(yoy_pct) %>%
      round_half_up(digits = 1) %>%
      paste0("%")

    icon <- ifelse(val > 0, "arrow-alt-circle-up", "arrow-alt-circle-down")

    tagList(
      HTML(val),
      icon(icon)
    )

  })

  output$provinces_mom <- renderPlotly({

    plot_data <- provinces %>%
      filter(ref_date == input$date) %>%
      mutate(order = case_when(geo == "British Columbia" ~ 1,
                               geo == "Canada" ~ 2,
                               TRUE ~ rank(desc(mom_pct)) + 2)) %>%
      arrange(order) %>%
      mutate(geo = factor(geo, levels = geo)) ## To ensure bars ordered in chart correctly

    p <- ggplot(plot_data,
                aes(x = geo, y = mom_pct)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 2.5) +
      labs(x = NULL,
           y = "% Change",
           title = "By Province") +
      theme_classic() +
      theme(axis.line.x = element_blank(),
            axis.text.x = element_text(angle = 25))

    p <- ggplotly(p)

  })

  ## Should this chart be done as an index to be able to compare across provinces?
  output$provinces_value <- renderPlotly({

    plot_data <- provinces %>%
      filter(geo == input$geo,
             as.Date(ref_date) %in% c((latest_month - years(5)):latest_month)) %>%
      mutate(value = value/1000000)

    p <- ggplot(plot_data, aes(x = ref_date, y = value)) +
      geom_line() +
      labs(x = NULL,
           y = "Billions of Dollars",
           #title = input$geo) +
           title = "British Columbia") +
      theme_classic() +
      scale_x_date(limits = c(latest_month - years(5), latest_month),
                   expand = c(0,0),
                   date_breaks = "6 months",
                   date_labels = "%b %Y" ) +
      theme(axis.text.x = element_text(angle = 15))

    p <- ggplotly(p)

    p$x$data[[1]]$text <- paste0(p$x$data[[1]]$text, "<br />yoy_pct: ", plot_data$yoy_pct)

    p

  })



}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)
