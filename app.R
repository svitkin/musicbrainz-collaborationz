library(shiny)
library(DT)
library(tidyr)
library(stringr)
library(urltools)
library(shinycssloaders)

source("R/data-retrieval.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$style(HTML("
    .shiny-output-error-validation {
        color: red;
        font-size: 16px;
    }
    "))),
    shiny::tags$br(),
    fluidRow(),
    # Application title
    column(8,
           tags$h1("MusicBrainz Collaborationz"),
           style = "border: 2px #8B008B solid; font-family: Optima"),
    fluidRow(column(12,
                    HTML("<p>All data is pulled from the <a href='https://musicbrainz.org/' target='_blank'>MusicBrainz</a> database.</p>"))),
    shiny::tags$br(),
    shiny::tags$br(),
    fluidRow(
        column(4,
               tags$head(tags$script(src = "helper-script.js")),
               textInput("artistName", "Artist Name: "),
               actionButton("makeTable", "Search")
        )
    ),
    shiny::tags$br(),
    shiny::tags$br(),
    fluidRow(
        column(10,
               offset = 1,
               withSpinner(dataTableOutput("artistCollabsTable"),
                           type = 7)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        output$artistCollabsTable <-
            renderDataTable({
                validate(need(input$makeTable, ""))
                # Load in collaboration data for artist
                mb_get_release_collaborations_by_artist(isolate(input$artistName)) %>%
                    # Create hyperlink to project
                    unite(a_tag, release_title, artist_2, sep = " ", remove = FALSE) %>%
                    mutate(a_tag = str_c("<a href='",
                                         str_replace(url_encode(
                                             paste0(str_c("https://duckduckgo.com/listen to ",
                                                          a_tag, " ", isolate(input$artistName)))),
                                             "/listen", "/?q=listen"),
                                         "' target='_blank'>", release_title, "</a>"),
                           artist_2 = str_c("<span onclick='searchArtistFromTable(this);' style='color: #18bc9c; cursor: pointer;'>", artist_2,"</span>")) %>%
                    select(Collaborator = artist_2,
                           Project = a_tag,
                           `Release Date` = release_date,
                           `Release Type` = release_type) %>%
                    arrange(Collaborator, `Release Date`)
            },
            escape = FALSE,
            rownames = FALSE,
            selection = "none",
            options = list(language = list(
                               zeroRecords = "No collaborative projects found.")))
}

# Run the application
shinyApp(ui = ui, server = server)
