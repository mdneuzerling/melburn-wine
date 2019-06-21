library(shiny)
library(lime)

vectoriser <- readr::read_rds("vectoriser.rds")
tfidf <- readr::read_rds("tfidf.rds")
stem_tokeniser <- readr::read_rds("stem_tokeniser.rds")
map_to_dtm <- readr::read_rds("map_to_dtm.rds")
explainer <- readr::read_rds("wine_classification_explainer.rds")

shared_states <- list()

feature_selection_strategy <- local({
    strategies <- c("auto", "none", "forward_selection", "highest_weights",
                    "lasso_path", "tree")
    strategies_clean <- stringi::stri_replace_all_fixed(strategies, "_", " ")
    names(strategies) <- strategies_clean
    as.list(strategies)
})

ui <- fluidPage(
    title = "Shiny Wine",
    theme = shinythemes::shinytheme("superhero"),
    titlePanel(title = "Is this wine good?"),
    hr(),
    sidebarPanel(
        textAreaInput(
            "text_to_explain", 
            label = NULL, 
            resize = "both", 
            height = "200px"
        ),
        numericInput(
            "number_permutations", 
            label = h5("Quantity of permutations to generate"), 
            value = 5000, 
            step = 1000
            ),
        selectInput(
            "feature_selection_strategy", 
            label = h5("Word selection strategies"), 
            choices = feature_selection_strategy, 
            selected = "auto"
        ),
        sliderInput(
            "number_features_to_explain", 
            label = h5("Number of words to select"), 
            min = 1, 
            max = 20, 
            value = 5, 
            ticks = FALSE
        )
    ),
    
    mainPanel(
        lime::text_explanations_output("text_explanations_plot")
    )
)

server <- function(input, output) {
    library(shiny)
    library(lime)
    output$text_explanations_plot <- render_text_explanations({
        validate(
            need(
                stringi::stri_count_words(input$text_to_explain) >= 5, 
                message = "Text provided is too short to be explained (>= 5)."
            )
        )
        shared_states$explanations <<- lime::explain(
            input$text_to_explain, 
            explainer, 
            n_labels = 1, 
            n_features = input$number_features_to_explain, 
            feature_select = input$feature_selection_strategy, 
            n_permutations = input$number_permutations
        )
        plot_text_explanations(shared_states$explanations)
    })
}

shinyApp(ui, server)