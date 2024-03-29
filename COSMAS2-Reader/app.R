# Backend
library(shiny)
library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(purrr)
library(wordcloud2)
library(shinythemes)
library(ggplot2)


# Frontend
ui <- fluidPage(
    # Theme
    theme = shinytheme("sandstone"),

# Application title
    titlePanel("Convert a COSMAS II export file to table"),

    sidebarLayout(
        sidebarPanel(
            h4("Your file must contain the Key Word in Context (KWIC) information and be exported to a plain text file."),
            # Input raw COSMAS text file
            fileInput(inputId = "raw.file",
                      label = "Upload a plain text file (max. 30 MB)",
                      buttonLabel = "Browse",
                      placeholder = "No file selected"),
            radioButtons("corpus.position", "Where is the information about the corpus source?",
                         c("After token" = "after",
                           "Before token" = "before",
                           "Not included" = "not.included")),
            radioButtons("context.type", "What is the token's context?",
                         c("Paragraph" = "paragraph",
                           "Sentence" = "one.sentence",
                           "Word" = "one.word",
                           "Letter" = "one.letter")),
            actionButton("go", "Submit", class = "btn btn-info btn-block", icon = shiny::icon("gears")),
            hr(),
            p(strong("Download data as CSV table")),
            # Download data
            downloadButton(outputId = "downloadData", 
                           label = "Download",
                           class = "btn btn-block",
                           icon = shiny::icon("download"))
        ),
        mainPanel(
            tabsetPanel(tabPanel("Table", 
                                 h4("Preview of the generated table"),
                                 DT::dataTableOutput("table.output")),
                        tabPanel("Tokens", 
                                 h4("Search phrase used to generate results"),
                                 verbatimTextOutput("phrase"),
                                 h4("Frequencies of unique tokens"),
                                 DT::dataTableOutput("unique.tokens"), 
                                 plotOutput("plot.tokens")),
                        tabPanel("Word cloud", wordcloud2Output("word.cloud")))
        )
    )
)

server <- function(input, output, session) {
    options(shiny.maxRequestSize=100*1024^2) 
    myphrase = reactiveVal()
    # Process data file and create a table
    mydata <- eventReactive(input$go, {
        inFile <- input$raw.file
        if (is.null(inFile))
            return(NULL)
        raw.file <- read_file(inFile$datapath, locale(encoding="latin1"))

        # Export options ----------------------------------------------------------
        context_type <- switch(input$context.type,
                               paragraph = 1,
                               one.sentence = 1,
                               one.word = 0,
                               one.letter = 0)
        corpus_position <- switch(input$corpus.position,
                                  after = "after",
                                  before = "before",
                                  not.included = "not.included")
        # Metadata ----------------------------------------------------------------
        # Save COSMAS version 
        C2API_Version <- raw.file %>%
            str_extract(regex("(?<=C2API-Version )(.*)(?= -)")) %>%
            str_subset(regex(".*"))
        
        # Split file into sections
        sections <- raw.file %>%
            str_split("\\_{80}")
        
        # Save export date
        Export_Date <- sections[[1]][2] %>%
            str_extract(regex("(?<=\\n\\nDatum).+(?=\\nArchiv)")) %>%
            str_extract(regex("(?<=:\\s)(.*)$"))
        
        # Save the search phrase
        phrase <- sections[[1]][2] %>%
            str_extract(regex("(?<=\\nSuchanfrage).+(?=\\nSuchoptionen)")) %>%
            str_extract(regex("(?<=:\\s)(.*)$"))
        myphrase(phrase)
        
        # Sentences and their information -----------------------------------------
        # Abbreviated corpus IDs
        corpora <- sections[[1]][3]
        corporaID <- corpora %>% 
            str_split("\\n") %>%
            unlist() %>%
            head(-7) %>%
            str_extract(regex('(^\\w+)\\s(?=.*$)')) %>%
            str_trim() %>%
            str_subset(regex(".*")) %>%
            unique() %>%
            str_c(collapse = "|")
        
        # All sentences
        all_sentences <- sections[[1]][4] %>%
            str_split(regex("\\n.+-Ansicht\\,\\s+[:digit:]*\\s+Einträge")) %>%
            unlist()
        
        # Split the file depending on whether the source information is before or after the target sentence, or is absent
        if (corpus_position == "after") {
            tp <- paste("(.*?)<B>(.+?)</>(.*?)\\(((?:",corporaID,")/.*?)\\)\\s*\\n", sep="")
            t <- 3 # token
            s <- 5 # sentence
            b <- 2 # before token
            a <- 4 # after token
        } else if (corpus_position == "before") {
            tp <- paste("((?:",corporaID,")/.*?)\\s*\\n+(.*?)<B>(.+?)</>(.*?)\\n", sep="")
            t <- 4 # token
            s <- 2 # sentence
            b <- 3 # before token
            a <- 5 # after token
        } else {
            tp <- paste("(.*?)<B>(.+?)</>(.*?)\\s*\\n", sep="")
            t <- 3 # token
            b <- 2 # before token
            a <- 4 # after token
        }
        
        # Splitting the sentences into text parts
        text_parts <- 
            all_sentences[1] %>%
            str_match_all(regex(tp,
                          dotall = TRUE))
        
        # Tokens
        data <- data.frame(Token = text_parts[[1]][,t] %>%
                               str_trim())
            
        # Source information
        if (corpus_position != "not.included") {
        data$Source <- text_parts[[1]][,s]
        } else {}
        
        # Context sentence BEFORE token sentence
        if (context_type == 1) {
        data$Precontext <- text_parts[[1]][,b] %>%
            str_extract_all(boundary("sentence")) %>%
            map(function(x) {nth(x,-2)} ) %>%
            str_trim() %>%
            unlist()
        } else {
            data$Precontext <- text_parts[[1]][,b]
        }
        
        # Sentence part BEFORE token
        data$Prehit <- text_parts[[1]][,b] %>%
            str_extract_all(boundary("sentence")) %>%
            map(last) %>%
            str_trim() %>%
            unlist()
        
        # Sentence part AFTER token
        data$Posthit <- text_parts[[1]][,a] %>%
            str_extract_all(boundary("sentence")) %>%
            map(first) %>%
            str_trim() %>%
            unlist()
        
        # Extract context sentence AFTER token sentence
        if (context_type == 1) {
            data$Postcontext <- text_parts[[1]][,a] %>%
            str_extract_all(boundary("sentence")) %>%
            map(function(x) {nth(x,2)} ) %>%
            str_trim() %>%
            unlist()
        } else {
            data$Postcontext <- text_parts[[1]][,a]
        }

        # Creating data frame for export ------------------------------------------
        # Choose which columns to keep based on the input structure
        cols.included <- c("C2API_Version", "Export_Date")
        
        # If the context is a word or letter, omit the sentence
        if (context_type == 1) {
            cols.included <- append(cols.included,
                                   c("Token", 
                                    "Precontext", 
                                    "Sentence", 
                                    "Postcontext"))
        } else {
            cols.included <- append(cols.included,
                                   c("Precontext", 
                                   "Token", 
                                   "Postcontext"))
        }
        
        # If there is corpus information, add it
        if (corpus_position != "not.included") {
            cols.included <- append(cols.included, "Source")
        } else {}
        
        # If the context is a paragraph or a sentence then make a target sentence, otherwise omit it
        if (context_type == 1) {
        sentence_data <- 
            data %>%
            unite(Prehit, Token, Posthit, col="Sentence", sep = " ", remove=F) %>%
            mutate(C2API_Version = C2API_Version, Export_Date = Export_Date) %>%
            select(cols.included) %>%
            replace_na(list(Precontext = "", Postcontext = ""))
        } else {
            sentence_data <- 
                data %>%
                mutate(C2API_Version = C2API_Version, Export_Date = Export_Date) %>%
                select(cols.included) %>%
                replace_na(list(Precontext = "", Postcontext = ""))
        }
        
        return(sentence_data)
    })
    
    # Output a table with all data
    output$table.output <- DT::renderDataTable({
        DT::datatable(mydata(), options = list(orderClasses = TRUE))
    })
    
    # Output search phrase
    output$phrase <- renderText({myphrase()})
    
    # Output table with unique tokens and their frequencies
    output$unique.tokens <- DT::renderDataTable({
        token_count <- mydata() %>%
                     count(Token, sort=TRUE)
        DT::datatable(token_count)
    })
    
    # Make a plot of the unique tokens and their frequencies
    output$plot.tokens <- renderPlot({
        token_count <- mydata() %>%
            count(Token, sort=TRUE)
        ggplot(token_count, aes(x = Token, y = n)) + geom_bar(stat = "identity")
    })

    # Make a word cloud plot
    output$word.cloud <- renderWordcloud2({
        # Export options 
        context_type <- switch(input$context.type,
                               paragraph = 1,
                               one.sentence = 1,
                               one.word = 0,
                               one.letter = 0)
        if (context_type == 1) {
        # Filter the target sentence
        unique_words <- mydata() %>%
            select(Sentence) %>%
            str_to_sentence("de") %>%
            str_extract_all(boundary("word")) %>%
            unlist() %>%
            str_subset(regex("[^und, der, die, das, Der, Die, Das][:alpha:]")) %>%
            data.frame()
        } else {
            unique_words <- mydata() %>%
                select(Precontext, Postcontext) %>%
                unlist() %>%
                str_subset(regex("[^und, der, die, das, Der, Die, Das][:alpha:]")) %>%
                data.frame()
        }
        
        colnames(unique_words) <- "word"
        # Calculate word frequencies
        unique_words <- unique_words %>%
            count(word, sort=TRUE)
        # Make word cloud
        wordcloud2(data=unique_words, size=1.6, color='random-dark', shape = "circle")
    })
    
    # Download table
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("CorpusData-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(mydata(), file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)