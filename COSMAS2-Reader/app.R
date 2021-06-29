# Backend
library(shiny)
library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(purrr)
library(wordcloud2)


# Frontend
ui <- fluidPage(
# Application title
    titlePanel("Convert a COSMAS II export file to table"),

    sidebarLayout(
        sidebarPanel(
            h3("Your file must contain the Key Word in Context (KWIC) information."),
            # Input raw COSMAS text file
            fileInput(inputId = "raw.file",
                      label = "Upload a text file (.txt or .TXT, max. 30 MB)",
                      buttonLabel = "Browse",
                      placeholder = "No file selected"),
            radioButtons("corpus.position", "Where is the information about the corpus source?",
                         c("After token" = "after",
                           "Before token" = "before")),
            radioButtons("context.type", "What is the token's context?",
                         c("Paragraph" = "paragraph",
                           "Sentence" = "one.sentence",
                           "Word" = "one.word",
                           "Letter" = "one.letter")),
            radioButtons("korpusansicht", "Is the 'Korpusansicht' included?",
                         c("No" = "no.corpus",
                           "Yes" = "yes.corpus")),
            actionButton("go", "Submit", class = "btn-success", icon = shiny::icon("gears")),
            hr(),
            p(strong("Download data as CSV table")),
            # Download data
            downloadButton(outputId = "downloadData", 
                           label = "Download",
                           icon = shiny::icon("download")),
        ),
        mainPanel(
            tabsetPanel(tabPanel("Table", DT::dataTableOutput("table.output")),
                        tabPanel("Tokens", DT::dataTableOutput("unique.tokens")),
                        tabPanel("Word cloud", wordcloud2Output("word.cloud")))
        )
    )
)

server <- function(input, output, session) {
    options(shiny.maxRequestSize=100*1024^2) 
    # Process data file and create a table
    mydata <- eventReactive(input$go, {
        inFile <- input$raw.file
        if (is.null(inFile))
            return(NULL)
        raw.file <- read_file(inFile$datapath, locale(encoding="latin1"))
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
        all_sentences <- sections[[1]][4]
        text_parts <- all_sentences %>%
            str_match_all(regex(paste("(.*?)<B>(.+?)</>(.*?)\\(((?:",corporaID,")/.*?)\\)\\s*\\n", sep=""), 
                                dotall = TRUE))
        
        # Source information
        data <- data.frame(Sources = text_parts[[1]][,5])
        
        # Tokens
        data$Token <- text_parts[[1]][,3] %>%
            str_trim()

        # Context sentence BEFORE token sentence
        data$Precontext <- text_parts[[1]][,2] %>%
            str_extract_all(boundary("sentence")) %>%
            map(function(x) {nth(x,-2)} ) %>%
            str_trim() %>%
            unlist()
        
        # Sentence part BEFORE token
        data$Prehit <- text_parts[[1]][,2] %>%
            str_extract_all(boundary("sentence")) %>%
            map(last) %>%
            str_trim() %>%
            unlist()
        
        # Sentence part AFTER token
        data$Posthit <- text_parts[[1]][,4] %>%
            str_extract_all(boundary("sentence")) %>%
            map(first) %>%
            str_trim() %>%
            unlist()
        
        # Extract context sentence AFTER token sentence
        data$Postcontext <- text_parts[[1]][,4] %>%
            str_extract_all(boundary("sentence")) %>%
            map(function(x) {nth(x,2)} ) %>%
            str_trim() %>%
            unlist()

        # Creating data frame for export ------------------------------------------
        sentence_data <- 
            data %>%
            unite(Prehit, Token, Posthit, col="Sentence", sep = " ", remove=F) %>%
            mutate(C2API_Version = C2API_Version, Export_Date = Export_Date) %>%
            select(C2API_Version, Export_Date, Token, Precontext, Sentence, Postcontext, Sources) %>%
            replace_na(list(Precontext = "", Postcontext = ""))

        return(sentence_data)
    })
    
    # Output a table with all data
    output$table.output <- DT::renderDataTable({
        DT::datatable(mydata(), options = list(orderClasses = TRUE))
    })
    
    # Output table with unique tokens and their frequencies
    output$unique.tokens <- DT::renderDataTable({
        token_count <- mydata() %>%
                     count(Token, sort=TRUE)
        DT::datatable(token_count)
    })
    
    # Make a word cloud plot
    output$word.cloud <- renderWordcloud2({
        # Filter the target sentence
        unique_words <- mydata() %>%
            select(Sentence) %>%
            str_to_sentence("de") %>%
            str_extract_all(boundary("word")) %>%
            unlist() %>%
            str_subset(regex("[^und, der, die, das, Der, Die, Das, mond][:alpha:]")) %>%
            data.frame()
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