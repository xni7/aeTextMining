library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(wordcloud2)
library(wordcloud)
library(stringr)

adae <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc-split/adae.xpt")
adae %>% count(TRTA, AEDECOD, sort=TRUE)
adae %>% count(USUBJID, AEDECOD, sort=TRUE)

# Refer to {tidytext} book
# https://www.tidytextmining.com/ngrams.html


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectizeInput(inputId = "grp", label="Group", choices=c("All", "PBO", "Xano"), selected="All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", wordcloud2Output("wc", height = 800)),
        tabPanel("AE Co-occurrence", plotOutput("co", height = 800)),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  #ae <- readr::read_csv("./ae.csv")
  #ae %>% count(SUBJIDN, AEDECOD, sort=TRUE)

  adae_selected <- reactive({
    if(input$grp == "All") {
      adae1 <- adae
    } else if (input$grp == "PBO") {
      adae1 <- adae %>% filter(TRTA == "Placebo")
    } else if (input$grp == "Xano") {
      adae1 <- adae %>% filter(TRTA != "Placebo")
    }
    adae1
  })


  adaetxt <-  reactive({
    adae_selected() %>%
      transmute(text = str_replace_all(AEDECOD, "[[:space:]]", "")
                , AEDECOD
                , book=USUBJID)

  })

  output$wc <- renderWordcloud2({
    # wordcloud
    adaetxt() %>% count(AEDECOD, sort=T) %>% wordcloud2()

  })

  # dictionary
  aedict <- reactive({
    adaetxt() %>%
    distinct(text, AEDECOD) %>%
    mutate(text = tolower(text), AEDECOD = str_to_title(AEDECOD))
  })

  # TODO: move the reactive content out to reuse
  output$summary <- renderPrint({
    book_words <- adaetxt() %>%
      unnest_tokens(word, text, token = "words", to_lower = FALSE) %>%
      count(book, word, sort = TRUE)

    total_words <- book_words %>%
      group_by(book) %>%
      summarize(total = sum(n))

    book_words <- left_join(book_words, total_words)

    print(book_words)


    book_tf_idf <- book_words %>%
      bind_tf_idf(word, book, n)

    print(book_tf_idf %>% arrange(desc(n), desc(tf_idf)))

    adaetxt2 <- adaetxt() %>% select(-AEDECOD)

    ae_bigrams <- adaetxt2 %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)

    ae_bigrams


    # remove self bigrams
    # https://stackoverflow.com/questions/29438282/find-repeated-pattern-in-a-string-of-characters-using-r
    ae_bigrams2 <- ae_bigrams %>%
      filter(!grepl("\\b(\\S+?)([[:space:]])\\1\\b", bigram, perl = TRUE))


    bigrams_separated <- ae_bigrams2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")

    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)

    # new bigram counts:
    bigram_counts <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE) %>%
      filter(!is.na(word1)) %>%
      #bigram_counts %>%
      filter(n>1) %>%
      left_join(aedict() %>% rename(WORD1=AEDECOD), by = c("word1"="text")) %>%
      left_join(aedict() %>% rename(WORD2=AEDECOD), by = c("word2"="text"))  %>%
      transmute(word1=WORD1, word2=WORD2, n)

    print(bigram_counts)


  })

  # co-occurrence

  output$co <- renderPlot({
    adaetxt2 <- adaetxt() %>% select(-AEDECOD)

    ae_bigrams <- adaetxt2 %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)

    ae_bigrams


    # remove self bigrams
    # https://stackoverflow.com/questions/29438282/find-repeated-pattern-in-a-string-of-characters-using-r
    ae_bigrams2 <- ae_bigrams %>%
      filter(!grepl("\\b(\\S+?)([[:space:]])\\1\\b", bigram, perl = TRUE))


    bigrams_separated <- ae_bigrams2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")

    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)

    # new bigram counts:
    bigram_counts <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE) %>%
      filter(!is.na(word1)) %>%
      #bigram_counts %>%
      filter(n>1) %>%
      left_join(aedict() %>% rename(WORD1=AEDECOD), by = c("word1"="text")) %>%
      left_join(aedict() %>% rename(WORD2=AEDECOD), by = c("word2"="text"))  %>%
      transmute(word1=WORD1, word2=WORD2, n)

    bigram_counts


    bigrams_united <- bigrams_filtered %>%
      unite(bigram, word1, word2, sep = " ")

    bigrams_united



    #### network graph #####
    # original counts
    bigram_counts


    # filter for only relatively common combinations
    # bigram_graph <- bigram_counts %>%
    #   filter(n > 1) %>%
    #   graph_from_data_frame()
    #
    # bigram_graph

    # set.seed(2017)
    #
    # ggraph(bigram_graph, layout = "fr") +
    #   geom_edge_link() +
    #   geom_node_point() +
    #   geom_node_text(aes(label = name), vjust = 1, hjust = 1)



    #https://stackoverflow.com/questions/56173310/how-to-adjust-the-width-of-edges-by-weight-in-ggraph-network-in-r

    set.seed(2021)
    bigram_counts %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(alpha = .25,
                     aes(width = n)) +
      geom_node_point(color = "blue", size = 2) +
      geom_node_text(aes(label = name), size=5, repel = TRUE)+
      theme_graph()+
      labs(title = 'AE Co-occurrence networkplot')

  })




}

shinyApp(ui, server)



