navbarPage ("PMIX (p,q,P,Q)",
                  tabPanel ("Dados Historicos",
                            sidebarPanel (
                              fileInput ("file", "Serie Historica hidrologica de vazoes",
                                         multiple = FALSE,
                                         accept = c ("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                              tags$hr ( ),
                              checkboxInput ("header", "Header", TRUE),
                              selectInput ("sep", label = "Separador de colunas", 
                                           choices = list ("Ponto" = '.', "Virgula" = ",", "Ponto e virgula" = ";", "Tabulacao" = "\t"), 
                                           selected = ";"),
                              selectInput ("dec", label = "Separador decimal", 
                                           choices = list ("Ponto" = '.', "Virgula" = ","), 
                                           selected = ',')
                            ),
                            mainPanel (plotOutput ("dados"),
                                       dataTableOutput("tabelaAnualHist"),
                                       dataTableOutput("tabelaMensalHist")
                            )
                  ),
                  
                  tabPanel ("Algoritmo",
                            fluidRow (
                              column (width = 1,
                                      numericInput ("p", label = "p", value = 1, min = 0, max = 12),
                                      numericInput ("q", label = "q", value = 0, min = 0, max = 12),
                                      numericInput ("P", label = "P", value = 0, min = 0, max = 12),
                                      numericInput ("Q", label = "Q", value = 0, min = 0, max = 12)
                              ),
                              column (width = 4,
                                      radioButtons ("tipo", label = "Estimacao de parametros",
                                                    choices = list ("Metodo de Powell" = 1,
                                                                    "Algoritmo Genetico" = 2), 
                                                    selected = 1)
                              ),
                              column (width = 2,
                                      checkboxInput ("lagSignificativo", "Lag Significativo", TRUE),
                                      numericInput ("lagAnual", label = "lag Anual", value = 1, min = 1, max = 12),
                                      numericInput ("lagMensal", label = "lag Mensal", value = 1, min = 1, max = 12)
                              )
                            ),
                            tags$hr ( ),
                            fluidRow (
                              column (width = 4,
                                      sliderInput ("nPop", label = "Tamanho da populacao", min = 10, max = 100, value = 50),
                                      sliderInput ("cicloMax", label = "Ciclo Maximo", min = 0, max = 50000, value = 10000),
                                      sliderInput ("nsint", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000),
                                      numericInput ("pC", label = "Probabilidade de cruzamento", value = 80, min = 0, max = 100),
                                      numericInput ("pM", label = "Probabilidade de mutacao", value = 5, min = 0, max = 100)
                              ),
                              column (width = 4,
                                      numericInput ("MAPEdiferencaMAX", label = "MAPEdiferencaMAX", value = 5, min = 0, max = 100),
                                      numericInput ("MAPEavaliacao", label = "MAPEavaliacao", value = 20, min = 0, max = 100)
                              )
                            ),
                            tags$hr ( ),
                            fluidRow (
                              column (width = 4,
                                      checkboxInput ("volume", "Gerar Volume", TRUE),
                                      checkboxInput ("hurst", "Gerar Hurst", TRUE)
                              )
                            )
                  ),
                  tabPanel ("Resultados",
                            sidebarPanel (
                              actionButton ("iniciar", "Iniciar!"),
                              verbatimTextOutput("resultadoGeral"),
                              tags$hr ( ),
                              selectInput ("nSerie", "Serie a ser analisada:", choices = 1:50, selected = 50),
                              tags$hr ( ),
                              downloadButton ("downloadSerie", "Download", icon ("save"))
                            ),
                            mainPanel (
                              tabsetPanel (
                                tabPanel("Tabela avaliacoes",
                                         dataTableOutput("tabelaAvaliacao"),
                                         verbatimTextOutput("observacoes")
                                ),
                                tabPanel("Graficos series",
                                         plotOutput("GraficoSerie"),
                                         dataTableOutput("tabelaMedias")
                                ),
                                tabPanel("Graficos FAC anuais",
                                         plotOutput("FACAnuais"),
                                         dataTableOutput("tabelaAnual")
                                ),
                                tabPanel("Graficos FAC mensais",
                                         selectInput ("lagMensalMAX", "lag mensal analisado:", choices = 1:12, selected = 1),
                                         plotOutput ("FACMensais"),
                                         dataTableOutput ("tabelaMensal")
                                )
                              )
                            )
                  )
)