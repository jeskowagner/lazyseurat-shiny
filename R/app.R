library(shiny)
library(ggplot2)
library(lazyseurat)
# Avoid leaking error message details to users
options(shiny.sanitize.errors = TRUE)

con <- get_connection("seurat.duckdb")
initalizeOptions(con)
#######
# App #
#######

ui <- fluidPage(
    titlePanel("Mouse Pulmonary Hypertension scRNA-seq"),
    tabsetPanel(
        tabPanel(
            strong("Dataset Info"),
            br(), br(),
            fluidRow(
                column(
                    12, p("This single-cell RNA-sequencing dataset has been generated using endothelial cells isolated from an in vivo model of Sugen 5416/Hypoxia-induced Pulmonary Hypertension in mice, and control mice. In order to specifically study endothelial cells, a tamoxifen-inducible Cdh5-CreERT2-TdTomato transgenic mouse line was used."),
                    HTML("<p>The figure below shows the experimental design of this dataset. More information is available in our <a href='https://doi.org/10.1093/cvr/cvab296'>publication</a>.<p>")
                )
            ), hr(),
            fluidRow(
                column(12, h4(strong("Experimental Design")))
            ), br(),
            fluidRow(
                column(12, imageOutput("exp_design"))
            )
        ),
        tabPanel(
            strong("UMAP"),
            fluidRow(
                headerPanel(h3(strong("Integrated Control + PAH"))),
                column(5, imageOutput("umap")),
                column(7, mainPanel(plotOutput("plot_gene"), width = "100%"))
            ),
            hr(),
            fluidRow(
                column(3, checkboxGroupInput(
                    inputId = "vessel", label = "Select endothelial population:",
                    choices = list("Capillary A", "Capillary B", "Artery", "Vein", "Lymphatic", "Proliferating", "Sftp+"),
                    selected = list("Capillary A", "Capillary B", "Artery", "Vein", "Lymphatic", "Proliferating", "Sftp+")
                )),
                column(3, checkboxInput(inputId = "replicates", label = "Show replicates", value = TRUE)),
                column(3, selectizeInput(inputId = "gene", label = "Enter gene name:", choices = NULL, options = list(maxItems = 1, maxOptions = 5))),
                column(3, downloadButton("download.plot", label = "Download plot"))
            )
        ),
        tabPanel(
            strong("Arteriovenous Axis"),
            headerPanel(h3(strong("Integrated Control and PAH"))),
            sidebarLayout(
                sidebarPanel(
                    p("An error message will be shown if the selected gene is not expressed in any cell."),
                    selectizeInput(inputId = "gene2", label = "Enter gene name:", choices = NULL, options = list(maxItems = 1, maxOptions = 5)),
                    hr(), downloadButton("download.axis", label = "Download plot")
                ),
                mainPanel(
                    plotOutput("plot_axis"), br(),
                    imageOutput("arteriovenous_axis")
                )
            )
        ),
        tabPanel(
            strong("Markers"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    radioButtons(inputId = "dataset2", label = "Choose a dataset:", choices = c("Control", "PAH", "Control and PAH")),
                    hr(), downloadButton("download.table", label = "Download table")
                ),
                mainPanel(
                    DT::dataTableOutput("markers")
                )
            )
        )
    )
)


server <- function(input, output, session) {

    ########
    # UMAP #
    ########
    output$exp_design <- renderImage(list(src = EXP_DESIGN_PNG), deleteFile = FALSE)

    output$umap <- renderImage(list(src = UMAP_PNG, width = 390, height = 400), deleteFile = FALSE)

    updateSelectizeInput(session, "gene", choices = gene.list, server = TRUE)

    output$plot_gene <- renderPlot({
        plotGene(file = PAH_ARROW, gene = input$gene, vessel = input$vessel, replicates = input$replicates)
    })

    output$download.plot <- downloadHandler(
        filename = function() {
            paste(input$gene, "_plot.png", sep = "")
        },
        content = function(file) {
            ggsave(file,
                plotGene(
                    file = PAH_ARROW,
                    gene = input$gene,
                    vessel = input$vessel,
                    replicates = input$replicates
                ),
                width = 10,
                height = 8,
                units = "in"
            )
        }
    )

    ######################
    # ARTERIOVENOUS AXIS #
    ######################
    output$arteriovenous_axis <- renderImage(list(src = ARTERIOVENOUS_PNG), deleteFile = FALSE)

    updateSelectizeInput(session, "gene2", choices = gene.list, server = TRUE)

    output$plot_axis <- renderPlot({
        plotAxis(data = PSEUDOTIME_ARROW, gene = input$gene2)
    })

    output$download.axis <- downloadHandler(
        filename = function() {
            paste(input$gene2, "_axis.plot.png", sep = "")
        },
        content = function(file) {
            ggsave(file, plotAxis(), width = 20, height = 8, units = "in")
        }
    )

    ###########
    # MARKERS #
    ###########

    dataInput2 <- reactive(
        switch(input$dataset2,
            "Control" = control_marker.names,
            "PAH" = pah_marker.names,
            "Control and PAH" = pah.combined_marker.names
        )
    )

    output$markers <- DT::renderDataTable({
        dataInput2()
    })

    output$download.table <- downloadHandler(
        filename = function() {
            paste(input$dataset2, "_marker.genes.csv", sep = "")
        },
        content = function(file) {
            write.csv(dataInput2(), file, row.names = FALSE)
        }
    )
}

shinyApp(ui = ui, server = server)
