# C:/Users/KaiserD/Desktop/Version_Githup_modified_ver_1-2/test_case_2_data_delta
library(shiny)
for(i in list.files("help_functions", pattern = ".R$", full.names = TRUE)) { source(i, local = TRUE) } 
for(i in list.files("user_functions", pattern = ".R$", full.names = TRUE)) { source(i, local = TRUE) }

# ---- Define UI ----
ui <- fluidPage(
    
    # Application title
    titlePanel("RrrEC"),
    
    sidebarLayout(
        sidebarPanel(
            wellPanel( 
                #fileInput("input_file", "upload a file"),
                textInput("input_path", "setup input", 
                          value = "test_case_2_data_delta/", 
                          placeholder = "path to setup"),
                actionButton("action1", "calculate & plot")
            ),
            wellPanel( 
                numericInput("lambda", "'smoothing' parameter lambda", value = 1),
                numericInput("alpha_min", "log_10(alpha_min)", value = 8),
                numericInput("alpha_max", "log_10(alpha_max)", value = 15),
                numericInput("N_alpha", "N_alpha", value = 301),
                numericInput("N_c", "n grid points", value = 101)
            ),
            wellPanel( # setting the boundary conditions for the nutrient concentration
                radioButtons("bnd_cond_type_z_min", "top boundary condition", choices = list("concentration" = 1, "derivative" = 2), selected = 1),
                numericInput("bnd_cond_C_z_min", "concentration or derivative at top", value = 25e3),
                radioButtons("bnd_cond_type_z_max", "bottom boundary condition", choices = list("concentration" = 1, "derivative" = 2), selected = 1),
                numericInput("bnd_cond_C_z_max", "concentration or derivative at bottom", value = 5e3),
                numericInput("C_water", "concentration in water", value = 25e3)
            )
        ),
        
        mainPanel(
            wellPanel( # plots
                #helpText("results plots"),
                plotOutput("resultsPlot"),
                plotOutput("alphaPlot"),
                plotOutput("inputPlot")
            ),
            wellPanel( # tables
                helpText("boundary fluxes"),
                tableOutput("boundaryFluxes")
            ),
            wellPanel(
                helpText("concentrations and rates on the N_c grid"),
                tableOutput("outputData")
            )
        )
    )
)

# ---- Define server ----
server <- function(input, output) {
    
    observeEvent(input$action1, {
        # data
        df <- reactive({
            import_from_setup(isolate(input$input_path))
        })
        # rec
        rec_out <- reactive({
            rec(original_data       = df(),
                N_c                 = input$N_c, 
                C_water             = input$C_water,    
                lambda              = input$lambda,
                alpha_min           = input$alpha_min,
                alpha_max           = input$alpha_max,
                N_alpha             = input$N_alpha,
                bnd_cond_type_z_min = input$bnd_cond_type_z_min,
                bnd_cond_C_z_min    = input$bnd_cond_C_z_min,
                bnd_cond_type_z_max = input$bnd_cond_type_z_max,
                bnd_cond_C_z_max    = input$bnd_cond_C_z_max)
        })  
        # plots
        output$resultsPlot <- renderPlot({plot_rec(rec_out())})
        output$alphaPlot <- renderPlot({plot_rec(rec_out(), type = "localmin")})    
        output$inputPlot <- renderPlot({ plot_rec(rec_out(), type = "input") })
        # tables
        fluxes <- boundary_fluxes(rec_out(), explain = FALSE)
        fluxes <- setNames(data.frame(t(fluxes[,-(1:2)])), fluxes[,1])
        output$boundaryFluxes <- renderTable({fluxes}, rownames = TRUE, digits = -3)
        outdat <- rec_out()$output_data
        outdat$conc <- format(outdat$conc, scientific = TRUE, digits = 3)
        outdat$rate <- format(outdat$rate, scientific = TRUE, digits = 3)
        output$outputData <- renderTable({outdat})
    }) # end observeEvent
}

# ---- Run the application ---- 
shinyApp(ui = ui, server = server)
