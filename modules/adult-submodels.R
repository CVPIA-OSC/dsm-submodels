adult_submodels_UI <- function(id) {
    ns <- NS(id)
    
    sidebarLayout(
        sidebarPanel = sidebarPanel("Submodels"), 
        mainPanel = mainPanel()
    )
}

adult_submodels_server <- function(input, output, session) {
    
}
