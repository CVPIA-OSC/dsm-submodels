navbarPage(
    title = "DSM Submodels",
    
    tabPanel("Adult", adult_submodels_UI("app")),
    tabPanel("Juvenile Survival", juvenile_survival_submodels_UI("app")),
    tabPanel("Juvenile Route and Rear")
)
