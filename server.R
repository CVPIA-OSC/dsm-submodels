shinyServer(function(input, output) {
    callModule(adult_submodels_server, "app")
    callModule(juvenile_survival_submodels_server, "app")
})
