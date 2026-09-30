source("server.R")
source("ui.R")

shinyApp(
    ui = ui, 
    server = server,
    options = list(port = 7990)
)