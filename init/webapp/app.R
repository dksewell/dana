library(shiny) 

source("server.R")
source("ui.R")

# If you end up with an error along the lines of 'Error in X', 
# try loading dana in console:
# devtools::load_all("dana)

# To add devtools code snippet in Rstudio,
# Tools > edit Code Snippets >
# 
# snippet dana
# devtools::load_all("dana")
#
# To use in console, type dana and press tab

# Another way to run app:
#
# snippet ap 
# runApp('02_shiny_app')
#


shinyApp(ui = ui, server = server, options = list(port = 7990))



