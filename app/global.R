require(easyr)
require(shiny)
easyr::begin() # perform best-practice operations like clearing variables. runs functions in an app/fun/ folder. 

enableBookmarking(store = "url")
disable_exhibit_cache = FALSE

# check if the app is running locally. helpful for development. 
islocal = Sys.getenv('SHINY_PORT') == ""

# source any files in the app/global/ folder. 
for(i in list.files('global', pattern = '[.][Rr]', recursive = TRUE, full.names = TRUE)) source(i, local = TRUE)
rm(i)

