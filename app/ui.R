uihead = function(){

    ihead = list(
        # Google fonts. Left here just in case - but it's better if you add these locally to www/.
        # HTML('<link rel="preconnect" href="https://fonts.gstatic.com">'),
        # HTML('<link href="https://fonts.googleapis.com/css2?family=Raleway+Dots&family=Work+Sans:wght@300;400;500;700&family=Lato:wght@300;400;500;700&display=swap" rel="stylesheet">')
    )

    # add CSS and JS files from www/
    files.www =  gsub('www/', '', list.files( 'www', full.names = TRUE, recursive = TRUE))
    files.css = files.www[grepl('[.]css$', files.www, ignore.case = TRUE )]
    files.js = files.www[grepl('[.]js$', files.www, ignore.case = TRUE )]
    for(icss in files.css ) ihead[[length(ihead) + 1]] = HTML(cc('<link rel="stylesheet" type="text/css" href="', icss, '">'))
    for(ijs in files.js) ihead[[length(ihead) + 1]] = HTML(cc('<script src="', ijs, '"></script>'))
    
    return(ihead)
    
}

# set up your tabs here. 
tabs = lapply(c(
  'Home'
  ), function(tab) tabPanel(title = tab, uiOutput(tab))
)
#tabs$selected = 'Make a Chart' # uncomment to set a default tab during developmnet. 

ui = function(...) div(
  
  # utility items. 
  div(
    tags$title('R Shiny + Tailwind CSS'),
    uihead(),
  ),

  # app container.
  div(class = 'h-lvh opensans bg-gray-50 text-gray-600',
    div(
      class = 'sticky top-0 z-50', 

      # header
      div(class = 'h-20 flex flex-wrap content-center bg-white drop-shadow-md pl-6',
        h1(class = 'opensans-bold text-xl', 'R Shiny + Tailwind CSS')
      ),
      
      div(
        class = 'h-full mb-4', 
        uiOutput('Home')
      )
  )

))

