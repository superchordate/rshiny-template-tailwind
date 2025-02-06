# logging and progress indicator functions that come in handy:

# a logging function for development that doesn't affect the deployed app.
log = function(x) if(islocal) cat(x, '\n')

# progress bar stuff.
# https://shiny.rstudio.com/articles/progress.html
progress = NULL

proginit = function(message = '', value = 0){
    if(is.null(progress)){
        progress = shiny::Progress$new()
        progress$set(message, value = value)
        progress <<- progress
    }
}

proginc = function(message = '', pct = 0.05) if(!is.null(progress)) {
    progress$inc(pct, detail = message)
}

progclose = function()if(!is.null(progress)){
    progress$close()
    progress <<- NULL
}

