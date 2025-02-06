server = function(input, output, session) {

  # the _init folder is set up to run first. useful when there are dependent files downstream.
  # source any files in app/server/_init/.
  initfiles = list.files('server/_init/', pattern = '[.][Rr]$', full.names = TRUE)
  for(i in initfiles) source(i, local = TRUE)
  rm(i)

  # then run the rest of the app/server/ files. 
  dofiles = list.files('server', pattern = '[.][Rr]$', recursive = TRUE, full.names = TRUE)
  dofiles = dofiles[!grepl('_init', dofiles)]
  for(i in dofiles) source(i, local = TRUE)
  rm(i)

}
