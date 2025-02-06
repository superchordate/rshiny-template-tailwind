# labals and colors. 
make_chart_labels = c() # we'll add to this later. 
make_chart_label = function(x) if(x %in% names(make_chart_labels)) make_chart_labels[[x]] else x

make_chart_colors = c() # we'll add to this later. 
make_chart_color = function(x, y) if(paste0(x, y) %in% names(make_chart_colors)) make_chart_colors[[paste0(x, y)]] else '#000000'

# function for easily creating charts. 
make_chart = function(dt, x, y){

  idt = dt[, c(x, y)]
  labels = sapply(colnames(idt), make_chart_label)
  
  colnames(idt) = c('x', 'y')

  return(list(
      chart = list(type = 'scatter'),
      title = list(text = paste0(labels, collapse = ' vs. ')),
      xAxis = list(title = list(text = labels[1], enabled = TRUE)),
      yAxis = list(title = list(text = labels[2], enabled = TRUE), labels = list(enabled = TRUE)),
      # tooltip = list(format = list(fontSize = '10px')),
      series = list(list(
        data = idt,
        color = make_chart_color(x, y),
        dataLabels = list(enabled = FALSE)
      ))
    ))
}