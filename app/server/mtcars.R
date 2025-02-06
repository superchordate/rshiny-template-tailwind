# print(str(mtcars))
#  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
#  $ disp: num  160 160 108 258 360 ...
#  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
#  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
#  $ qsec: num  16.5 17 18.6 19.4 17 ...
#  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
#  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
#  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
#  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...

# function for easily creating charts. 
mtcars_chart = function(x, y){

  idt = mtcars[, c(x, y)]
  
  colnames(idt) = c('x', 'y')
  labels = c(mcars_lables[[x]], mcars_lables[[y]])

  return(list(
      chart = list(type = 'scatter'),
      title = list(text = cc(labels, sep = ' vs. ')),
      xAxis = list(title = list(text = labels[1], enabled = TRUE)),
      yAxis = list(title = list(text = labels[2], enabled = TRUE), labels = list(enabled = TRUE)),
      # tooltip = list(format = list(fontSize = '10px')),
      series = list(list(
        data = idt,
        color = mcars_colors[[cc(x, y)]],
        dataLabels = list(enabled = FALSE)
      ))
    ))
}

# labals and colors. 
mcars_lables = c(
  cyl = "Cylinders",
  disp = "Displacement",
  hp = "Horsepower",
  drat = "Rear Axle Ratio",
  wt = "Weight",
  qsec = "1/4 Mile Time",
  vs = "V/S",
  am = "Transmission",
  gear = "Gears",
  carb = "Carburetors",
  mpg = "Miles per Gallon"
)

# https://www.shutterstock.com/blog/101-color-combinations-design-inspiration?customer_ID=48606055&utm_medium=email&campaign_ID=shutters.12467865&utm_campaign=CORE-IMAGE-NEWSLETTER-CONTENT-march-_-fourtyfive&launch_ID=11181965&utm_source=sstkemail
# 11. Color Whirl:
# #E3A4A5
# #BC5F6A
# #19B3B1
# #034b61
mcars_colors = c(
  cylhp = "#034b61",
  hpmpg = "#19B3B1",
  gearqsec = "#BC5F6A",
  dispwt = "#E3A4A5"
)

# the charts themselves. 
output$mtcars1 = renderUI({ hc_html('mtcars1', mtcars_chart('cyl', 'hp')) })
output$mtcars2 = renderUI({ hc_html('mtcars2', mtcars_chart('hp', 'mpg')) })
output$mtcars3 = renderUI({ hc_html('mtcars3', mtcars_chart('gear', 'qsec')) })
output$mtcars4 = renderUI({ hc_html('mtcars4', mtcars_chart('disp', 'wt')) })