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

# https://www.shutterstock.com/blog/101-color-combinations-design-inspiration?customer_ID=48606055&utm_medium=email&campaign_ID=shutters.12467865&utm_campaign=CORE-IMAGE-NEWSLETTER-CONTENT-march-_-fourtyfive&launch_ID=11181965&utm_source=sstkemail
# 11. Color Whirl:
# #E3A4A5
# #BC5F6A
# #19B3B1
# #034b61
make_chart_colors[['cylhp']] = "#034b61"
make_chart_colors[['hpmpg']] = "#19B3B1"
make_chart_colors[['gearqsec']] = "#BC5F6A"
make_chart_colors[['dispwt']] = "#E3A4A5"

make_chart_labels$cyl = "Cylinders"
make_chart_labels$disp = "Displacement"
make_chart_labels$hp = "Horsepower"
make_chart_labels$drat = "Rear Axle Ratio"
make_chart_labels$wt = "Weight"
make_chart_labels$qsec = "1/4 Mile Time"
make_chart_labels$vs = "V/S"
make_chart_labels$am = "Transmission"
make_chart_labels$gear = "Gears"
make_chart_labels$carb = "Carburetors"
make_chart_labels$mpg = "Miles per Gallon"

output$mtcars1 = renderUI({ hc_html('mtcars1', make_chart(mtcars, 'cyl', 'hp')) })
output$mtcars2 = renderUI({ hc_html('mtcars2', make_chart(mtcars, 'hp', 'mpg')) })
output$mtcars3 = renderUI({ hc_html('mtcars3', make_chart(mtcars, 'gear', 'qsec')) })
output$mtcars4 = renderUI({ hc_html('mtcars4', make_chart(mtcars, 'disp', 'wt')) })