# str(data.frame(state.x77))
# $ Population: num  3615 365 2212 2110 21198 ...
# $ Income    : num  3624 6315 4530 3378 5114 ...
# $ Illiteracy: num  2.1 1.5 1.8 1.9 1.1 0.7 1.1 0.9 1.3 2 ...
# $ Life.Exp  : num  69 69.3 70.5 70.7 71.7 ...
# $ Murder    : num  15.1 11.3 7.8 10.1 10.3 6.8 3.1 6.2 10.7 13.9 ...
# $ HS.Grad   : num  41.3 66.7 58.1 39.9 62.6 63.9 56 54.6 52.6 40.6 ...
# $ Frost     : num  20 152 15 65 20 166 139 103 11 60 ...
# $ Area      : num  50708 566432 113417 51945 156361 ...

states = data.frame(state.x77)

# https://www.shutterstock.com/blog/101-color-combinations-design-inspiration?customer_ID=48606055&utm_medium=email&campaign_ID=shutters.12467865&utm_campaign=CORE-IMAGE-NEWSLETTER-CONTENT-march-_-fourtyfive&launch_ID=11181965&utm_source=sstkemail
# 11. Color Whirl:
# #E3A4A5
# #BC5F6A
# #19B3B1
# #034b61
make_chart_colors[['AreaPopulation']] = "#034b61"
make_chart_colors[['IncomeLiteracy']] = "#19B3B1"
make_chart_colors[["MurderLife.Exp"]] = "#BC5F6A"
make_chart_colors[["FrostHS.Grad"]] = "#E3A4A5"

make_chart_labels$HS.Grad = "High School Graduation"
make_chart_labels$Life.Exp = "Life Expectancy"

output$states1 = renderUI({ hc_html('states1', make_chart(states, 'Area', 'Population')) })
output$states2 = renderUI({ hc_html('states2', make_chart(states, 'Income', 'Illiteracy')) })
output$states3 = renderUI({ hc_html('states3', make_chart(states, 'Murder', 'Life.Exp')) })
output$states4 = renderUI({ hc_html('states4', make_chart(states, 'Frost', 'HS.Grad')) })