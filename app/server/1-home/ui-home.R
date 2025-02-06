# tab names come from ui.R object `tabs`
output$Home = renderUI(div(class = 'w-full p-4 space-4 flex',

    div(
        class = 'p-4 rounded-lg shadow-md bg-white pt-6 flex justify-center', 
        style = 'width: 375px; ', 
        div(

        div(class = 'mb-4', sliderInput(
            inputId = "bins",
            label = "Number of bins:",
            min = 1,
            max = 50,
            value = 30
        )),

        selectInput(
            inputId = 'color',
            label = 'Color',
            choices = c('Emerald' = '#085438', 'Pharlap' = '#af8f90', 'Eggplant' = '#69454f', 'Calypso' = '#2a6078')
        )
    )),

    div(
        class = 'p-4 rounded-lg shadow-md bg-white box-border ml-6 flex-grow',
        plotOutput('distPlot')
    )

))

 output$distPlot = renderPlot({
 
    x = faithful$waiting
    bins = 

    hist(
        x, 
        breaks = seq(min(x), max(x), length.out = input$bins + 1), 
        col = input$color, 
        border = "white",
        xlab = "Waiting time to next eruption (in mins)",
        main = "Histogram of waiting times"
    )

})
