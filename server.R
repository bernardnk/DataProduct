library(shiny)
library(datasets)
library(ggplot2)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
        
        # By declaring rank as a reactive expression we ensure that:
        #
        #  1) It is only called when the inputs it depends on changes
        #  2) The computation and result are shared by all the callers (it 
        #     only executes a single time)
        #
        rankInput <- reactive({
                switch(input$rank,
                       "1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4")
        })
        
        # The output$caption is computed based on a reactive expression that
        # returns input$caption. When the user changes the "caption" field:
        #
        #  1) This expression is automatically called to recompute the output 
        #  2) The new caption is pushed back to the browser for re-display
        # 
        # Note that because the data-oriented reactive expressions below don't 
        # depend on input$caption, those expressions are NOT called when 
        # input$caption changes.
        output$caption <- renderText({
                input$caption
        })
        
        mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
        mydata$rank <- factor(mydata$rank)
        mydata$admit <- factor(mydata$admit)
        fit <- glm(admit~gre+gpa+rank,data=mydata,family=binomial())

        mydata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

        mydata3 <- cbind(mydata2, predict.glm(fit, newdata = mydata2, type = "link", se = TRUE))
        mydata3 <- within(mydata3, {
                PredictedProb <- plogis(fit)
                LL <- plogis(fit - (1.96 * se.fit))
                UL <- plogis(fit + (1.96 * se.fit))
                })

# The output$summary depends on the rankInput reactive expression, 
# so will be re-executed whenever rankInput is invalidated
# (i.e. whenever the input$rank changes)
output$summary <- renderPrint({
        displayData <- mydata3[mydata3$rank == rankInput(),]
        drops <- c("fit","se.fit","residual.scale","UL","LL","rank")
        displayData <- displayData[,!(names(displayData) %in% drops)]
        summary(displayData)
})


        output$newHist <- renderPlot({
                ggplot(mydata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                        ymax = UL, fill = rank), alpha = 0.2) + 
                        geom_line(aes(colour = rank),
                        size = 1)
                        })
        
        # The output$view depends on both the databaseInput reactive expression
        # and input$obs, so will be re-executed whenever input$rank or 
        # input$obs is changed. 
        output$view <- renderTable({
                drops <- c("fit","se.fit","residual.scale","UL","LL")
                displayData <- mydata3[,!(names(mydata3) %in% drops)]
                head(displayData[displayData$rank == rankInput(),], n = input$obs)
        })
})
