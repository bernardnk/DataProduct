library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
        
        # Application title
        headerPanel("Logistic Regression - Probability of college admission"),
        
        # Sidebar with controls to select a rank, view the regression analysis output, and 
        # specify the number of observations to view.
        sidebarPanel(
                #textInput("caption", "Caption:", "Data Summary"),
                
                selectInput("rank", "Choose a rank:", 
                            choices = c("1", "2", "3", "4")),
                
                numericInput("obs", "Number of observations to view:", 10)
        ),
        
        
        # Show the caption, a summary of the dataset and an HTML table with
        # the requested number of observations
        mainPanel(
                h3(textOutput("caption")), 
                
                verbatimTextOutput("summary"), 
        
                tableOutput("view"),
                
                plotOutput('newHist')
        )
))

