library(shiny)

# Define student names based on your provided list
students <- c("Sanjida Alam", "Aidan Caron", "Aedan Coulter", "Brody Fish", 
              "Antonio Galvao da Fonseca", "Sasha Gannon", "JJ Gifford", "Denys Godwin", 
              "Sophie Hayes", "Noah Kantor", "Pacifique Madibi","Kurt McAuliffe","Kalu Okigwe",
              "Sunita Phuyal","Annan Shrestha","Bikal Shrestha", "Tevita Soqo", "Annemaire Walsh")

library(shiny)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Student Call System"),
  sidebarLayout(
    sidebarPanel(
      actionButton("call_button", "Call a Random Student"),
      actionButton("show_history", "Show Call History"),
      actionButton("histogram_btn", "Show Histogram")
    ),
    mainPanel(
      verbatimTextOutput("selected_student"),
      tableOutput("history_table"),
      plotOutput("histogram_plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store called students
  called_students <- reactiveVal(character(0))
  
  # Action for calling a random student
  observeEvent(input$call_button, {
    chosen_student <- sample(students, 1)
    new_list <- c(called_students(), chosen_student)
    called_students(new_list)
    output$selected_student <- renderText({
      paste("Last Called Student:", chosen_student)
    })
  })
  
  # Show call history
  observeEvent(input$show_history, {
    output$history_table <- renderTable({
      if (length(called_students()) == 0){
        data.frame("Called Students" = "No students called yet.")
      } else {
        data.frame("Called Students" = called_students())
      }
    })
  })
  
  # Plot histogram showing frequency of students being called
  observeEvent(input$histogram_btn, {
    output$histogram_plot <- renderPlot({
      if(length(called_students()) > 0){
        hist_data <- table(called_students())
        
        par(mar=c(10,5,4,2))
        
        barplot(hist_data, 
                main="Frequency Histogram",
                ylab="Frequency",
                las=2, 
                col="skyblue",
                cex.names=0.8)
        
        mtext("Students", side=1, line=8, cex=1)
      } else {
        plot.new()
        text(0.5, 0.5, "No students have been called yet.")
      }
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
