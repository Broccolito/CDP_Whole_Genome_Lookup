library(shiny)
library(knitr)
library(ggplot2)
library(shinydashboard)

example_allel = "2
2
0
2
0
2
1
1
2
2
1
1
2
1
1
0
1
2
2
1
2
0
0
1
1
2
0
2
1
1
0
0
2
1
2
1
1
2
1
1
"

p = read.csv(file = "physio_gene_mat.csv")

ui = dashboardPage(
  header = dashboardHeader(title = "SNP Lookup", titleWidth = 400),
  sidebar = dashboardSidebar(width = 400,
    textInput(inputId = "SNP_name", label = "Name of the SNP", placeholder = "chr1_67685387 OR  rs570553380"),
    textAreaInput(inputId = "allel_freq", label = "Allel Frequencies", placeholder = example_allel,
                  height = 700),
    actionButton(inputId = "run", label = "Analyse")
  ),
  body = dashboardBody(
    # fixedRow(
    column(4, tableOutput(outputId = "population_allel1")),
    column(4,tableOutput(outputId = "population_allel2")),
    # ),
    fixedRow(
      column(4,verbatimTextOutput(outputId = "SNP")),
      column(4,tableOutput(outputId = "regression_stat_overall")),
      column(4,tableOutput(outputId = "regression_stat_male")),
      column(4,tableOutput(outputId = "regression_stat_female"))
    ),
    plotOutput(outputId = "bp")
  )
)


server = function(input, output, session) {
  
  observeEvent(input$run, {
    output$SNP = renderText({
      paste0("Regressing [Hb] on ", input$SNP_name, ":")
    })
  })
  
  observeEvent(input$run, {
    tryCatch({
      af = data.frame(as.numeric(unlist(strsplit(input$allel_freq, split = "\n"))))
      colnames(af) = "af"
      data = cbind.data.frame(p, af)
      data_male = subset(data, sex == "M")
      data_female = subset(data, sex == "F")
      l = summary(lm(data = data, hb ~ af))$coefficient
      colnames(l) = c("Overall", "Std. Error", "t value", "Pr(>|t|)")
      l_male = summary(lm(data = data_male, hb ~ af))$coefficient
      colnames(l_male) = c("Male", "Std. Error", "t value", "Pr(>|t|)")
      l_female = summary(lm(data = data_female, hb ~ af))$coefficient
      colnames(l_female) = c("Female", "Std. Error", "t value", "Pr(>|t|)")
      output$population_allel1 = renderTable({
        data = data[1:18,c(1:5,11)]
        colnames(data) = c("id", "hb", "sex", "age", "bmi", input$SNP_name)
        data
      })
      output$population_allel2 = renderTable({
        data = data[19:36,c(1:5,11)]
        colnames(data) = c("id", "hb", "sex", "age", "bmi", input$SNP_name)
        data
      })
      output$regression_stat_overall = renderTable({l})
      output$regression_stat_male = renderTable({l_male})
      output$regression_stat_female = renderTable({l_female})
      
      output$bp = renderPlot({
        ggplot(data = data, aes(x = as.factor(af), y = hb)) + 
          geom_boxplot() +
          xlab(input$SNP_name) + 
          ylab("[Hb]") + 
          facet_grid(row = .~sex)
      })
    }, error = function(){
      return()
    })
  })
  
  onSessionEnded(function(){stopApp()})
}

shinyApp(ui, server)