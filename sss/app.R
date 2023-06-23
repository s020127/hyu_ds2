library(shiny)
library(palmerpenguins)
library(ggplot2)
library(DT)

# UI 구성 요소를 정의합니다.
ui <- fluidPage(
  titlePanel("펭귄 데이터 분석"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("species", "펭귄 종류를  선택하세요", choices = unique(penguins$species)),
      selectInput("x_axis", "x축을 선택하세요.", choices = setdiff(colnames(penguins), c("species", "island", "sex", "year"))),
      selectInput("y_axis", "y축을 선택하세요.", choices = setdiff(colnames(penguins), c("species", "island", "sex", "year"))),
      sliderInput("point_size", "점 크기를 선택하세요", min = 1, max = 10, value = 5)
    ),
    mainPanel(
      DTOutput("datatable"),
      plotOutput("plot")
    )
  )
)

# 서버 함수를 정의합니다.
server <- function(input, output) {
  # 선택한 펭귄 종류와 x, y 축에 해당하는 데이터를 그래프로 출력합니다.
  output$plot <- renderPlot({
    filtered_data <- penguins[penguins$species %in% input$species, ]
    x <- filtered_data[[input$x_axis]]
    y <- filtered_data[[input$y_axis]]
    point_size <- input$point_size
    shape <- ifelse(filtered_data$sex == "male", "Male", "Female")
    
    ggplot(filtered_data, aes(x = x, y = y, shape = shape, color = species)) +
      geom_point(size = point_size) +
      labs(x = input$x_axis, y = input$y_axis) +
      scale_shape_manual(values = c(Male = 17, Female = 16)) +
      theme_minimal()
  })
  
  # 선택한 펭귄 데이터를 데이터 테이블로 출력합니다.
  output$datatable <- renderDT({
    filtered_data <- penguins[penguins$species %in% input$species, ]
    datatable(filtered_data)
  })
}

# 앱을 실행합니다.
shinyApp(ui = ui, server = server)
