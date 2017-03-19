library(shiny)
library(miniUI)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(qgraph)
library(plotly)
library(reshape2)
library(cluster)
library(rpart)
library(rpart.plot)

# Load data
data <- fread("../HR_comma_sep.csv", # Edit to the correct CSV
              col.names = c("Satisfaction Level",
                            "Last Evaluation Score",
                            "Number of Projects",
                            "Average Weekly Hours",
                            "Years in Company",
                            "Work Accident",
                            "Has Left",
                            "Promotion in Last 5 Years",
                            "Department",
                            "Salary"),
              colClasses = c(rep("numeric", 5), rep("factor", 5)))

# Preprocess data (adjust month->week hours, adjust factors)
data$`Average Weekly Hours` <- data$`Average Weekly Hours` / ((365 / 12) / 7)
levels(data$`Work Accident`) <- c("No Accident", "Accident")
levels(data$`Has Left`) <- c("Not Left", "Left")
levels(data$`Promotion in Last 5 Years`) <- c("No Promotion", "Had a Promotion")
levels(data$`Department`) <- c("IT", "Research and Development", "Accounting", "Human Resources", "Management", "Marketing", "Product Management", "Sales", "Support", "Technical")
data$`Department` <- factor(data$`Department`, c("Accounting", "Human Resources", "IT", "Management", "Marketing", "Product Management", "Research and Development", "Sales", "Support", "Technical"))
levels(data$`Salary`) <- c("High", "Low", "Medium")
data$`Salary` <- factor(data$`Salary`, c("Low", "Medium", "High"))

# Store a copy of the data
better_data <- copy(data)

ui <- miniPage(
  gadgetTitleBar("Human Resources Mini-App"),
  miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   sliderInput("feat_01", "Satisfaction Level", 0, 1, c(0, 1), sep = ""),
                   sliderInput("feat_02", "Last Evaluation Score", 0, 1, c(0, 1), sep = ""),
                   sliderInput("feat_03", "Number of Projects", 2, 7, c(2, 7), sep = ""),
                   sliderInput("feat_04", "Average Weekly Hours", 22, 72, c(22, 72), sep = ""),
                   sliderInput("feat_05", "Years in Company", 2, 10, c(2, 10), sep = ""),
                   selectInput("feat_06", "Work Accident", choices = c("No Accident", "Accident"), selected = c("No Accident", "Accident"), multiple = TRUE),
                   selectInput("feat_07", "Has Left", choices = c("Not Left", "Left"), selected = c("Not Left", "Left"), multiple = TRUE),
                   selectInput("feat_08", "Promotion in Last 5 Years", choices = c("No Promotion", "Had a Promotion"), selected = c("No Promotion", "Had a Promotion"), multiple = TRUE),
                   selectInput("feat_09", "Department", choices = c("Accounting", "Human Resources", "IT", "Management", "Marketing", "Product Management", "Research and Development", "Sales", "Support", "Technical"), selected = c("Accounting", "Human Resources", "IT", "Management", "Marketing", "Product Management", "Research and Development", "Sales", "Support", "Technical"), multiple = TRUE),
                   selectInput("feat_10", "Salary", choices = c("Low", "Medium", "High"), selected = c("Low", "Medium", "High"), multiple = TRUE),
                   selectInput("feat_corr", "Visualize", choices = c("Satisfaction Level", "Last Evaluation Score", "Number of Projects", "Average Weekly Hours", "Years in Company", "Work Accident", "Has Left", "Promotion in Last 5 Years", "Department: Accounting", "Department: Human Resources", "Department: IT", "Department: Management", "Department: Marketing", "Department: Product Management", "Department: Research and Development", "Department: Sales", "Department: Support", "Department: Technical", "Salary: Low", "Salary: Medium", "Salary: High"), selected = c("Satisfaction Level", "Last Evaluation Score", "Years in Company", "Department: Management"), multiple = TRUE),
                   checkboxInput("check_corr", "Show Values in Visualization", value = TRUE)
                 )
    ),
    miniTabPanel("Data", icon = icon("table"),
                 miniContentPanel(
                   dataTableOutput("table", height = "100%")
                 )
    ),
    miniTabPanel("Associations", icon = icon("bar-chart"),
                 miniContentPanel(
                   plotlyOutput("corrplot", height = "100%")
                 )
    ),
    miniTabPanel("Graph", icon = icon("snowflake-o"),
                 miniContentPanel(
                   plotOutput("graphplot", height = "100%")
                 )
    ),
    miniTabPanel("Tree Settings", icon = icon("tasks"),
                 miniContentPanel(
                   sliderInput("size", "Text Size:", min = 50, max = 150, value = 110, step = 1, round = TRUE, post = "%"),
                   selectInput("label", "Label Variable:", choices = colnames(data), selected = "Salary"),
                   selectInput("ban", "Feature Selection:", choices = colnames(data), selected = c("Satisfaction Level", "Last Evaluation Score", "Number of Projects", "Average Weekly Hours", "Years in Company"), multiple = TRUE),
                   sliderInput("max_depth", "Maximum Depth:", min = 1, max = 32, value = 3, step = 1, round = TRUE, post = " Levels"),
                   sliderInput("min_improve", "Pruning Strength:", min = 0, max = 0.01, value = 0.0001, step = 0.0001, post = " Strength"),
                   sliderInput("min_split", "EXPERT: Minimum Observations for a Node:", min = 1, max = 100, value = 20, step = 1, round = TRUE, post = " Obs./Node"),
                   sliderInput("min_bucket", "EXPERT: Minimum Observations for a Leaf:", min = 1, max = 100, value = 7, step = 1, round = TRUE, post = " Obs./Leaf"),
                   sliderInput("surrogate_search", "EXPERT: Number of Surrogates:", min = 1, max = 100, value = 5, step = 1, round = TRUE, post = " Surrogates"),
                   sliderInput("surrogate_type", "EXPERT: Surrogate Type (0, 1, 2):", min = 0, max = 2, value = 2, step = 1, round = TRUE, pre = "Type ", post = " Surrogate"),
                   sliderInput("surrogate_style", "EXPERT: Surrogate Style (0, 1):", min = 0, max = 1, value = 1, step = 1, round = TRUE, pre = "Style ", post = " Surrogate"),
                   numericInput("seed", "EXPERT: Seed:", value = 0)
                 )
    ),
    miniTabPanel("Explain", icon = icon("tree"),
                 miniContentPanel(
                   plotOutput("tree", height = "100%")
                 )
    )
  )
)

server <- function(input, output, session) {
  
  # Data parser function: decimate/filter rows (SQL WHERE, IN)
  parse_data <- function(data, feature_list) {
    
    # Create temporary data.frame
    temp_data <- matrix(nrow = nrow(data), ncol = 10)
    
    # Loop through each continuous feature to find if it fits
    for (i in 1:5) {
      temp_data[, i] <- (data[[i]] >= feature_list[[i]][1]) & (data[[i]] <= feature_list[[i]][2])
    }
    
    # Loop through each discrete feature to find if it fits
    for (i in 6:10) {
      temp_data[, i] <- data[[i]] %in% feature_list[[i]]
    }
    
    # Debugging purposes
    # print(sum(rowSums(temp_data) == 10))
    
    # Return only rows matching ALL criterias
    return(rowSums(temp_data) == 10)
    
    # return(rep(TRUE, nrow(data)))
    
  }
  
  # Decimate/Filter data and store it
  better_data <- reactive({
    copy(data[which(parse_data(data, list(input$feat_01,
                                          input$feat_02,
                                          input$feat_03,
                                          input$feat_04,
                                          input$feat_05,
                                          input$feat_06,
                                          input$feat_07,
                                          input$feat_08,
                                          input$feat_09,
                                          input$feat_10)) == TRUE), ])
  })
  
  # Create distance matrix
  dist_data <- reactive({
    
    temp_data <- copy(better_data())
    temp_data$`Work Accident` <- as.numeric(temp_data$`Work Accident`) - 1
    temp_data$`Has Left` <- as.numeric(temp_data$`Has Left`) - 1
    temp_data$`Promotion in Last 5 Years` <- as.numeric(temp_data$`Promotion in Last 5 Years`) - 1
    temp_data$`Salary Low` <- as.numeric(temp_data$`Salary` == "Low")
    temp_data$`Salary Medium` <- as.numeric(temp_data$`Salary` == "Medium")
    temp_data$`Salary High` <- as.numeric(temp_data$`Salary` == "High")
    temp_data$`Salary` <- NULL
    levels(temp_data$`Department`) <- c("Accounting", "HR", "IT", "Mgmt", "Marketing", "Product Mgmt", "R&D", "Sales", "Support", "Tech")
    temp_data <- model.matrix(~.+0, data = temp_data)
    
    # Rescale 0-1 numeric
    for (i in 3:5) {
      #temp_data[, i] <- (temp_data[, i] - min(temp_data[, i])) / (max(temp_data[, i]) - min(temp_data[, i]))
      temp_data[, i] <- temp_data[, i] / max(temp_data[, i])
    }
    
    # Rescale 9-18 to 1/10
    for (i in 9:18) {
      temp_data[, i] <- temp_data[, i] / 10
    }
    
    # Rescale 19:21 to 1/3
    for (i in 19:21) {
      temp_data[, i] <- temp_data[, i] / 3
    }
    
    features_selected <- which(c("Satisfaction Level", "Last Evaluation Score", "Number of Projects", "Average Weekly Hours", "Years in Company", "Work Accident", "Has Left", "Promotion in Last 5 Years", "Department: Accounting", "Department: Human Resources", "Department: IT", "Department: Management", "Department: Marketing", "Department: Product Management", "Department: Research and Development", "Department: Sales", "Department: Support", "Department: Technical", "Salary: Low", "Salary: Medium", "Salary: High") %in% input$feat_corr)
    
    plot_data <- as.matrix(daisy(t(temp_data[, features_selected]), metric = "manhattan"))
    
    return(plot_data)
    
  })
  
  # Return table to print
  output$table <- renderDataTable({
    datatable(better_data()) %>%
      formatStyle(c("Satisfaction Level", "Last Evaluation Score"),
                  background = styleColorBar(c(0, 1), color = "lightgreen"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Number of Projects",
                  background = styleColorBar(c(0, 7), color = "lightblue"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatRound("Average Weekly Hours", digits = 2) %>%
      formatStyle("Average Weekly Hours",
                  background = styleColorBar(c(0, 72), color = "lightblue"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Years in Company",
                  background = styleColorBar(c(0, 10), color = "lightblue"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle(c("Work Accident"),
                  backgroundColor = styleEqual(c("No Accident", "Accident"), c("lightgrey", "pink"))) %>%
      formatStyle("Has Left",
                  backgroundColor = styleEqual(c("Not Left", "Left"), c("lightgrey", "pink"))) %>%
      formatStyle("Promotion in Last 5 Years",
                  backgroundColor = styleEqual(c("No Promotion", "Promotion"), c("lightgrey", "orange"))) %>%
      formatStyle("Salary",
                  backgroundColor = styleEqual(c("Low", "Medium", "High"), c("orange", "yellow", "lightblue")))
  })
  
  # Add Correlation plot
  output$corrplot <- renderPlotly({
    
    plot_data <- dist_data()
    plot_data[upper.tri(plot_data)] <- NA
    
    plot_data <- melt(plot_data, na.rm = TRUE)
    plot_data$value <- plot_data$value / nrow(better_data())
    plot_data$value <- -(plot_data$value - 0.5) * 2
    colnames(plot_data) <- c("Variable_1", "Variable_2", "Agreement")
    
    if (input$check_corr == TRUE) {
      
      plot_data$Text <- sprintf("%0.2f", round(plot_data$Agreement, digits = 2))
      return(ggplotly(ggplot(data = plot_data, aes_string(x = "Variable_1", y = "Variable_2", fill = "Agreement")) + geom_tile(color = "white") + geom_text(aes_string(x = "Variable_1", y = "Variable_2", label = "Text")) + scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Agreement<br>Strength") + theme_bw() + theme(axis.text.x = element_text(angle = 45)) + labs(x = "Variable 1", y = "Variable 2"), autosize = TRUE, margin = list(l = 20, r = 20, b = 250, t = 20, p = 4)))
      
    } else {
      
      return(ggplotly(ggplot(data = plot_data, aes_string(x = "Variable_1", y = "Variable_2", fill = "Agreement")) + geom_tile(color = "white") + scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Agreement<br>Strength") + theme_bw() + theme(axis.text.x = element_text(angle = 45)) + labs(x = "Variable 1", y = "Variable 2"), autosize = TRUE, margin = list(l = 20, r = 20, b = 250, t = 20, p = 4)))
      
    }
  })
  
  # Add Graph plot
  output$graphplot <- renderPlot({
    
    plot_data <- dist_data()
    plot_data <- plot_data / nrow(better_data())
    plot_data <- -(plot_data - 0.5) * 2
    
    features_name <- c("Satisfaction Level", "Last Evaluation Score", "Number of Projects", "Average Weekly Hours", "Years in Company", "Work Accident", "Has Left", "Promotion in Last 5 Years", "Department: Accounting", "Department: Human Resources", "Department: IT", "Department: Management", "Department: Marketing", "Department: Product Management", "Department: Research and Development", "Department: Sales", "Department: Support", "Department: Technical", "Salary: Low", "Salary: Medium", "Salary: High")
    
    features_selected <- which(features_name %in% input$feat_corr)
    
    colnames(plot_data) <- c("A1", "A2", "B", "C", "D", "E", "F", "G", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "I1", "I2", "I3")[features_selected]
    
    features_name <- c("A1: Satisfaction Level", "A2: Last Evaluation Score", "B: Number of Projects", "C: Average Weekly Hours", "D: Years in Company", "E: Work Accident", "F: Has Left", "G: Promotion in Last 5 Years", "H1 Department: Accounting", "H2 Department: Human Resources", "H3 Department: IT", "H4 Department: Management", "H5 Department: Marketing", "H6 Department: Product Management", "H7 Department: Research and Development", "H8 Department: Sales", "H9 Department: Support", "H10 Department: Technical", "I1 Salary: Low", "I2 Salary: Medium", "I3 Salary: High")
    
    qgraph(plot_data, layout = "spring", groups = features_name[features_selected], palette = "pastel", theme = "classic", shape = "ellipse", borders = FALSE, vTrans = 100, vsize = 12, title = paste0("Agreement: [", paste(sprintf("%.03f", range(plot_data)), collapse = ", "), "]"), edge.labels = TRUE, XKCD = TRUE)
    
  })
  
  # Plot tree
  output$tree <- renderPlot({
    tree_data <- copy(better_data())
    levels(tree_data$`Department`) <- c("Accounting", "HR", "IT", "Mgmt", "Marketing", "Product Mgmt", "R&D", "Sales", "Support", "Tech")
    tree_label <- copy(tree_data[[input$label]])
    #tree_data[[input$label]] <- NULL
    tree_data <- tree_data[, unique(c(input$ban, input$label)), with = FALSE]
    #tree_data <- tree_data[, input$ban[which(!input$ban %in% input$label)], with = FALSE]
    formula <- reformulate(termlabels = paste0("`", input$ban[which(!input$ban %in% input$label)], "`"), response = input$label)
    
    temp_model <- rpart(formula = formula,
                        data = tree_data,
                        method = ifelse(input$label %in% c("Satisfaction Level", "Last Evaluation Score", "Average Weekly Hours"), "anova", ifelse(input$label %in% c("Number of Projects", "Years in Company"), "poisson", "class")),
                        control = rpart.control(minsplit = input$min_split,
                                                minbucket = input$min_bucket,
                                                cp = input$min_improve,
                                                maxcompete = 0,
                                                maxsurrogate = input$surrogate_search,
                                                usesurrogate = input$surrogate_type,
                                                xval = 3,
                                                surrogatestyle = input$surrogate_style,
                                                maxdepth = input$max_depth))
    
    # temp_model <- Laurae::FeatureLookup(data = tree_data,
    #                                     label = tree_label,
    #                                     ban = NULL,
    #                                     antiban = FALSE,
    #                                     type = ifelse(input$label %in% c("Satisfaction Level", "Last Evaluation Score", "Average Weekly Hours"), "anova", ifelse(input$label %in% c("Number of Projects", "Years in Company"), "poisson", "class")),
    #                                     split = "information",
    #                                     folds = 3,
    #                                     seed = input$seed,
    #                                     verbose = FALSE,
    #                                     plots = FALSE,
    #                                     max_depth = input$max_depth,
    #                                     min_split = input$min_split,
    #                                     min_bucket = input$min_bucket,
    #                                     min_improve = input$min_improve,
    #                                     competing_splits = 0,
    #                                     surrogate_search = input$surrogate_search,
    #                                     surrogate_type = input$surrogate_type,
    #                                     surrogate_style = input$surrogate_style)
    
    rpart.plot(temp_model, main = "Decision Tree", tweak = input$size/100)
  })
  
  # Need to stop using a button?
  observeEvent(input$done, {
    stopApp(TRUE)
  })
  
}

shinyApp(ui, server, options = list(host = "0.0.0.0")) # Fully fledged version

# runGadget(shinyApp(ui, server), viewer = paneViewer()) # Viewer in RStudio (local only)
