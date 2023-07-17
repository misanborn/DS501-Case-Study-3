library(shiny)
library(readr)
library(class)
library(Hmisc)

# Normalization Helper
normalize <- function(x){ return ((x - min(x)) / (max(x) - min(x))) }


# K-Nearest Neighbors Function
knn_c <- function(x,k){
  norm <- as.data.frame(lapply(x, normalize))
  
  set.seed(123)
  data_spl <- sample(1:nrow(norm),size=nrow(norm)*0.7,replace = FALSE) 
  
  train <- x[data_spl,] # 70% training data
  test <- x[-data_spl,] # remaining 30% test data
  
  train_labels <- x[data_spl,5]
  test_labels <-x[-data_spl,5]
  knn_pred <- knn(train=train, test=test, cl=train$diag_tag, k=k)
  
  misClassError <- mean(knn_pred != test$diag_tag)
  
  return(list("knn"=knn_pred, 'error' =misClassError))
}

num_diff <- function(x,y){
  count = 0
  for(i in x){
    if(i == y[i]) {
      count = count + 1
    }
  }
   return(count/length(x)) 
}



Cancer_Data <- read_csv("Cancer_Data.csv")
Cancer_Data <- Cancer_Data[, -33]

Cancer_Data$diag_tag <- ifelse(Cancer_Data$diagnosis=="M",2, 1)

# Remove Categorical Data for Kmeans
vars <- setdiff(names(Cancer_Data), c( "id","diagnosis"))

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabsetPanel(
    tabPanel("Report",
             h4("K Means Clustering vs. K Nearest Neighbors: An Analysis of Two 
                Machine Learning Techniques for Oncological Applications"),
             br(),
             h5("DS 501"),
             h5("By: Madison Sanborn"),
             br(),
             p("Each year, over a million people globally are diagnosed with 
               cancer. Each individual patient’s prognosis is dependent on 
               several things including the type of cancer, any comorbidities 
               the patient may have, and their age, but one of the factors that 
               most affects the prognosis is how early the cancer is detected. 
               The earlier a patient can be diagnosed, the higher their chances
               of survival. However, diagnosing cancer in its earliest stages 
               can be very difficult, and mistakes are deadly. One of the 
               biggest factors slowing diagnosis is the development of benign 
               tumors. When most people hear the word “tumor” cancer is 
               automatically assumed, but this is actually incorrect. 
               Tumors themselves are simply groups of overgrown cells, 
               which can sometimes naturally occur and are not cancerous. 
               These are called benign tumors. Malignant tumors are those that 
               are cancerous, and pose significantly higher threats to patients.
               The determination of whether a tumor is benign or malignant is 
               extremely important, but can be difficult. Typically these 
               diagnoses are made by a combination of tests including blood work
               and invasive biopsies, which can all take a significant amount of
               time and money, ultimately delaying how quickly treatment can 
               begin. Currently a significant amount of research is being done 
               to analyze possible ways to speed up the diagnostic process, and
               one of the main research foci is using Machine learning. "),
             br(),
             p("machine learning is able to be trained on practice data sets to discover 
               relationships between different variables and can then be used to
               predict outcomes on data sets that it has never been seen before.
               This has enormous potential for use in aiding with cancer 
               diagnoses. For case study three I chose to take a firsthand look
               at the potential uses of machine learning in oncology, using a cancer data set 
               from Kaggle. This data set contains data on 570 different cancer 
               cells from tumor biopsies. The data includes 31 numeric metrics 
               on the cells including radius, concavity, texture, area, 
               smooth/roughness, and symmetry. The data set also includes 
               whether each of the cells is benign or malignant. It can be 
               assumed that cancerous cells have different features than 
               non-cancerous, but with so much data it is very difficult to 
               discover these effects and use them for diagnosis. However, it is
               possible to use machine learning algorithms to analyze the data and draw 
               conclusions much quicker."),
             br(),
             p("To analyze this cancer dataset, two different algorithms were 
               used to draw conclusions from the data, and compare whether a 
               clustering or classification method works best for this purpose.
               First, a k means clustering algorithm was used to look for 
               correlations between individual characteristics. This algorithm 
               takes in data and organizes the points into “k” clusters (for 
               this case only two clusters: benign and malignant). To accomplish
               this, two centroids are identified, and each data point is 
               assigned to one of the two clusters. Then the sum of squares is 
               calculated for each cluster, which is essentially the total 
               distance each point lies from the centroid. The points are then 
               reassigned to the clusters, and the sum of squares is calculated 
               again. This process is repeated until the minimum possible sum of
               square values is achieved, resulting in the most effective 
               centroids and their clusters. This clustering technique can yield
               highly accurate results, but it is limited by the fact that it 
               gets very complicated when there is more than one variable 
               involved. For this reason, this first analysis focused on looking
               for relationships between groups of two variables. A second 
               technique, k nearest neighbors (KNN), was used to look at all 31 
               characteristics of the cells."),
             br(),
             p("The k nearest neighbors (KNN) classification algorithm was used
                to further analyze the data, and assess which method would be 
                best for this data set. K nearest neighbors works similarly to 
                the k means clustering algorithm. The goal is to sort the data 
                set into different categories (benign and malignant), but KNN 
                looks at which category the neighbors of an unclassified data 
                point are assigned to in order to determine where the new point 
                belongs. This is typically accomplished by using the Euclidean 
                distance between points, which is the distance between them on a
                straight line. The number of neighbors analyzed is the “k” 
                value, which is preselected by the data scientist. Different 
                data sets will have differing values of k that make the 
                classification most accurate, so the experiment is run with 
                several values of k before determining which works best. The 
                accuracy of the model is determined simply by calculating the 
                percentage of points that were classified into the correct 
                category; in this case the percentage of cells that were 
                correctly diagnosed as benign or malignant."),
             br(),
             p("To visualize the results of this data analysis, a Shiny applicat
               ion was made using R that gives users the ability to alter the ex
               perimental inputs and observe how this effects the results. The a
               pp contains three pages: this first introductory section, a page 
               for the K means clustering algorithm, and one for the KNN. "),
             br(),
             p("Within the K means clustering page, there are two drop downs all
               owing the user to select any two of the cell characteristics to c
               ompare. Two plots will then appear: the top displaying the centro
               ids and clusters predicted by the machine learning K means algori
               thm, and the bottom showing the actual benign and malignant diagn
               oses. The page will also display the accuracy of the model in bet
               ween the two plots. This method of analysis is an effective way t
               o observe the correlations between two individual variables, but 
               it is not very accurate. The accuracy values are very low, with m
               ost combinations of variables only having the correct diagnoses p
               redicted less than 50% of the time. Some of the metrics seem to h
               ave higher correlations with accuracies around 70%, but I would s
               till not suggest that further effort be put into this model. "),
             br(),
             p("The last page of the application is dedicated to the KNN classif
               ication model. This page allows the user to select different valu
               es of K, and observe how this effects the accuracy of the machine
               learning algorithm. The prediction accuracy peaks at just over 96
               % when k is equal to 4,5, 7, or 8. This method allows for all of 
               the cell values to be compared at once, which significantly incre
               ases the accuracy of the prediction model. With an accuracy ratin
               g this high, this machine learning has substantial potential for 
               use in diagnosing cancer."),
             br(),
             p("With significant amounts of further research and development, it
               is very possible that machine learning classification models such as KNN could 
               play a key role in the early diagnosis of many different cancer t
               ypes. Using training data, the algorithm is able to learn which c
               haracteristics of cells mean they are likely benign or malignant,
               with prediction accuracy ratings of 96%. Although machine learning, machine lea
               rning, and data science is unlikely to be able to fully replace d
               iagnosticians, they have the potential to be extremely valuable t
               ools physicians can use to make their diagnoses easier, quicker, 
               and more accurate."),
             

             ),
    tabPanel("Clustering",
             
             titlePanel("Kmeans Clustering"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput('xcol', 'X Variable', vars),
                 selectInput('ycol', 'Y Variable', vars, selected = vars[[2]])
               ),
               
               mainPanel(
                 plotOutput(outputId = "kmeanPlot"),
                 br(),
                 textOutput("kmeans"),
                 br(),
                 plotOutput(outputId = "truePlot")
               ))
             ),
    
       tabPanel("Classification",
             titlePanel("K-Nearest Neighbor Classification"),
             sidebarLayout(
               sidebarPanel(
                 numericInput('k','K', 1, min=1, max=20)
               ),
               mainPanel(
                 textOutput("kpred"),
                 plotOutput(outputId = "knnPlot")
               )
             )
             
            
             
             )
    
    
  )
)


server <- function(input, output) {
  
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    Cancer_Data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), 2)
  })
  
  # km_acc <- reactive({
  #   
  # })
  
  output$kmeans <- renderText({
    paste("Accuracy = ",num_diff(clusters()$cluster, Cancer_Data$diag_tag))
    #paste(typeof(clusters))
  })
  
  
  
# Render Kmean Clusters  
  output$kmeanPlot  <- renderPlot({
    palette(c("#377EB8",  "#984EA3"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         main = 'Predicted Clusters',
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  
# Render Benign vs Malignant Plot
  output$truePlot <- renderPlot({
    palette(c( "#4DAF4A","#E41A1C"))
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         main = 'Benign vs Malignant',
         col = Cancer_Data$diag_tag,
         pch = 20, cex = 3)
    legend('bottomright',inset=0.05,c("Malignant","Benign"),pch=20,col=c("#E41A1C","#4DAF4A"))
  })
  
  # KNN
  knn_class <- reactive({
    knn_c(Cancer_Data[,3:33], input$k)
  })
  
  knn_list <- c()
  
  
  knn_data <- reactiveValues(x = integer(), y = double())
  observeEvent(input$k,{
    knn_data$x <- c(input$k, knn_data$x)
    knn_data$y <- c(1-knn_class()$error, knn_data$y)
  })
  output$kpred <- renderText({
    paste("Accuracy=",1-knn_class()$error)
  })
   
  output$knnPlot <- renderPlot({
    plot(knn_data,
         main = 'Benign vs Malignant Accuracy',
         xlab = "Value of K",
         ylab = "Accuracy",
         ylim = c(0.9,1),
         xlim= c(1,20),
         col = '#377EB8',
         pch = 20, cex = 1.5
    )
    text(knn_data$x, knn_data$y, round(knn_data$y, 4), cex=.6, pos=1, col= '#377EB8')
    minor.tick(nx=5, tick.ratio=.5)
  })
}


shinyApp(ui = ui, server = server)