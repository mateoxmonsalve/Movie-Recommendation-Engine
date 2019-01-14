# Movie-Recommendation-Engine
Class project using R and the IMBD database to create a content based movie recommendation engine. It takes the name of ine film you like as an input and recommends 5 films plus their general information as output.


#################################################    UI   #######################################################################
library(shiny)
library(shinyWidgets)
library(shinythemes)
# Define UI for app that draws a histogram ----
ui <- fluidPage(div(titlePanel("Please Enter Movie ID"), style = "color:white"),theme = shinytheme("cyborg"),
                 
                 sidebarLayout(
                   sidebarPanel(img(src = "Logo.png", height = 300, width = 300),
                     selectInput("MovieID", "Select Movie", choices = movie_raw_data$id),
                     setBackgroundColor(color = "black")
                     
                   ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Your Choice",
                                div(h2("Recommendations For you"), style = "color:white"),
                                div(strong(tableOutput("ChoiceData"),style = "color:white")),
                                setBackgroundImage(src = "photo.png")),
                     tabPanel("Results",
                     div(h2("Recommendations For you"), style = "color:white"),
                     div(strong(tableOutput("MovieData"),style = "color:white")),
                     setBackgroundImage(src = "photo.png")),
                     tabPanel("Summary",
                       div(h2("Recommendations For you"), style = "color:white"),
                       div(strong(tableOutput("SummaryData"),style = "color:white")),
                       setBackgroundImage(src = "photo.png")),
                     tabPanel("Information",
                              div(h2("Recommendations For you"), style = "color:white"),
                              div(strong(tableOutput("InformationData"),style = "color:white")),
                              setBackgroundImage(src = "photo.png"))
                   )
                  )
                 )
)

#########################################    Server   #############################################################

server <- function(input, output) {
  output$ChoiceData <- renderTable({
    n<-input$MovieID
    
    for(x in 1:nrow(movie_raw_data))
    {
      
      if(n %in% movie_raw_data[x,1])          
      {
        vtest <- as.data.frame(movie_raw_data[x,])
      }
      else
        
        x+1
    }
    
    vtest<- as.data.frame(vtest)
    print(vtest[,1:2])
  })
  
  output$MovieData <- renderTable({
    n<-input$MovieID
    
    for(x in 1:nrow(movie_raw_data))
    {
      
      if(n %in% movie_raw_data[x,1])          
      {
        vtest <- as.data.frame(movie_raw_data[x,])
      }
      else
        
        x+1
    }
    
    class(vtest)
    vtest<- as.matrix(vtest)
    
    
    #####################################################################################################################
    ############################################# Keyword Analysis######################################################
    
    #define keyword test vector
    vtest_keyword<-as.vector(vtest[,30:38])
    
    #sort alphabetically keyword test vector
    vtest_keyword<-sort(vtest_keyword)
    
    #define keyword database
    keyword_database<-as.matrix(movie_raw_data[,30:38])
    
    #Remove row/col names
    dimnames(keyword_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    s <- keyword_database
    for(i in 1:NROW(keyword_database)) {
      s[i,] <- sort(s[i,],na.last = TRUE)
    }
    #s
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(keyword_database)){
      
      keyword_database[j,] <- as.character(s[j,] %in% vtest_keyword)
      j=j+1 
    }
    
    #Convert T/F to binary
    keyword_database=ifelse(keyword_database=="TRUE",1,0)
    
    #Add results and create a vector
    keyword_results<-as.vector(rowSums(keyword_database))
    
    #####################################################################################################################
    
    ############################################# Genre Analysis#########################################################
    
    #define Genre test vector
    vtest_genre<-as.vector(vtest[,39:43])
    
    #sort alphabetically keyword test vector
    vtest_genre<-sort(vtest_genre)
    
    #define keyword database
    genre_database<-as.matrix(movie_raw_data[,39:43])
    
    #Remove row/col names
    dimnames(genre_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    s <- genre_database
    for(i in 1:NROW(genre_database)) {
      s[i,] <- sort(s[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(genre_database)){
      
      genre_database[j,] <- as.character(t[j,] %in% vtest_genre)
      j=j+1 
    }
    #Convert T/F to binary
    genre_database=ifelse(genre_database=="TRUE",1,0)
    
    #Add results and create a vector
    genre_results<-as.vector(rowSums(genre_database))
    
    #####################################################################################################################
    
    ############################################# Actor Analysis#########################################################
    
    #define Actor test vector
    vtest_actor<-as.vector(vtest[,25:29])
    
    #sort alphabetically keyword test vector
    vtest_actor<-sort(vtest_actor)
    
    #define keyword database
    actor_database<-as.matrix(movie_raw_data[,25:29])
    
    #Remove row/col names
    dimnames(actor_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- actor_database
    for(i in 1:NROW(actor_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(actor_database)){
      
      actor_database[j,] <- as.character(u[j,] %in% vtest_actor)
      j=j+1 
    }
    #Convert T/F to binary
    actor_database=ifelse(actor_database=="TRUE",1,0)
    
    #Add results and create a vector
    actor_results<-as.vector(rowSums(actor_database))
    
    #####################################################################################################################
    ############################################# Production Country Analysis#########################################################
    
    #define prod_country test vector
    vtest_prod_country<-as.vector(vtest[,21:24])
    
    #sort alphabetically keyword test vector
    vtest_prod_country<-sort(vtest_prod_country)
    
    #define keyword database
    prod_country_database<-as.matrix(movie_raw_data[,21:24])
    
    #Remove row/col names
    dimnames(prod_country_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- prod_country_database
    for(i in 1:NROW(prod_country_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(prod_country_database)){
      
      prod_country_database[j,] <- as.character(u[j,] %in% vtest_prod_country)
      j=j+1 
    }
    #Convert T/F to binary
    prod_country_database=ifelse(prod_country_database=="TRUE",1,0)
    
    #Add results and create a vector
    prod_country_results<-as.vector(rowSums(prod_country_database))
    
    #####################################################################################################################
    
    ############################################# Spoken Language Analysis#########################################################
    
    #define prod_country test vector
    vtest_spoken_l<-as.vector(vtest[,16:20])
    
    #sort alphabetically keyword test vector
    vtest_spoken_l<-sort(vtest_spoken_l)
    
    #define keyword database
    spoken_l_database<-as.matrix(movie_raw_data[,16:20])
    
    #Remove row/col names
    dimnames(spoken_l_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- spoken_l_database
    for(i in 1:NROW(spoken_l_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(spoken_l_database)){
      
      spoken_l_database[j,] <- as.character(u[j,] %in% vtest_spoken_l)
      j=j+1 
    }
    #Convert T/F to binary
    spoken_l_database=ifelse(spoken_l_database=="TRUE",1,0)
    
    #Add results and create a vector
    spoken_l_results<-rowSums(spoken_l_database)
    
    #####################################################################################################################
    
    #########################################################Other variables comparison############################################################
    #create a matrix with resuts from selected movie to the movie database
    comparisonmatrix <- matrix(NA, ncol=12, nrow=4795)#create empty matrix
    
    for(x in 1:nrow(movie_raw_data)){
      
      comparisonmatrix[x,] <- as.vector(colSums(movie_raw_data[x,1:12]==vtest))
      x=x+1 
    }#loop to compare each vector to the movie database and then paste it as a row in the resultmatrix
    
    ########################################################Numerical variables analysis######################################################
    #Perform numerical variable analysis
    
    popularity_vector<-as.vector(movie_raw_data$Adjusted_popularity)
    year_vector<-as.vector(movie_raw_data$Adjusted_year)
    vote_vector<-as.vector(movie_raw_data$Vote_true_bayesian_rating)
    
    resultmatrix<-cbind(comparisonmatrix,popularity_vector,year_vector,vote_vector,keyword_results,actor_results,genre_results,prod_country_results)
    
    #Add every row and sort the results
    
    
    rankvector<-as.vector(rowSums(resultmatrix,na.rm=TRUE))
    resultvector<-as.vector(tail(sort.int(rankvector, partial=length(rankvector)-4,decreasing=FALSE), 6))
    resultvector=resultvector[-which(resultvector==max(resultvector))]
    positionvector<- which(rankvector %in% resultvector)
    
    
    #####################################################################################################################
    ##################################################Print Results###################################################################
   
  
    recommendation=data.frame()
    movie_raw_data1=as.data.frame(movie_raw_data)
    
    
    for (i in 1:length(positionvector)){
    ww=positionvector[i] # taking the row number
      recommendation<- rbind(recommendation,movie_raw_data1[ww,1:4]) #appending all the results in new dataframe
    }
    
    print(recommendation)
    
    
  })
  output$SummaryData <- renderTable({
    n<-input$MovieID
    
    for(x in 1:nrow(movie_raw_data))
    {
      
      if(n %in% movie_raw_data[x,1])          
      {
        vtest <- as.data.frame(movie_raw_data[x,])
      }
      else
        
        x+1
    }
    
    class(vtest)
    vtest<- as.matrix(vtest)
    
    
    #####################################################################################################################
    ############################################# Keyword Analysis######################################################
    
    #define keyword test vector
    vtest_keyword<-as.vector(vtest[,30:38])
    
    #sort alphabetically keyword test vector
    vtest_keyword<-sort(vtest_keyword)
    
    #define keyword database
    keyword_database<-as.matrix(movie_raw_data[,30:38])
    
    #Remove row/col names
    dimnames(keyword_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    s <- keyword_database
    for(i in 1:NROW(keyword_database)) {
      s[i,] <- sort(s[i,],na.last = TRUE)
    }
    #s
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(keyword_database)){
      
      keyword_database[j,] <- as.character(s[j,] %in% vtest_keyword)
      j=j+1 
    }
    
    #Convert T/F to binary
    keyword_database=ifelse(keyword_database=="TRUE",1,0)
    
    #Add results and create a vector
    keyword_results<-as.vector(rowSums(keyword_database))
    
    #####################################################################################################################
    
    ############################################# Genre Analysis#########################################################
    
    #define Genre test vector
    vtest_genre<-as.vector(vtest[,39:43])
    
    #sort alphabetically keyword test vector
    vtest_genre<-sort(vtest_genre)
    
    #define keyword database
    genre_database<-as.matrix(movie_raw_data[,39:43])
    
    #Remove row/col names
    dimnames(genre_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    s <- genre_database
    for(i in 1:NROW(genre_database)) {
      s[i,] <- sort(s[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(genre_database)){
      
      genre_database[j,] <- as.character(t[j,] %in% vtest_genre)
      j=j+1 
    }
    #Convert T/F to binary
    genre_database=ifelse(genre_database=="TRUE",1,0)
    
    #Add results and create a vector
    genre_results<-as.vector(rowSums(genre_database))
    
    #####################################################################################################################
    
    ############################################# Actor Analysis#########################################################
    
    #define Actor test vector
    vtest_actor<-as.vector(vtest[,25:29])
    
    #sort alphabetically keyword test vector
    vtest_actor<-sort(vtest_actor)
    
    #define keyword database
    actor_database<-as.matrix(movie_raw_data[,25:29])
    
    #Remove row/col names
    dimnames(actor_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- actor_database
    for(i in 1:NROW(actor_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(actor_database)){
      
      actor_database[j,] <- as.character(u[j,] %in% vtest_actor)
      j=j+1 
    }
    #Convert T/F to binary
    actor_database=ifelse(actor_database=="TRUE",1,0)
    
    #Add results and create a vector
    actor_results<-as.vector(rowSums(actor_database))
    
    #####################################################################################################################
    ############################################# Production Country Analysis#########################################################
    
    #define prod_country test vector
    vtest_prod_country<-as.vector(vtest[,21:24])
    
    #sort alphabetically keyword test vector
    vtest_prod_country<-sort(vtest_prod_country)
    
    #define keyword database
    prod_country_database<-as.matrix(movie_raw_data[,21:24])
    
    #Remove row/col names
    dimnames(prod_country_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- prod_country_database
    for(i in 1:NROW(prod_country_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(prod_country_database)){
      
      prod_country_database[j,] <- as.character(u[j,] %in% vtest_prod_country)
      j=j+1 
    }
    #Convert T/F to binary
    prod_country_database=ifelse(prod_country_database=="TRUE",1,0)
    
    #Add results and create a vector
    prod_country_results<-as.vector(rowSums(prod_country_database))
    
    #####################################################################################################################
    
    ############################################# Spoken Language Analysis#########################################################
    
    #define prod_country test vector
    vtest_spoken_l<-as.vector(vtest[,16:20])
    
    #sort alphabetically keyword test vector
    vtest_spoken_l<-sort(vtest_spoken_l)
    
    #define keyword database
    spoken_l_database<-as.matrix(movie_raw_data[,16:20])
    
    #Remove row/col names
    dimnames(spoken_l_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- spoken_l_database
    for(i in 1:NROW(spoken_l_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(spoken_l_database)){
      
      spoken_l_database[j,] <- as.character(u[j,] %in% vtest_spoken_l)
      j=j+1 
    }
    #Convert T/F to binary
    spoken_l_database=ifelse(spoken_l_database=="TRUE",1,0)
    
    #Add results and create a vector
    spoken_l_results<-rowSums(spoken_l_database)
    
    #####################################################################################################################
    
    #########################################################Other variables comparison############################################################
    #create a matrix with resuts from selected movie to the movie database
    comparisonmatrix <- matrix(NA, ncol=12, nrow=4795)#create empty matrix
    
    for(x in 1:nrow(movie_raw_data)){
      
      comparisonmatrix[x,] <- as.vector(colSums(movie_raw_data[x,1:12]==vtest))
      x=x+1 
    }#loop to compare each vector to the movie database and then paste it as a row in the resultmatrix
    
    ########################################################Numerical variables analysis######################################################
    #Perform numerical variable analysis
    
    popularity_vector<-as.vector(movie_raw_data$Adjusted_popularity)
    year_vector<-as.vector(movie_raw_data$Adjusted_year)
    vote_vector<-as.vector(movie_raw_data$Vote_true_bayesian_rating)
    
    resultmatrix<-cbind(comparisonmatrix,popularity_vector,year_vector,vote_vector,keyword_results,actor_results,genre_results,prod_country_results)
    
    #Add every row and sort the results
    
    
    rankvector<-as.vector(rowSums(resultmatrix,na.rm=TRUE))
    resultvector<-as.vector(tail(sort.int(rankvector, partial=length(rankvector)-4,decreasing=FALSE), 6))
    resultvector=resultvector[-which(resultvector==max(resultvector))]
    positionvector<- which(rankvector %in% resultvector)
    
    
    #####################################################################################################################
    ##################################################Print Results###################################################################
    
    
    recommendation=data.frame()
    movie_raw_data1=as.data.frame(movie_raw_data)
    
    
    for (i in 1:length(positionvector)){
      ww=positionvector[i] # taking the row number
      recommendation<- rbind(recommendation,movie_raw_data1[ww,c(2,5)]) #appending all the results in new dataframe
    }
    
    print(recommendation)
    
    
  })
  
  output$InformationData <- renderTable({
    n<-input$MovieID
    
    for(x in 1:nrow(movie_raw_data))
    {
      
      if(n %in% movie_raw_data[x,1])          
      {
        vtest <- as.data.frame(movie_raw_data[x,])
      }
      else
        
        x+1
    }
    
    class(vtest)
    vtest<- as.matrix(vtest)
    
    
    #####################################################################################################################
    ############################################# Keyword Analysis######################################################
    
    #define keyword test vector
    vtest_keyword<-as.vector(vtest[,30:38])
    
    #sort alphabetically keyword test vector
    vtest_keyword<-sort(vtest_keyword)
    
    #define keyword database
    keyword_database<-as.matrix(movie_raw_data[,30:38])
    
    #Remove row/col names
    dimnames(keyword_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    s <- keyword_database
    for(i in 1:NROW(keyword_database)) {
      s[i,] <- sort(s[i,],na.last = TRUE)
    }
    #s
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(keyword_database)){
      
      keyword_database[j,] <- as.character(s[j,] %in% vtest_keyword)
      j=j+1 
    }
    
    #Convert T/F to binary
    keyword_database=ifelse(keyword_database=="TRUE",1,0)
    
    #Add results and create a vector
    keyword_results<-as.vector(rowSums(keyword_database))
    
    #####################################################################################################################
    
    ############################################# Genre Analysis#########################################################
    
    #define Genre test vector
    vtest_genre<-as.vector(vtest[,39:43])
    
    #sort alphabetically keyword test vector
    vtest_genre<-sort(vtest_genre)
    
    #define keyword database
    genre_database<-as.matrix(movie_raw_data[,39:43])
    
    #Remove row/col names
    dimnames(genre_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    s <- genre_database
    for(i in 1:NROW(genre_database)) {
      s[i,] <- sort(s[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(genre_database)){
      
      genre_database[j,] <- as.character(t[j,] %in% vtest_genre)
      j=j+1 
    }
    #Convert T/F to binary
    genre_database=ifelse(genre_database=="TRUE",1,0)
    
    #Add results and create a vector
    genre_results<-as.vector(rowSums(genre_database))
    
    #####################################################################################################################
    
    ############################################# Actor Analysis#########################################################
    
    #define Actor test vector
    vtest_actor<-as.vector(vtest[,25:29])
    
    #sort alphabetically keyword test vector
    vtest_actor<-sort(vtest_actor)
    
    #define keyword database
    actor_database<-as.matrix(movie_raw_data[,25:29])
    
    #Remove row/col names
    dimnames(actor_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- actor_database
    for(i in 1:NROW(actor_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(actor_database)){
      
      actor_database[j,] <- as.character(u[j,] %in% vtest_actor)
      j=j+1 
    }
    #Convert T/F to binary
    actor_database=ifelse(actor_database=="TRUE",1,0)
    
    #Add results and create a vector
    actor_results<-as.vector(rowSums(actor_database))
    
    #####################################################################################################################
    ############################################# Production Country Analysis#########################################################
    
    #define prod_country test vector
    vtest_prod_country<-as.vector(vtest[,21:24])
    
    #sort alphabetically keyword test vector
    vtest_prod_country<-sort(vtest_prod_country)
    
    #define keyword database
    prod_country_database<-as.matrix(movie_raw_data[,21:24])
    
    #Remove row/col names
    dimnames(prod_country_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- prod_country_database
    for(i in 1:NROW(prod_country_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(prod_country_database)){
      
      prod_country_database[j,] <- as.character(u[j,] %in% vtest_prod_country)
      j=j+1 
    }
    #Convert T/F to binary
    prod_country_database=ifelse(prod_country_database=="TRUE",1,0)
    
    #Add results and create a vector
    prod_country_results<-as.vector(rowSums(prod_country_database))
    
    #####################################################################################################################
    
    ############################################# Spoken Language Analysis#########################################################
    
    #define prod_country test vector
    vtest_spoken_l<-as.vector(vtest[,16:20])
    
    #sort alphabetically keyword test vector
    vtest_spoken_l<-sort(vtest_spoken_l)
    
    #define keyword database
    spoken_l_database<-as.matrix(movie_raw_data[,16:20])
    
    #Remove row/col names
    dimnames(spoken_l_database)=NULL
    
    #Loop to alphabetically arrange Keyword Database by row
    u <- spoken_l_database
    for(i in 1:NROW(spoken_l_database)) {
      u[i,] <- sort(u[i,],na.last = TRUE)
    }
    
    
    #Loop to compare test vector to keyword database
    for(j in 1:nrow(spoken_l_database)){
      
      spoken_l_database[j,] <- as.character(u[j,] %in% vtest_spoken_l)
      j=j+1 
    }
    #Convert T/F to binary
    spoken_l_database=ifelse(spoken_l_database=="TRUE",1,0)
    
    #Add results and create a vector
    spoken_l_results<-rowSums(spoken_l_database)
    
    #####################################################################################################################
    
    #########################################################Other variables comparison############################################################
    #create a matrix with resuts from selected movie to the movie database
    comparisonmatrix <- matrix(NA, ncol=12, nrow=4795)#create empty matrix
    
    for(x in 1:nrow(movie_raw_data)){
      
      comparisonmatrix[x,] <- as.vector(colSums(movie_raw_data[x,1:12]==vtest))
      x=x+1 
    }#loop to compare each vector to the movie database and then paste it as a row in the resultmatrix
    
    ########################################################Numerical variables analysis######################################################
    #Perform numerical variable analysis
    
    popularity_vector<-as.vector(movie_raw_data$Adjusted_popularity)
    year_vector<-as.vector(movie_raw_data$Adjusted_year)
    vote_vector<-as.vector(movie_raw_data$Vote_true_bayesian_rating)
    
    resultmatrix<-cbind(comparisonmatrix,popularity_vector,year_vector,vote_vector,keyword_results,actor_results,genre_results,prod_country_results)
    
    #Add every row and sort the results
    
    
    rankvector<-as.vector(rowSums(resultmatrix,na.rm=TRUE))
    resultvector<-as.vector(tail(sort.int(rankvector, partial=length(rankvector)-4,decreasing=FALSE), 6))
    resultvector=resultvector[-which(resultvector==max(resultvector))]
    positionvector<- which(rankvector %in% resultvector)
    
    
    #####################################################################################################################
    ##################################################Print Results###################################################################
    
    
    recommendation=data.frame()
    movie_raw_data1=as.data.frame(movie_raw_data)
    
    
    for (i in 1:length(positionvector)){
      ww=positionvector[i] # taking the row number
      recommendation<- rbind(recommendation,movie_raw_data1[ww,c(2,6,8,9,10,11)]) #appending all the results in new dataframe
    }
    
    print(recommendation)
    
    
  })
  
}

# See above for the definitions of ui and server


shinyApp(ui = ui, server = server)
