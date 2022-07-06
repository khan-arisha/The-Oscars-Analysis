
##general set up/data import
library(readxl)
library(readr)
library(tidyverse)
library(gganimate)
library(shiny)
library(rpart)
library(randomForest)
library(shinythemes)
library(bslib)
oscars<- read_excel("oscars.xlsx")%>%
  rename(race=Race,category=Category)%>%
  mutate(category=str_to_sentence(category))

actors<-read_csv("Top 100 Greatest Hollywood Actors of All Time.csv")%>%
  rename(oscar_nom="Oscar Nominations",name=Name,oscars=Oscars,BAFTA_nom="BAFTA Nominations",goldenglobes="Golden Globes",goldenglobe_nom="Golden Globe Nominations",greatest_performances = "Greatest Performances")

IMDB <- read_csv("IMDB_scraping.csv")

oscars<- oscars %>%
  mutate(gender= fct_collapse(gender,"Female"= c("female", "Female")))

IMDB_genres<-read_csv("imdb_top_1000.csv")%>%
  rename(film=Series_Title)

#set up for data tab 

#set up for oscars and rankings
moviesWin <- oscars %>%
  filter(category== "Best picture") 

moviesWin$film = str_to_lower(moviesWin$film)

IMDB_clean <- IMDB %>%
  mutate(Title=str_trim(str_match(str_to_lower(Title), "^[^\\(]+")))


Omovie_rank <-left_join(moviesWin,IMDB_clean,  by = c("film" = "Title")) 
Omovie_rank <- Omovie_rank %>% 
  drop_na() 

Omovie_rank<-distinct(Omovie_rank)

#set up for actors and awards

actors_awards<-actors%>%
  mutate("Oscar Wins"=as.factor(ifelse(oscars>=1,"yes","no")),
         "BAFTA Wins"=as.factor(ifelse(BAFTA>=1,"yes","no")),
         "Golden Globe Wins"=as.factor(ifelse(goldenglobes>=1,"yes","no")),
         "Oscar Nominations" =as.factor(ifelse(oscar_nom>=1,"yes","no")),
         "BAFTA nominations" =as.factor(ifelse(BAFTA_nom>=1,"yes","no")),
         "Golden Globe Nominations" =as.factor(ifelse(goldenglobe_nom>=1,"yes","no")))%>%
  rename("Total Oscar Wins"=oscars,
         "Total BAFTA Wins"=BAFTA,
         "Total Golden Globe Wins"=goldenglobes,
         "Total Oscar Nominations"=oscar_nom,
         "Total Golden Globe Nominations" = goldenglobe_nom,
         "Total BAFTA Nominations" = BAFTA_nom)

actors_long<-actors_awards%>%
  pivot_longer(cols = "Oscar Wins":"Golden Globe Nominations",
               names_to = "award",
               values_to = "won")
variables<-c("Total Oscar Wins","Total Oscar Nominations","Total BAFTA Wins","Total BAFTA Nominations","Total Golden Globe Wins","Total Golden Globe Nominations")

#set up for diversity in awards
timeRace<- oscars %>%
  group_by(year_ceremony,race) %>%
  summarise(n=n())

timeGender<- oscars %>%
  group_by(year_ceremony,gender) %>%
  summarise(n=n())

actors_demo<-left_join(actors_long,oscars,by="name")%>%
  select(c(name:greatest_performances,gender,race))%>%
  distinct()
actors_demo$race[3]<-"White" #alan rickman
actors_demo$gender[3]<-"Male"
actors_demo$race[52]<-"White" #jim carrey
actors_demo$gender[52]<- "Male"
actors_demo$race[55]<-"White" #john goodman
actors_demo$gender[55]<- "Male"
actors_demo$race[66]<-"White" #Marin Sheen
actors_demo$gender[66]<- "Male"

actors_demo <- actors_demo%>%
  mutate(won_oscar=as.factor(ifelse(`Total Oscar Wins`>=1,"yes","no")),
         won_BAFTA=as.factor(ifelse(`Total BAFTA Wins`>=1,"yes","no")),
         won_goldenglobe=as.factor(ifelse(`Total Golden Globe Wins`>=1,"yes","no")))

actors_demo<-actors_demo%>%
  pivot_longer(cols = won_oscar:won_goldenglobe,
               names_to = "award",
               values_to = "won",
               names_prefix = "won_")

#set up for prediction

#creating random forest
oscars<- oscars %>%
  mutate(gender= fct_collapse(gender,"Female"= c("female", "Female")))

oscar_tree<-inner_join(IMDB_genres,oscars,by="film")%>%
  mutate(genre=gsub(",.*", "", Genre))%>%
  mutate(winner=as.character(winner)) %>% 
  mutate(class = fct_recode(
    winner, 
    win = "TRUE" , 
    nowin = "FALSE"),
    class = fct_relevel(class, "win") # make "win" the first level (our "positive")
  )%>%
  select(c(year_film,genre,category,gender,race,class))%>%
  na.exclude()

oscars_forest<-randomForest(
  class ~ .,
  data = oscar_tree,
  mtry = 3
)

#making empty data frame: 
g_factor<-factor(c("Female","Male"))
gen<-rep(g_factor[2],1)
cat<-"NA"
race<-"NA"
genre<-"NA"
year<-1000
oscars_empty<-data.frame(year_film=year,genre=genre,category=cat,gender=gen,race=race)

races<-unique(oscar_tree$race)
genders<-unique(oscar_tree$gender)
categories<-unique(oscar_tree$category)
genres<-unique(oscar_tree$genre)




##Start of Shiny App

ui<-fluidPage(
  
  
  titlePanel(title="The Oscars and the Entertainment Industry"),
  theme = bs_theme(
    bootswatch = "lumen"),
  tabsetPanel(
    tabPanel("Introduction",
             img(src = "my_image.png", height = 400, width = 800, align = "center"),
             br(),
             p(),
             p(),
             p("The Academy awards is probably the most anticipated award ceremony in the United States. Members of the Academy of Motion Picture Arts and Science choose the nominees and out of those nominees, best films and talent of the year take home the Oscar. However, it is hard not to notice the lack of diversity in Oscars with so many white and male identifying Oscar winners. In fact, many might argue that Oscar winners and nominations seldom reflect the broad range of cinema the world has to offer.",style="font-size: 15px ; color: darkalategray3"), 
             br(),
             p("We analyzed four datasets (Oscar, IMDB rantings, IMDB top 1000 movies and top 100 greatest actors of all time) from Kaggle. First, We checked the IMDB ratings of movies nominated for Oscars and analyzed if movies and their IMDB rating. Are higher rated movies more likely to win Oscars? We also analyzed which MPAA rankings were most commonly found in oscar-winning movies. For the top 100 greatest actors dataset, we checked to see if there was any correlation between their BAFTA, golden globes and Oscars award. If an actor doesnt have an Oscar, how many BAFTAs/ Golden Globes do they have?  We also analyzed for any relation between awards and actors race. If they have won other awards, is the system biased against them? While we are focusing on OScars, we also evaluated if there are more biased award ceremonies. Are some awards more biased than others? For our Oscars dataset, we analyzed the gender and race prevalence in entertainment industry through out the years by analyzing Oscar nominations and wins for various genres and Oscars award types. Has diversity in Oscars increased over time? Lastly, we used the top 1000 IMDB movies dataset to  check the genres od mvies nomianted for BEst PIcture Oscar category and allowed the user to input common Oscar nomination characteristics (film genre, race, gender, Oscar category and release year of film) which predicts the chances of the user winning Oscars through the Random Forest classifier method.",style="font-size: 15px ; color: darkslategray3")),
    tabPanel("The Data",
             strong("All three datasets were taken from Kaggle.",style="font-size: 20px"),
             br(),
             strong("We also used the package tidyverse to tidy the data using dplyr, tidyr and stringr before starting our analysis.",style="font-size: 20px"),
             navbarPage(title="Data Set",
                        tabPanel("Top 100 Greatest Actors",
                                 dataTableOutput("actors_data")),
                        tabPanel("Oscar Awards",
                                 dataTableOutput("oscars_data")),
                        tabPanel("Movie Data from IMDB",
                                 dataTableOutput("IMDB_data"))
             )
    ),
    tabPanel("Oscars and Rankings",
             #put stuff for oscars and rankings here
             navlistPanel(
               widths = c(3, 8),
               
               tabPanel("Do higher ranked movies win more Oscars?",
                        strong("This boxplot shows the ranking of the Oscar winning movies and also movies that were nominated but did not win an oscar."),
                        br(),
                        p("As seen in the boxplot, Oscar winning movies are ranked a bit higher on average."),
                        plotOutput("rank_boxplot", height=500)
               ),
               tabPanel("Most common MPAA rating for Best Picture noms and winners",
                        strong("This stacked bar chart shows the rating of Oscar winning movies and also movies that were nominated but did not win an oscar."),
                        br(),
                        p("As seen in this graph, most oscars and oscar nominations are given to R rated movies."),
                        plotOutput("rate_bar", height=500)
               )
             )
             
             
    ),
    tabPanel("Actors and Awards",
             navlistPanel(
               widths = c(3, 8),
               tabPanel("How many of the greatest actors have won each award?",
                        strong("This stacked bar chart shows the different number of nominations and award wins for the 100 greatest actors."),
                        br(),
                        p("Most of them have oscar nominations but most of them have won BAFTA awards."),
                        plotOutput("award_bar",height=500)),
               tabPanel("What are the correlations between awards?",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId="x.var", 
                                        label="Awards/Nominations on X Axis",
                                        choices=variables),
                            selectInput(inputId="y.var", 
                                        label="Awards/Nominations on Y Axis",
                                        choices=variables
                            )),
                          mainPanel(
                            strong("This scatterplot checks if there is any correlation between each award type nominations/wins for top 100 greatest actors."),
                            br(),
                            strong("Does winning one award increase the likelihood of winning another?"),
                            plotOutput("scatter", height = 500),
                            textOutput("cor"))
                        )),
               tabPanel("Race and Different Award wins",
                        sidebarLayout(
                          sidebarPanel(
                            varSelectInput(inputId = "varA",
                                           label = "Choose an award",
                                           data= select(actors_demo, 
                                                        c("Total Oscar Wins", 
                                                          "Total BAFTA Wins",
                                                          "Total Golden Globe Wins")))),
                          mainPanel(strong("This stacked bar chart shows the different award winners for each race."),
                                    br(),
                                    strong(),
                                    plotOutput(outputId = "awards_var", height = 500),
                                    p("It is important to note that there is only one Asian actor, Ben Kingsley, and he has won all three awards."),
                                    p("It looks like the Oscars are the most diverse among all three awrds."),
                                    p("However, this could be due to the fact that most of the 100 greatest actors have Oscar nominations."))
                          
                        )
               )
             )#end second tab (within)
    ), #end actors and awards tab
    tabPanel("Diversity in Oscars",
             navlistPanel(
               widths = c(3, 8),
               tabPanel("Race & Oscars",
                        
                        fluidRow(
                          
                          column(6,
                                 strong("This bar graph analyses the proportion of total Oscar wins and noms for each race(this includes every Oscar category)."),
                                 plotOutput("race_bar"),
                                 p("There are significantly more white people present comapared to black people across each Oscar category.")),
                          
                          
                          column(6, strong("This bar graph gives the proportion of Oscar wins and noms for each race out of the total wins/noms for that race."),
                                 plotOutput("race_bar2"),
                                 p("Out of all the Hispanics nominated, most of them have won the Oscar. However, this is only because there are very few Hispanics nominated."))
                        )),
               tabPanel("Race and The Entertainment Industry over the years",
                        
                        sidebarLayout(
                          sidebarPanel(sliderInput(inputId = "yearR",
                                                   label= "Choose year range ",
                                                   min = 1928,
                                                   max = 2020,
                                                   step = 1,
                                                   value = 1928,
                                                   sep = "",
                                                   animate = TRUE)),
                          mainPanel(strong("This time series plot analyses the prevelance of each race in the entertainment industry thought the years 1928-2020. This is gauged through Oscar nominations and wins across each cateogry available."),
                                    plotOutput("race_line"),
                                    p("Usurprisingly, non-white people have always had low represenatnion in Oscars (and by a significant margin) over the years. Recently, there seems be an increase in racial diversity but by a very small amount."))
                        )),
               tabPanel("Gender & Oscars",
                        fluidRow(
                        column(6,
                               strong("This bar graph analyses the proportion of total Oscar wins and noms for each gender(this includes every Oscar category)."),
                               plotOutput("gender_bar"),
                               p("There are 4x more males than females in Oscars, Overall.")),
                        
                        column(6, strong("This bar graph gives the proportion of Oscar wins and noms for each race out of the total wins/noms for that gender."),
                               plotOutput("gender_bar2"),
                               p("Out of their respective proportions, both genders have the same amount of oscar wins but since there are already very few females in Oscars, this graph doesnt represent that well."))
               )),
               tabPanel("Gender and The Entertainment Industry over the years",
                        sidebarLayout(
                          sidebarPanel(sliderInput(inputId = "yearG",
                                                   label= "Choose year range ",
                                                   min = 1928,
                                                   max = 2020,
                                                   step = 1,
                                                   value = 1928,
                                                   sep = "",
                                                   animate = TRUE)),
                          mainPanel(strong("This time series plot analyses the prevelance of each gender in the entertainment industry thought the years 1928-2020. This is gauged through Oscar nominations and wins across each cateogry available."),
                                    plotOutput("gender_line"),
                                    p("Historally, women have been underespented in Oscars but this trend seems to be changing. Female representantion has increased greatly in recent years."))
                        ))
             )),
    
    tabPanel("Would your movie win an oscar?",
             fluidRow(column(width=12)),
             fluidRow(column(width = 3,
                             selectInput(inputId = "genre",
                                       label = "What genre is your film?",
                                       choices= genres
                             )#input film - as unique/whatever they want
             ),
             column(width = 3, offset = 1,
                    selectInput(inputId="category", 
                                label="What category would you like to be nominated for?",
                                choices=categories)#select category
             ),
             column(width=3,offset = 1,
                    textInput(inputId="year",
                              label = "What year was your film made?",
                              placeholder = "YYYY"))
             ), #end first row 
             fluidRow(
               column(width = 3,
                      selectInput(inputId="gender", 
                                  label="What gender are you?",
                                  choices=genders)#input gender - from selections
               ),
               column(width = 3, offset = 2,
                      selectInput(inputId="race", 
                                  label="What race are you?",
                                  choices=races)#input race - from selections
               )
             ), #end second row
             fluidRow(column(width = 12,
                             textOutput("results")
             )),
             fluidRow(column(width = 4,
                             textOutput("prob_intro")),
                      column(width=2,
                             textOutput("results_prob"))),
             
             strong("We used Random Forest classifer method by randomly selecting 3 predictors at each split (mtry value 3) and by using 500 bootstraped trees (ntree = 500) to make our prediction. This model has an accuracy of around 63%, with a specificity of around 70% and sensitivity of around 35%. Theres only one winner out of many nominations and hence this predidction may not be entirely accurate. Oscar nominations/ winners are also chosen through a jury which may result in unpredictable or biased human behaviour and outlier values that are not caught by this prediction.")
    ) #end prediction tab
  )
)




server <- function(input,output){
  
  ##server for Data tab
  output$oscars_data<-renderDataTable(oscars)
  output$actors_data<-renderDataTable(actors)
  output$IMDB_data<-renderDataTable(IMDB)
  
  ##server for oscars and rankings 
  
  ##server for actors and awards
  output$award_bar<-renderPlot({
    ggplot(actors_long)+
      geom_bar(aes(x=award,fill=won),position="dodge") +
      scale_fill_brewer(palette = "Accent") +
      labs(title="Award Wins/Nominations for Top 100 Greatest Actors",x="Award/Nomination",y = "Number of Actors Who Have Ever Won")
  })
  
  
  output$scatter<-renderPlot({
    ggplot(actors_awards,aes(x=get(input$x.var),y=get(input$y.var)))+
      geom_point(position="jitter")+
      geom_smooth(method="lm",se=0)+
      labs(x=input$x.var,y=input$y.var,title="Correlations Between Different Awards or Nomincations")
  })
  
  
  output$cor<-renderText({
    cor<-if (input$x.var==input$y.var) {
      print("You have selected the same variable on each axis! The correlation is 1")
    } else if (input$x.var=="Total Oscar Wins" & input$y.var=="Total Oscar Nominations" | input$y.var=="Total Oscar Wins" & input$x.var=="Total Oscar Nominations") {
      print("The correlation between oscar wins and oscar nominations is: 0.6212523")
    } else if (input$x.var=="Total Oscar Wins" & input$y.var=="Total BAFTA Wins" | input$y.var=="Total Oscar Wins" & input$x.var=="Total BAFTA Wins"){
      print("The correlation between oscar wins and BAFTA wins is: 0.4784657")
    } else if (input$x.var=="Total Oscar Wins" & input$y.var=="Total BAFTA Nominations" | input$y.var=="Total Oscar Wins" & input$x.var=="Total BAFTA Nominations"){
      print("The correlation between oscar wins and BAFTA nominations is: 0.466382")
    } else if(input$x.var=="Total Oscar Wins" & input$y.var=="Total Golden Globe Wins" | input$y.var=="Total Oscar Wins" & input$x.var=="Total Golden Globe Wins") {
      print("The correlation between oscar wins and golden globe wins is: 0.3636958")
    } else if(input$x.var=="Total Oscar Wins" & input$y.var=="Total Golden Globe Nominations" | input$y.var=="Total Oscar Wins" & input$x.var=="Total Golden Globe Nominations") {
      print("The correlation between oscar wins and golden globe nominations is: 0.3636958")
    }else if(input$x.var=="Total Oscar Nominations" & input$y.var=="Total BAFTA Wins" | input$y.var=="Total Oscar Nominations" & input$x.var=="Total BAFTA Wins"){
      print("The correlation between oscar nominations and BAFTA wins is: 0.6064473")
    }else if(input$x.var=="Total Oscar Nominations" & input$y.var=="Total BAFTA Nominations" | input$y.var=="Total Oscar Nominations" & input$x.var=="Total BAFTA Nominations"){
      print("The correlation between oscar nominations and BAFTA nominations is: 0.7259319")
    }else if(input$x.var=="Total Oscar Nominations" & input$y.var=="Total Golden Globe Wins" | input$y.var=="Total Oscar Nominations" & input$x.var=="Total Golden Globe Wins"){
      print("The correlation between oscar nominations and golden globe wins is: 0.5107176")
    }else if(input$x.var=="Total Oscar Nominations" & input$y.var=="Total Golden Globe Nominations" | input$y.var=="Total Oscar Nominations" & input$x.var=="Total Golden Globe Nominations"){
      print("The correlation between oscar nominations and golden globe nominations is: 0.6081102")
    }else if(input$x.var=="Total BAFTA Wins" & input$y.var=="Total BAFTA Nominations"|input$y.var=="Total BAFTA Wins" & input$x.var=="Total BAFTA Nominations"){
      print("The correlation between BAFTA wins and BAFTA nominations is: 0.819788")
    }else if(input$x.var=="Total BAFTA Wins" & input$y.var=="Total Golden Globe Wins"|input$y.var=="Total BAFTA Wins" & input$x.var=="Total Golden Globe Wins"){
      print("The correlation between BAFTA wins and golden globe wins is: 0.3636176")
    }else if(input$x.var=="Total BAFTA Wins" & input$y.var=="Total Golden Globe Nominations"|input$y.var=="Total BAFTA Wins" & input$x.var=="Total Golden Globe Nominations"){
      print("The correlation between BAFTA wins and golden globe nominations is: 0.3742357")
    }else if(input$x.var=="Total BAFTA Nominations" & input$y.var=="Total Golden Globe Wins"|input$y.var=="Total BAFTA Nominations" & input$x.var=="Total Golden Globe Wins"){
      print("The correlation between BAFTA nominations and golden globe wins is: 0.3840752")
    }else if(input$x.var=="Total BAFTA Nominations" & input$y.var=="Total Golden Globe Nominations" |input$y.var=="Total BAFTA Nominations" & input$x.var=="Total Golden Globe Nominations"){
      print("The correlation between BAFTA nominations and golden globe nominations is: 0.5177539")
    }else if(input$x.var=="Total Golden Globe Wins" & input$y.var=="Total Golden Globe Nominations"| input$y.var=="Total Golden Globe Wins" & input$x.var=="Total Golden Globe Nominations"){
      print("The correlation between golden globe wins and golden globe nominations is: 0.7138471")
    }else {
      print("The correlation between your selected variables is unknown")
    }
  })
  
  ##server for diversity in awards
  
  ##server for prediction
  output$results<-renderText({
    oscar.new<-rbind(oscars_empty,c(input$year,input$genre,input$category,input$gender,input$race))%>%
      slice(2)
    
    predict.win<-predict(oscars_forest,newdata=oscar.new,type="response")
    
    if(predict.win == "nowin"){
      print("Sorry, it does not look like you would win the award you were trying for. Better luck next time!")
    }else if(predict.win== "win"){
      print("Congratulations! You won an oscar! You better get working on your acceptance speech...")
    }else {
      print("Oops, there might be a mistake")
    }
  })
  output$prob_intro<-renderText(print("Your probability of winning is:"))
  output$results_prob<-renderText({
    oscar.new<-rbind(oscars_empty,c(input$year,input$genre,input$category,input$gender,input$race))%>%
      slice(2)
    prob.win<-predict(oscars_forest,newdata = oscar.new,type="prob")
    print(prob.win[[1]])
  })
  
  
  output$rank_boxplot <- renderPlot({
    ggplot(Omovie_rank) +
      geom_boxplot(aes( winner, Rating))+
      labs(title = "IMDB Rating and Oscar Category (Best Picture)",
           x = "Oscar Won?",
           y = "IMDB Rating")
  })
  
  output$rate_bar <- renderPlot({
    ggplot(Omovie_rank) +
      geom_bar(aes( `MPAA Rating`, fill = winner), position = "dodge")+
      scale_fill_brewer(palette = "Accent") +
      labs(title = "MPAA Rating and Oscar Category (Best Picture)",
           x = "MPAA Rating",
           y= "Count",
           fill = "Oscar Won?")
  })
  
  output$race_bar <- renderPlot({
    ggplot(oscars) +
      geom_bar(aes(race, y = ..prop.., group=1))+
      labs(title = "Racial diversity in Oscars",
           subtitle = "Do more white people have Oscar noms/wins?",
           x = "Race",
           y = "Proportion") +
      scale_fill_brewer(palette = "Accent")
  })
  
  output$race_bar2 <- renderPlot({
    ggplot(oscars) +
      geom_bar(aes(race, fill = winner), position = "fill")+
      labs(title = "Racial diversity in Oscars",
           subtitle = "Are white people more likely to win?",
           x = "Race",
           y = "Proportion") +
      scale_fill_brewer(palette = "Accent")
  })  
  
  output$race_line <- renderPlot({
    
    ggplot(filter(timeRace, year_ceremony <= input$yearR), aes(year_ceremony, n, color = race)) +
      geom_line() +
      geom_point()+
      xlim(1928,2020) +
      ylim(1,184) +
      labs(title = "Race analysis in the entertainment industry",
           subtitle = "Does the proportion decrease/increase over time?",
           x = "Year",
           y = "Count")
  })  
  
  output$gender_bar <- renderPlot({
    ggplot(oscars) +
      geom_bar(aes(gender, y = ..prop.., group=1)) +
      scale_fill_brewer(palette = "Accent") +
      labs(title = "Gender diversity in Oscars",
           subtitle = "Do more male identifying people have Oscar noms/wins?",
           x = "Gender",
           y = "Proportion")
    
  })
  
  output$gender_bar2 <- renderPlot({
    ggplot(oscars) +
      geom_bar(aes(gender, fill = winner), position = "fill") +
      scale_fill_brewer(palette = "Accent") +
      labs(title = "Gender diversity in Oscars",
           subtitle = "Are male identifying people more likely to win?",
           x = "Gender",
           y = "Proportion")
  })  
  
  output$gender_line <- renderPlot({
    
    ggplot(filter(timeGender, year_ceremony <= input$yearG), aes(year_ceremony, n, color= gender)) +
      geom_line() +
      geom_point()+
      xlim(1928,2020) +
      ylim(3,173) +
      labs(title = "Gender analysis in the entertainment industry",
           subtitle = "Does the proportion decrease/increase over time?",
           x = "Year",
           y = "Count")
  }) 
  
  output$awards_var <- renderPlot({
    ggplot(actors_demo) +
      geom_col(aes(race, !!input$varA, fill = won),position = "fill") +
      labs(title = "Different awards by race",
           subtitle = "Are some awards biased?",
           x = "Race",
           y = as.character(input$varA)) +
      scale_fill_brewer(palette = "Accent")
    
  })
  
}


shinyApp(ui=ui,server=server)

