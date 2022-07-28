# The Oscars #

https://arisha.shinyapps.io/TheOscars/

#### Overview  ####

Analyzed racial and gender diversity in the history of entertainment industry’s competitive awards (with the main focus on
the Oscars and some analysis on the BAFTAs and the Golden Globes) in R by using ShinyR and tidyverse for general tidying
and Random Forest Classifier method for predicting if a certain user can win the Oscar. The project used four datasets
(Oscar, IMDB ratings, IMDB top 1000 movies, and top 100 greatest actors of all time) from Kaggle.

Key results indicated that across all Oscar nominations and wins for various genres and Oscar award types, non-white and
non-male individuals have been marginally underrepresented, however, there seems to be a slight increase in racial inclusivity
and a greater increase in gender inclusivity in recent years.

#### Datasets  ####

*IMDB_scraping.csv*

*imdb_top_1000.csv*

*oscars.xlsx*

*Top 100 Greatest Hollywood Actors of All Time.csv*



#### Description of files  ####

*Shiny_app.R:* Contains code for our Shiny app. Worked on this together.

*www:* Contains image for our introduction tab.

#### Brief technical report ####

We analyzed four datasets (Oscar, IMDB ratings, IMDB top 1000 movies, and top 100 greatest actors of all time) from Kaggle. We wanted to see if the Oscars represented the film industry and the broad range of cinema fairly. We found that movies with higher IMDB ratings and an R-rated MPAA rating were more likely to win an Oscar. While we are focusing on the Oscars, we also evaluated if there are other biased award ceremonies. For the top 100 greatest actors dataset, we found that most of them had Oscar nominations but most of them have won BAFTA awards. We checked to see if there was any correlation between their BAFTA, Golden Globe, and Oscar awards.  We did not find any strong correlation between the different award types. We also analyzed for any relation between awards and actors' race. We found that the Oscars seemed more diverse among all three but as most of them had Oscar nominations, the graph might be misleading.  For our Oscars dataset, we analyzed the gender and race prevalence in the entertainment industry throughout the years 1928-2020 by analyzing Oscar nominations and wins for various genres and Oscar award types. Non-white and non-male individuals have been marginally underrepresented, however, there seems to be a slight increase in racial and gender inclusivity (more so for gender and less for race) in recent years. Lastly, we used the top 1000 IMDB movies data set to check the genres of movies nominated and allowed the user to input common Oscar nomination characteristics (film genre, race, gender, Oscar category, and release year of film) which predicts the chances of the user winning Oscars through the Random Forest classifier method. Possibly due to limitations in sample size, our model had an accuracy of around 63%. Nonetheless, this model provides a framework for predicting the likelihood of an Oscar nominee to win their award that could easily be improved with deeper data sets in the future. 


