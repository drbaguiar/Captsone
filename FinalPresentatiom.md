Coursera Data Science Capstone Final Presentation
========================================================
author: Bryan Aguiar
date: April 5, 2015

Please minimize the toolbar below to navigate the slide deck

Project Overview
========================================================

The goal of this project was to produce a predictive text algorithm in R that took a user's text input and suggested the next most likely word to be entered.  Major steps in the project:  

- Getting and Cleaning [Data] (https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)
- Exploratory Data Analysis [Milestone Report](http://rpubs.com/bryanaguiar/capstonemilestone)
- Develop and Test Prediction [Algorithm] (http://en.wikipedia.org/wiki/Katz%27s_back-off_model)
- Develop, Test, and Deploy a [ShinyApp] (https://drbaguiar.shinyapps.io/Coursera_Capstone/)
- Prepare Final R Presentation

Prediction Algorithm
========================================================
- n-gram model was built based on the n-gram analysis of a random sample of 2% of the data.
-  [Katz back-off] (http://en.wikipedia.org/wiki/Katz%27s_back-off_model) model was used to estimate the conditional probability of a given word:

      - search for the most frequent quad gram, if no word exists,
    
      - search for the most frequent trigram, if no word exists,
    
      - search for the most frequent bigram, and if no word exists,
    
      - use the most frequent unigram.
- results presented in a bar chart in descend order of likeliness.

Example of the App
========================================================
- [Application](https://drbaguiar.shinyapps.io/Coursera_Capstone/)
- Type word sequence in left box. See top predicted words in the barplot on the right
- Tried to balance predictive accurary and speed for use on a smart phone

