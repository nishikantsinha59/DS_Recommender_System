# Data Science Recommender System
Nishikant Sinha - February 2018

This recommender System is aimed to recommend topics belonging to a particular course. The recommender system uses the concept of Network analysis with igraph package.

Prerequisites-
          
    a) R and RStudio
    b) R packages - tidyverse, igraph and jsonlite

This repository contains code for two diffrent recommender System aimed to recommend topics belonging to a particular course. Their core functionality is same but they only differ in the way they take inputs. One takes input as regular console while other requires input in JSON array or object.

1) recommenderSystem.R - This one contains predifined universal course graph and the user is given control to select the syllabus from available option. After selecting the syllabus user will be asked to give input as pass(1) or fail(0) and based on the result the recommendation will be made.

Instructions for runninng recommenderSystem.R

      a) Run recommenderSystem.R using source() command in RStudio console.      
      b) Select the syllabus from the given option.
      c) Set configuration for recommending predicates not part of syllabus, traversal depth allowed in case of failure and success criteria as instructed.
      d) The system will recommend the topic and will prompt the user to input value of Pass/Fail.
      e) The system will display "Sucess" message if user meets the sucess criteria and "You Aced it" message if user all topics are completed successfully.
      f) You can exit the recommender system ay time by pressing "esc" key.
      
2) recommenderSystemJSON.R - This system also has a predefined universal course graph but unlike the former system this will prompt user to input JSON array for syllabus having nodes of a particular syllabus. This will also prompt the user to enter Pass/ Fail value for each node as JSON object.

Instructions for runninng recommenderSystemJSON.R

      a) Run recommenderSystemJSON.R using source() command in RStudio console.      
      b) Input the syllabus node in JSON array format as given in sample data.
      c) Input the pass/fail value for each nodes of syllabus in form of JSON object as given in sample data.
      d) Set configuration for recommending predicates not part of syllabus and success criteria as instructed.
      e) The system will recommend the topic and will prompt the user to input any key to proceed.
      f) The system will display "Sucess" message if user meets the sucess criteria and "You Aced it" message if user all topics are completed successfully.
      g) You can exit the recommender system ay time by pressing "esc" key.

If you notice any bugs or typos, or have any suggestions on making the tutorial easier to follow, please send me a direct message through any of ...

Email : nishikantsinha59@gmail.com

Kaggle: https://www.kaggle.com/nishikantsinha59

Kind regards,

Nishikant Sinha
