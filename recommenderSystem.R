
#install.packages('igraph')
library(igraph) # Load the igraph package

# Create universal graph of the course
universal <- graph( edges=c('A1','A2', 'A1','A3','A2','A4','A3','A4',
                            'B1','B2','B1','B3','B2','B4',
                            'C1','C2','C3','C4','C2','C4',
                            'A4','D1','B4','D1','D1','D2','D1','D3','D3','D4',
                            'C4','E1','E1','E3','E3','E4','E4','F1',
                            'D4','F1','F1','F2','F1','F3','F3','F4'),
                    isolates=c('E2'),
                    directed=T ) 

# Function for plotting grpahs
visualizeGraph <- function(g) {
  plot(g, edge.arrow.size=.2, vertex.color="gold", vertex.size=15, 
       
       vertex.frame.color="gray", vertex.label.color="black", 
       
       vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0.2)
}


# Define function for syllabus 1
syll1 <- function() {
  # Create syllabus of semester 1 by instructor A
  syllabus1<-induced.subgraph(universal, 
                              which(V(universal)$name %in% c("A1","A2","A3","A4","B1","B2","B4","C1","C2","C3","C4")))
  
  # Plot syllabus of sem 1 by instructor A
  visualizeGraph(syllabus1)
  
  # Call Recommender Function
  recommender(syllabus1)
}


# Define function for syllabus 2
syll2 <- function() {
  # Create syllabus of semester 1 by instructor B
  syllabus2<-induced.subgraph(universal, 
                              which(V(universal)$name %in% c("A1","A2","A3","A4","B1","B2","B4","D1","D3","D4")))
  
  # Plot syllabus of sem 1 by instructor B
  visualizeGraph(syllabus2)
  
  # Call Recommender Function
  recommender(syllabus2)
}


# Define function for syllabus 3
syll3 <- function() {
  # Create syllabus of semester 2 by instructor A
  syllabus3<-induced.subgraph(universal, 
                              which(V(universal)$name %in% c("D1","D2","D3","D4","E1","E3","E4","F1","F2","F3","F4")))
  
  # Plot syllabus of sem 2 by instructor A
  visualizeGraph(syllabus3)
  
  # Call Recommender Function
  recommender(syllabus3)
}


# Define function for syllabus 4
syll4 <- function() {
  # Create syllabus of semester 2 by instructor B
  syllabus4<-induced.subgraph(universal, 
                              which(V(universal)$name %in% c("C1","C2","C3","C4","E1","E2","E3","E4","F1","F2","F3","F4")))
  
  # Plot syllabus of sem 2 by instructor A
  visualizeGraph(syllabus4)
  
  # Call Recommender Function
  recommender(syllabus4)
}


# Define function for setting configuration
setConfiguration <- function() {
  
  print("Set Configuration")

  # Set whether to recommend predicate which are not part of syllabus or not
  consider_predicate_not_part_of_syllabus_depth <- as.integer(readline(prompt="consider_predicate_not_part_of_syllabus_depth [Choice 0/1] - "))

  # Set the level allowed for recommender to go from parent node in case of failure
  traverse_depth_to_child_if_parent_efficacy_not_met <- as.integer(readline(prompt ="Set traverse_depth_to_child_if_parent_efficacy_not_met to [Choice 0/1/2] - "))

  # Set the succes criteria for particular syllabus
  success_criteria <- as.double(readline(prompt="success_criteria [from 0.0 to 1.0] - "))
  
  # Create a list of configuration variables and return the list
  configuration <- list("predicate" = consider_predicate_not_part_of_syllabus_depth,
                        "efficacy" = traverse_depth_to_child_if_parent_efficacy_not_met,
                        "success_criteria" = success_criteria)
  
  return(configuration)
  }


# Define the core function which will recommend the topics of a particular syllabus
recommender <- function(syllabus) {
  
  # Call to setConfiguration function and store configuration setting value as list
  confval <- setConfiguration()
  
  # Declare and initialize varialble to store individual topic score
  topicScore <- as.integer(0)
  
  # Declare and initialize varialble to keep track of consecutive fails
  consecutiveFails <- as.integer(1)
  
  # Begin recommendation from first topic and keep iterating till end
  for(topic in V(syllabus)) {
    
    # Check whether user achieved success criteria
    overallScore <- (topicScore/vcount(syllabus))
    if(signif(overallScore, digits = 1) == confval$success_criteria) {
      print("Success")
    }
    
    
    # Display the current recommendation and record value for pass
    print(paste("Current Topic",V(syllabus)$name[topic]))
    pass <- as.integer(readline(prompt="Enter Pass(1) or Fail(0) : "))
    
    # Recommend predicate which are not part of syllabus
    if((pass == 0) & (confval$predicate == 1) & (V(syllabus)$name[topic] %in% c("D1","E1","F1")))
    {
      extraPredicate <- as_ids(neighbors(universal, v = (V(syllabus)$name[topic]), mode = "in"))
      if(all(extraPredicate %in% (V(syllabus)$name))){
         #print("inside new if")
        }
      else {
        # Call function to handle special cases
        specialRecommendation((V(syllabus)$name[topic]), syllabus)
        consecutiveFails <- as.integer(1)
        print(paste("Current Topic",V(syllabus)$name[topic]))
        pass <- as.integer(readline(prompt="Enter Pass(1) or Fail(0) : "))
      }
    }
    
    # Logic for recommending topic, when user failed in any current topic
    if(pass == 0 ){
      print("Failed!!!!")
      consecutiveFails <- (consecutiveFails+1)
      
      if(consecutiveFails == (confval$efficacy+2)) {
        while(pass == 0) {
          print(paste("Failed!!! Cannot proceed without passing ", V(syllabus)$name[topic]))
          pass <- as.integer(readline(prompt="Enter Pass(1) or Fail(0) : "))
        }
        print("Passed!!!")
        consecutiveFails <- as.integer(1)
        topicScore <- (topicScore+1)
      }
      print("Moving to next topic")
      next()
      
    }
    
    # Logic for recommending topic, when user passed in any current topic
    else if(pass == 1) {  
      consecutiveFails <- as.integer(1)
      print("Passed!! Moving to next topic")
      topicScore <- (topicScore+1)
    }
    # Check whether user have successfully passed in all topics
    if(topicScore == vcount(syllabus)) {
      print("You Aced it")
    }  
  } 
    
  }

specialRecommendation <- function(topicName, syllabus) {
  # Find the immediate predicate which are not part of syllabus
  extraPredicate <- as_ids(neighbors(universal, v = topicName, mode = "in"))
  
  print("Currently recommended topic - ")
  
  if(length(extraPredicate) > 1) {
      for(temp in extraPredicate) {
        if(temp %in% (V(syllabus)$name))
          next()
        print(temp)
        #vector = c(vector,extraPredicate[temp])
    }
    print("Choose from topic from recommendation")
    #print(vector)
    chosenTopic <- readline(prompt="Enter your choice : ")
    print(paste("Choosen Topic - ",chosenTopic))
  }
  else {
    # Display the current recommendation
    chosenTopic <- extraPredicate
  }
  
  pass <- as.integer(readline(prompt="Enter Pass(1) or Fail(0) : "))
  
  while(pass == 0) {
    print(paste("Cannot proceed without passing - ",chosenTopic))
    pass <- as.integer(readline(prompt="Enter Pass(1) or Fail(0) : "))
  }
  
  #print("Moving to next topic")
  
}

# Plot the course
visualizeGraph(universal)

# Select The Syllabus for the course
print("*************TOPIC RECOMMENDER SYSTEM*************")
print("Select Course Syllabus")
print("1.Semester 1 by Instructor A")
print("2.Semester 1 by Instructor B")
print("3.Semester 2 by Instructor A")
print("4.Semester 2 by Instructor B")
print("5.Press Esc to exit RECOMMENDER SYSTEM anytime")

choice = as.integer(readline(prompt="Enter choice[1/2/3/4]: "))
switch(choice, syll1(), syll2(), syll3(), syll4())


