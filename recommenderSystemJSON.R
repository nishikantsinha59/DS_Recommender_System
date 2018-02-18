#install.packages('igraph')
library(igraph) # Load the igraph package
library(jsonlite) # Load jsonlite package for reading JSON data

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

# Define function for setting configuration
setConfiguration <- function() {
  
  print("Set Configuration")
  
  # Set whether to recommend predicate which are not part of syllabus or not
  consider_predicate_not_part_of_syllabus_depth <- as.integer(readline(prompt="consider_predicate_not_part_of_syllabus_depth [Choice 0/1] = "))
  
  
  # Set the succes criteria for particular syllabus
  success_criteria <- as.double(readline(prompt="success_criteria [from 0.0 to 1.0] = "))
  
  # Create a list of configuration variables and return the list
  configuration <- list("predicate" = consider_predicate_not_part_of_syllabus_depth,
                        "success_criteria" = success_criteria)
  
  return(configuration)
}

recommender <- function(syllabus, nodeList) {
  
  # Call to setConfiguration function and store configuration setting value as list
  confval <- setConfiguration()
  
  # Declare and initialize varialble to store individual topic score
  topicScore <- as.integer(0)
 
  for(topic in V(syllabus)) {
    # Display the current recommendation and record value for pass
    print(paste("Current Topic ",V(syllabus)$name[topic]))
    pass <- nodeList[topic]
    
    if(pass == 0){
      print("Failed!!")
      if((confval$predicate == 1) & (V(syllabus)$name[topic] %in% c("D1","E1","F1")))
      {
        extraPredicate <- as_ids(neighbors(universal, v = (V(syllabus)$name[topic]), mode = "in"))
        if(all(extraPredicate %in% (V(syllabus)$name))){
          # Nothing to do
        }
        else {
          # Call function to handle special cases
          specialRecommendation((V(syllabus)$name[topic]), syllabus)
        }
      }
      readline(prompt = "Press any key to proceed")
      print("Moving to next topic")
      next()
      
    }
    else if(pass == 1) {  
      print("Passed!!")
      readline(prompt = "Press any key to proceed.")
      print("Moving to next topic")  
      topicScore <- (topicScore+1)
      
      # Check whether user achieved success criteria
      overallScore <- (topicScore/vcount(syllabus))
      if(signif(overallScore, digits = 1) == confval$success_criteria) {
        print("Success")
      }
      
      if(topicScore == vcount(syllabus)) {
        print("You Aced it")
      }
    }
  }
}

specialRecommendation <- function(topicName, syllabus) {
  # Find the immediate predicate which are not part of syllabus
  extraPredicate <- as_ids(neighbors(universal, v = topicName, mode = "in"))
  
  print("Recommended topic(Not part of Syllabus) - ")
  
    for(temp in extraPredicate) {
      if(temp %in% (V(syllabus)$name))
        next()
      print(temp)
  }
}

# Plot the course
visualizeGraph(universal)


# Select The Syllabus for the course
print("*************TOPIC RECOMMENDER SYSTEM*************")
print("Press Esc to exit RECOMMENDER SYSTEM anytime")
print("Enter JSON array of nodes id (like [node_id,...])")

# Ask user to input syllabus in JSON format
jsonArray <- readline(prompt='Sample Data ["A1","A2","A3","A4","B1","B2","B4","C1","C2","C3","C4"] = ')

# Ask user to enter pass or fail for respective node in JSON format
print("Enter pass or fail value in JSON format....")
json <- readline(prompt='Sample Data {"A1":1,"A2":0,"A3":1,"A4":1,"B1":1,"B2":0,"B4":1,"C1":1,"C2":0,"C3":1,"C4":1} = ')

# Convert JSON array to R object
syllabus <- fromJSON(jsonArray)
nodeValueList <- fromJSON(json)

# Create syllabus graph
syllabusGraph<-induced.subgraph(universal, 
                      which(V(universal)$name %in% syllabus))

# Visualize syllabus graph
visualizeGraph(syllabusGraph)

# Call recommender function
recommender(syllabusGraph, nodeValueList)
