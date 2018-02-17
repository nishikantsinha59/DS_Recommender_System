getwd()
setwd('F://Data Science')
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


recommender <- function(syllabus, nodeList) {
  
  # Declare and initialize topic score
  topicScore <- as.integer(0)
 
  for(topic in V(syllabus)) {
    print(paste("Current Topic",V(syllabus)$name[topic]))
    pass <- nodeList[topic]
    
    if(pass == 0){
      print("Failed!!")
      readline(prompt = "Press any key to go to next topic.")
      print("Moving to next topic")
      next()
      
    }
    else if(pass == 1) {  
      print("Passed!!")
      readline(prompt = "Press any key to go to next topic.")
      print("Moving to next topic")  
      topicScore <- (topicScore+1)
    }
    
    
    overallScore <- (topicScore/vcount(syllabus))
    if(overallScore > 0.7) {
      print("Success")
    }
    
    if(topicScore == vcount(syllabus)) {
      print("You Aced it")
    }
    
  }
  
}


# Plot the course
visualizeGraph(universal)


# Select The Syllabus for the course
print("*************TOPIC RECOMMENDER SYSTEM*************")
print("Press Esc to exit RECOMMENDER SYSTEM anytime")
print("Enter JSON array of nodes id (like [node_id,...])")

# Ask user to input syllabus in JSON format
jsonArray <- readline(prompt='Enter JSON array...... Sample Data ["A1","A2","A3","A4","B1","B2","B4","C1","C2","C3","C4"] - ')

# Ask user to enter pass or fail for respective node in JSON format
json <- readline(prompt='Enter pass or fail value in JSON format.... Sample Data {"A1":1,"A2":0,"A3":1,"A4":1,"B1":1,"B2":0,"B4":1,"C1":1,"C2":0,"C3":1,"C4":1}')

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
