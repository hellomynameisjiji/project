
graph1 <- graph (edges = c(1,2, 2,3, 3,4, 4,5, 1,5), n = 5, directed = F)
plot(graph1)
dev.off()

E(graph1)		# edges
V(graph1)

graph2 <- graph( c("1", "2", "2", "3", "2", "3", "1", "1"), 
             isolates=c("4", "5", "6", "7") )  

plot(graph2, edge.arrow.size=.5, vertex.color="gold", vertex.size=15,  #Plotting asthetics
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 


drawGraph <- function(n_edges) {	
	node_names = c(1:n_edges)

	#to make graph, we're going to create 'n_edges' edges between the nodes
	all_possible_edges_df = t(combn(node_names, m=2)) #find all possible combinations of the nodes

	#create a graph using a data frame
	#-------
	set.seed(as.numeric(Sys.time()))
	edges_df = data.frame(all_possible_edges_df[sample(1:nrow(all_possible_edges_df), n_edges),], stringsAsFactors = FALSE) #randomly select 'n_edges' of these rows
	edges_df
	
	dfgraph <<- graph_from_data_frame(edges_df, directed = FALSE, vertices = node_names) 
	plot(dfgraph)
	
	duplicated_graph <<- dfgraph
}
