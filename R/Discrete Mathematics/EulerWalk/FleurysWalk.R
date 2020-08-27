
#install.packages("igraph")
library('igraph')

#create a graph using a data frame

euler.makeDataFrame <- function(n_vertices) {
	node_names = c(1:n_vertices)

	#to make graph, we're going to create 'n_edges' edges between the nodes
	all_possible_edges_df = t(combn(node_names, m=2)) #find all possible combinations of the nodes

	#create a graph using a data frame
	#-------
	set.seed(as.numeric(Sys.time()))
	edges_df = data.frame(all_possible_edges_df[sample(1:nrow(all_possible_edges_df), n_vertices),], stringsAsFactors = FALSE) #randomly select 'n_edges' of these rows
	return(edges_df)
	
}

euler.makeDataFrame(6)

drawGraph <- function(edges_df) {	
	node_names <- c(1:nrow(edges_df))
	dfgraph <- graph_from_data_frame(edges_df, directed = FALSE, vertices = node_names) 
	coords <- layout_(dfgraph, as_star())
	plot(dfgraph, layout=coords)
}

coordGraph <- function(dfgraph) {
	coords <- layout_(dfgraph, as_star())
	return(coords)
}

drawGraphColor <- function(edges_df, x) {	
	node_names <- c(1:nrow(edges_df))
	dfgraph <- graph_from_data_frame(edges_df, directed = FALSE, vertices = node_names, edges.color = c("red", "black")[1+V(dfgraph)$name%in%x])
	coords <- layout_(dfgraph, as_star())
	plot(dfgraph, layout=coords)
}
