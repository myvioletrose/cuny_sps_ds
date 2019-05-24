# introtoigraph.R
# sources:
#  (1) Github site for Statistical Analysis of Network Data with R, https://github.com/kolaczyk/sand
#  (2) Jared Lander

#install.packages("igraph")
library(igraph)

g.full <- graph.full(7)
g.ring <- graph.ring(7)
g.tree <- graph.tree(7, children=2,
                     mode="undirected")
g.star <- graph.star(7,
                     mode="undirected")
par(mfrow=c(2, 2))
plot(g.full)
plot(g.ring)
plot(g.tree)
plot(g.star)

dg <- graph.formula(Sam-+Mary, 
                    Sam-+Tom, Mary++Tom)
plot(dg)

dg <- graph.formula(Honolulu++'Los Angeles', 
                    'Los Angeles'-+'New York', 'New York'-+'Honolulu')
plot(dg)

get.adjacency(dg)

E(dg)$flighttime <- c(362, 495, 800)

# igraph edges can loop back on same vertex
g <- graph(c(1,1, 1,2, 1,3, 2,3, 4,5), n=5)
print.igraph(g, full=TRUE)
plot(g)

df.flights <- read.table("c://data//flights.csv", sep=",", header=TRUE,
                         stringsAsFactors=FALSE)
head(df.flights)
str(df.flights)

# edgelist, since each row represents an edge
flights <- graph.data.frame(df.flights, directed=TRUE)

print.igraph(flights, full=TRUE)
plot(flights)

# list all edges, vertices
E(flights)
V(flights)
vcount(flights)
ecount(flights)

# make this directed graph into an undirected graph
flights2 <- as.undirected(flights)
print.igraph(flights2, full=TRUE)

# plot no longer has arrrows
plot(flights2)
E(flights2)
ecount(flights2)
vcount(flights2)

# thinner line - less flight time
plot(flights, layout=layout.fruchterman.reingold, edge.width=E(flights)$flighttime/100)

plot(flights, layout=layout.kamada.kawai)

# metrics
average.path.length(flights)
diameter(flights)
farthest.nodes(flights)
V(flights)[farthest.nodes(flights)]

# shortest path (time or distance?),  here, hops
shortest.paths(flights)

heatmap(shortest.paths(flights))

# how many edges come out of each node?
degree(flights)
hist(degree(flights))

# shortest path (time or distance?),  here, hops
shortest.paths(flights)

heatmap(shortest.paths(flights))

V(flights)[degree(flights) >= 5]$color <- "green"
V(flights)[degree(flights) <= 10]$color <- "red"
plot(flights)

plot(flights, edge.width=E(flights)$flighttime/100)

flights3 <- flights

# now, shortest paths will be weighted by time (instead of hops)
E(flights3)$weight <- E(flights)$flighttime

heatmap(shortest.paths(flights3))
shortest.paths(flights3)


# takes edgelist (not graph) as first argument

#install.packages("d3Network")

library(d3Network)

setwd("c:/data")
getwd()

d3SimpleNetwork(df.flights, Source="depart", Target = "arrive", width=1200, height=800, file="RouteMap.html")








