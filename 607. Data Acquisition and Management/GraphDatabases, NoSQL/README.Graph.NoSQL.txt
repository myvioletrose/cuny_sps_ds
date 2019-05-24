###########################################
### Overview
This week, we'll introduce graph databases, and learn about using Neo4j, which is currently the most popular Graph DBMS.  Data Scientists work with graphs both in social network analysis, and in logistical work, such as optimizing transportation networks.

Events and Deliverables

###########################################
### Getting Started with Neo4j

Here is Neo4j's Getting Started page:

http://neo4j.com/developer/get-started/

Take a look at this free, on-line tutorial on Neo4j and Graph databases:

http://neo4j.com/graphacademy/online-course/

Here are some tutorials that you may find helpful:

http://neo4j.com/docs/stable/tutorials.html

Here are some specific instructions for loading data from a PostgreSQL database into Neo4j:

http://neo4j.com/developer/guide-importing-data-and-etl/

Here is an example of how graph databases are being used:

Steven Mellendez, “NASA is Harnessing Graph Databases to Organize Lessons Learned from Past Projects,” Fast Company, Nov 2, 2016.  https://www.fastcompany.com/3065044/mind-and-machine/nasa-is-harnessing-graph-databases-to-organize-lessons-learned-from-past-pr

This O'Reilly book, Graph Databases, has been made freely downloadable.  

http://info.neotechnology.com/rs/neotechnology/images/GraphDatabases.pdf 

###########################################
### Other learning materials on working with graphs (optional)

Graph Theory

Students with either a background in mathematics or who have taken a discrete math course in a computer science curriculum will have already done some work with network analysis, graph theory and graph algorithms.

To get an overview (or a refresher), the Khan academy material on Graph Representation and Breadth-first search provide a good starting point:

Graph Representation, https://www.khanacademy.org/computing/computer-science/algorithms#graph-representation
Breadth-first search, https://www.khanacademy.org/computing/computer-science/algorithms#breadth-first-search

iGraph
Attached Files:
File IntrotoiGraph.R (2.687 KB)
File Graph Database Model Relationships.pdf (814.305 KB)
File flights.csv (558 B)
Instead of using Neo4j, you can use an R package like iGraph to model your graph data.  The attached presentation slide deck, dataset, and code, and the recorded, related Office Hours from a previous semester provide some initial guidance on using iGraph.

### iGraph
https://youtu.be/QATUSFTynN4

######################################################################################
######################################################################################
Week 13 assignment - NoSQL migration
For this assignment, you should take information from a relational database and migrate it to a NoSQL database of your own choosing. 

For the relational database, you might use the flights database, the tb database, the "data skills" database your team created for Project 3, or another database of your own choosing or creation.

For the NoSQL database, you may use MongoDB (which we introduced in week 7), Neo4j, or another NoSQL database of your choosing.

Your migration process needs to be reproducible.  R code is encouraged, but not required.  You should also briefly describe the advantages and disadvantages of storing the data in a relational database vs. your NoSQL database.

You may work on a small team on this project.  Due end of day Sunday April 29.



