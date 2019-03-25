# load packages
library(RMySQL)

#Use this connection if a db hasn't been created.
mdb <- DBI::dbConnect(
  RMySQL::MySQL(),
  host = "35.226.125.86",
  user = "root",
  password = "rootpass"#rstudioapi::askForPassword("Database password"))
  
# Create the db
dbSendQuery(mdb, 'CREATE SCHEMA `project3`')
  
  
#Connection with schema specified
 mdb2 <- DBI::dbConnect(
  RMySQL::MySQL(),
  dbname = 'project3',
  host = "35.226.125.86",
  user = "root",
  password = rstudioapi::askForPassword("Database password")
    
# Create db tables and write data frame contents
dbWriteTable(mdb2, 'company', company, row.names = FALSE)
dbWriteTable(mdb2, 'job_postion', job_position, row.names = FALSE)
dbWriteTable(mdb2, 'job_post_specific', job_post_specific, row.names = FALSE)
dbWriteTable(mdb2, 'description', description, row.names = FALSE)


# TOD0: Set table relationships