##################################################
### Getting Started with MongoDB
Here are instructions for installing and getting started with MongoDB:

Installation:  http://docs.mongodb.org/manual/installation/
Getting Started: http://docs.mongodb.org/manual/tutorial/getting-started/
Here are three handy references for using MongoDB:

MongoDB reference cards (you need to fill out form with your contact information): https://www.mongodb.com/lp/misc/quick-reference-cards
“SQL to MongoDB Mapping Chart,” https://docs.mongodb.org/manual/reference/sql-comparison/
MongoDB cheat sheet (earlier version): https://blog.codecentric.de/files/2012/12/MongoDB-CheatSheet-v1_0.pdf

##################################################
### Loading data into a mongodb database
The mongoimport command line utility lets you bulk-load information (e.g. from a json or csv file) into a mongodb database.  A description of how to use mongoimport is provided here:

http://docs.mongodb.org/manual/reference/program/mongoimport/#bin.mongoimport

If you want, you can practice using mongodb to import a file containing all of the U.S. zip codes, that is referenced here:

http://docs.mongodb.org/manual/tutorial/aggregation-zip-code-data-set/

##################################################
### R and MongoDB
The most popular R package to work with MongoDB at this point in time seems to be rmongodb.  [RMongo is also well-regarded].

Here is some sample code to get started with rmongodb:

https://gist.github.com/Btibert3/7751989

##################################################
### Optional: A graphical user interface for MongoDB admin?
If you want to try out an "Admin UI" for MongoDB (like MySQL WorkBench provides for MySQL), mongodb.org maintains a list here:  http://docs.mongodb.org/ecosystem/tools/administration-interfaces/.   Many of these tools are single platform, commercial, and/or require installation of additional software (such as .NET or a PHP server).  

##################################################
### NoSQL Introduced
https://youtu.be/GcaZmd0VtYE

### Document Databases
https://youtu.be/Nh6Y7DgZDrg

##################################################
### TO CONSIDER: Code First?
When software developers build out applications from requirements, they often start with either the user interface design or the database design.  Recently, many developers have adopted “schema-less” databases like MongoDB.  Choosing MongoDB means that the developer could postpone designing a database schema, or in some cases, do away entirely with a fixed schema. 

What are the advantages and disadvantages of a “code first” vs. “data first” design methodology? 

Give specific examples, considering factors like how business logic is handled, and the number of different applications that work with the given data.
