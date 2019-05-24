############### Working with Data in the Cloud; Spark and Hadoop ###############

### Overview
This week, we'll look at cloud computing,  distributed processing paradigms, Hadoop, and Spark.

Arguably, the two most important platforms today are Amazon Web Services and Microsoft Azure.  Google Cloud is increasingly important and compelling.  DataBricks Community Edition--which is a cloud environment built on top of AWS--is a great, low-cost Spark learning environment. 

You're encourage to download Hadoop and work with it on a virtual machine.  

There will be no more new discussion items, so that you can focus on your final project.

###############################################
### Working in the Cloud

# Learning more about Cloud Computing
Attached Files:
File Data Science in the Cloud.pdf (396.036 KB)
electric grid

"Era of the Cloud: Nicholas Carr (Google Atmosphere Session 2)," [video, 0:31], https://www.youtube.com/watch?v=BYP3uMOobqk. Oct 28, 2009.  History of Cloud computing, starting in upstate New York in 1851.
The attached slide deck also provides some background and context about doing data science in the cloud.

# Learning More about Amazon Web Services (OPTIONAL)
Here is the information for signing up for AWS's free tier: https://aws.amazon.com/free/

And here is AWS's Getting Started Guide: https://aws.amazon.com/getting-started/


Here are some fast-path instructions for setting up RStudio on AWS.  These instructions are bare-bones in terms of security, and you want to make sure you sign up for the free-tier (1 year), unless you're willing to pay charges as you go.

"RStudio Server Amazon Machine Image (AMI)," http://www.louisaslett.com/RStudio_AMI/
"RStudio in the cloud, for dummies," Ken Kleinman, http://www.r-bloggers.com/rstudio-in-the-cloud-for-dummies/.  Feb 13, 2012.
"Connecting to Your Linux Instance from Windows Using PuTTY," https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html

# Learning more about Microsoft Azure (OPTIONAL)
Sign up for Microsoft Azure's 30 day ($200 in usage) trial: https://azure.microsoft.com/en-us/pricing/free-trial/
Getting started videos for Azure: https://azure.microsoft.com/en-us/get-started/
Microsoft's jump start course for Azure Machine Learning:  https://mva.microsoft.com/en-us/training-courses/getting-started-with-microsoft-azure-machine-learning-8425
"Data Science with Azure Machine Learning, SQL Server and R, Rafal Lukawiecki," https://sec.ch9.ms/sessions/ignite/2015/BRK3550.mp4, [video 1:13].  Overview of Microsoft's cloud offering for machine learning, with demonstrations.
Data Science in the Cloud with Microsoft Azure Machine Learning and R, Stephen Elston, http://www.oreilly.com/data/free/data-science-in-the-cloud.csp, O’Reilly report.  February 2015.  Freely downloadable, but you need to provide your contact information.


###############################################
### Distributed Processing Paradigms

# Performance
One motivation for running R programs in the cloud is to be able to run on a machine with a lot of RAM and fast computational speed.  Here is some additional reading on ways to improve performance in your R code.   Julia is basically a subset of R with some rough edges.

Hadley Wickham's Advanced R book has five superb chapters on performance (about 12o written pages).  Read what he writes about making your R code faster here:  http://adv-r.had.co.nz/ [start here: http://adv-r.had.co.nz/Performance.html ]
There is a nice tutorial on Julia at the Julia Studio site here:  http://forio.com/labs/julia-studio/tutorials/  This is exciting but raw stuff--I had to go to stack overflow to get Julia Studio to even load with the newest stable release of Julia. 

# Learning more about MapReduce
Attached Files:
File Map Reduce from R in a Nutshell.pdf (346.285 KB)
Please read/watch the following:

To get an overview of the MapReduce paradigm, I'd start with chapter 2 from Mining Massive Datasets by  Jure Leskovec, Anand Rajaraman, and Jeff Ullman.  The authors also periodically offer a free related Stanford/Coursera course here.

“Introducing Apache Hadoop to Developers,” http://hortonworks.com/hadoop-tutorial/introducing-apache-hadoop-developers/
“How MapReduce Works,” from Joseph Adler’s R in a Nutshell, 2nd edition, O’Reilly 2012.  Excerpt attached.
“Learn MapReduce with Playing Cards,”  Jesse Anderson, https://www.youtube.com/watch?v=bcjSe0xCHbE.  Aug 14, 2013 [0 h 10 m].

# Learning more about MapReduce
https://youtu.be/bcjSe0xCHbE

# Text in Distributed Systems [VIDEO series]
https://youtu.be/qrxqcnjOOGc
https://youtu.be/RAFHG5mKi-M
https://youtu.be/OlD9CMDNah0
https://youtu.be/egOk_EXWkJ4
https://youtu.be/XjX8_m-u2wY

###############################################
### Hadoop

# Learning more about Hadoop
Attached Files:
File Hadoop Overview.pdf (1.456 MB)
Here are some resources to jump start your Hadoop learning: 

The Hadoop ecosystem is huge, and is growing in many directions at once.  If you want to get an overview for where to begin, you may find this article helpful:  "Learning How to Learn Hadoop, Rich Morrow," http://www.globalknowledge.com/training/generic.asp?pageid=3438
Here is a webinar by Cloudera's chief scientist that talks about problems that Hadoop solves: "10 Common Hadoopable Problems," webinar, [1:04], Jeff Hammerbacher,  http://www.cloudera.com/content/cloudera/en/resources/library/recordedwebinar/10-common-hadoop-able-problems-recorded-webinar.html, August 5, 2010.
I think that the single best book on Hadoop is Hadoop: The Definitive Guide 4/e, Tom White, O'Reilly, April 11, 2015. 
The attached slide deck looks at the motivation that led to Hadoop, its architecture, the major choices in development and run time environments, and some use cases.

# Setting up a Virtual Machine

If you want to have a local Hadoop sandbox to play in, you can freely download Oracle’s Virtual Box and either Cloudera's or Hortonwork’s Hadoop sandbox.   Cloudera is the market leader; I prefer Hortonworks' excellent tutorial materials on HDFS, MapReduce, and other components of the Hadoop ecosystem.

Oracle Virtual Box Virtual Machine, https://www.virtualbox.org/
Hortonworks Hadoop Sandbox, http://hortonworks.com/products/hortonworks-sandbox/
Cloudera Virtual Machine, http://www.cloudera.com/content/www/en-us/downloads/quickstart_vms/5-4.html
You’ll want a modern PC or Mac with (preferably) 8+ GB of ram and ~20 GB of free disk space.

###############################################
### Spark
 
# sparklyr
An alternative to using sparkr for working with R on Spark is the sparklyr package.  sparklyr's approach is to use Spark's APIs to generate SparkSQL code, that run on the worker threads.

There is a lot of good content on sparklyr on the rstudio site, including this cheat sheet:

http://spark.rstudio.com/images/sparklyr-cheatsheet.pdf

And here is a good overview video on sparklyr:
https://youtu.be/oItFZfzqqMY

# Setting up Spark
Here are the instructions for downloading Apache Spark.  While PCs are supported, in my experience, it's easier on a Mac or a Unix box.

http://spark.apache.org/downloads.html

# Setting up Spark and AWS
Attached Files:
File awsscripts.txt (961 B)
http://spark.apache.org/docs/latest/ec2-scripts.html

Getting a Spark/AWS environment set up on a PC requires some small adjustments.  Specifically,

Where the Mac bash shell and Linux provide a secure shell command, ssh, there is no built-in equivalent on the PC.   I recommend using PuTTY here.  The .PEM file that you create for a Mac doesn't work with PuTTY, so you'll also need to use PuTTYgen to create a .PPK file from a .PEM file

Where Mac and Linux copy files use scp to securely copy files, on a PC you need to use a tool like pscp.exe (that comes with PuTTY) or another alternative (such as the graphical WinSCP).

The following 18 minute video shows a walk through of setting up a Spark ready cluster on AWS from a PC.  At the end of the process, you'll have a launching node, one master node, and two slave nodes.  I've also attached a script file with some of the code shown in the video.  You should treat most of the code in this script as template code--you'll need to modify based on the DNS name, IP address, and region that AWS assigns your node(s).

https://youtu.be/KwMBTSJgMhQ

Here are some additional resources that you may also find helpful.

“Supercharge R with Spark: Getting Apache's SparkR Up and Running on Amazon Web Services (AWS),” Manuel Amunategui, http://amunategui.github.io/sparkr/.  Sep 30, 2015.  (Mac-based)

“Using Putty to Connect to an Amazon EC2,” Dan Morrill, https://www.youtube.com/watch?v=8Dsq4MeVh8M.  Jan 9, 2013.  [3 minute video]

# Conceptual Overview of Apache Spark and SparkR
Here is a nice conceptual overview of Spark and SparkR:  http://blog.revolutionanalytics.com/2015/01/a-first-look-at-spark.html.

I highly recommend the referenced YouTube presentation [1 hour 10 minutes] by Reza Zadeh:

https://youtu.be/-TiMNoj7Rrs?list=PL87GtQd0bfJzt1mZZQSj7NCRLeFdh_How

# Spark and SparkSQL
Here is a UC Berkeley AMP Camp talk by SparkSQL creator Michael Armbrust: 

https://youtu.be/KiAnxVo8aQY

# Apache Hive
Here is some "getting started" information on Hive, a SQL-like language for Hadoop.

https://youtu.be/Pn7Sp2-hUXE
Source: “Hadoop Tutorial: Apache Hive," Hortonworks.  [17 min] https://www.youtube.com/watch?v=Pn7Sp2-hUXE.  See also http://hortonworks.com/hadoop-tutorial/how-to-process-data-with-apache-hive/

# pySpark
DataCamp provides a pySpark cheat sheet here:

https://s3.amazonaws.com/assets.datacamp.com/blog_assets/PySpark_Cheat_Sheet_Python.pdf 	





























