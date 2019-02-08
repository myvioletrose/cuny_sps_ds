########################################################################################################################
# where to put files for upload to MySql
# The method of loading from local (.CSV) files is faster, and technically preferable.  
# But MySQL tightened up the default security configurations when they moved from 5.7 to 8.0 in 2018.  
# To find the location where you need to copy the .CSV files in the default configuration, run
SHOW VARIABLES LIKE "secure_file_priv";
# On a Windows machine, this might return C:\ProgramData\MySQL\MySQL Server 8.0\Uploads\.

############################################################
# https://www.r-bloggers.com/accessing-mysql-through-r/
# https://stackoverflow.com/questions/49194719/authentication-plugin-caching-sha2-password-cannot-be-loaded
# in order to access the db inside RStudio, let's change the setting in MySql using its command line
ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'Wtf12345';