# https://dev.mysql.com/downloads/connector/python/

import mysql.connector as mc

print(mc.__version__)


connection = mc.connect(user='root', password='jamiel',
                              host='127.0.0.1', database='myfirstdb',
                              auth_plugin='mysql_native_password')


result = connection.cmd_query("select * from orders")
print(result)
rows = connection.get_rows() # can call this only once
for k,v in rows[0]:
    print(k,v)

connection.close()