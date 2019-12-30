import sys
PYTHON_MAJOR_VERSION_REQUIRED = 3
PYTHON_MINOR_VERSION_REQUIRED = 7
print('Checking for Python version')
python_major_version = sys.version_info[0]
python_minor_version = sys.version_info[1]
print('Python version installed:',python_major_version,'.',python_minor_version)
if not python_major_version==PYTHON_MAJOR_VERSION_REQUIRED and not python_minor_version==PYTHON_MINOR_VERSION_REQUIRED:
    print('Incorrect Python version installed')

# core
import csv
import datetime
import hashlib
import json
import math
import matplotlib.pyplot
import unittest
import numpy
import pandas
import re
import requests
import uuid

# flask
from flask import Flask
from flask import request

# mysql
import mysql.connector as mc

# sklearn
from sklearn import datasets as ds
from sklearn import linear_model as lm
from sklearn.neighbors import KNeighborsClassifier as KNN
from sklearn.model_selection import train_test_split as tts


