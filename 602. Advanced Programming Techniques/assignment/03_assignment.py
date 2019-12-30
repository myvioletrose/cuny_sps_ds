'''
Assignment #3
1. Add / modify code ONLY between the marked areas (i.e. "Place code below"). Do not modify or add code elsewhere.
2. Run the associated test harness for a basic check on completeness. A successful run of the test cases does not guarantee accuracy or fulfillment of the requirements. Please do not submit your work if test cases fail.
3. To run unit tests simply use the below command after filling in all of the code:
    python 03_assignment.py
  
4. Unless explicitly stated, please do not import any additional libraries but feel free to use built-in Python packages
5. Submissions must be a Python file and not a notebook file (i.e *.ipynb)
6. Do not use global variables
7. Make sure your work is committed to your master branch in Github
8. Use the test cases to infer requirements wherever feasible
'''
import csv, json, math, pandas as pd, requests, unittest, uuid

# ------ Declare your classes here ONLY \/ \/ \/ ------

######################################
# exercise01 - Box class declaration #
######################################
class Box:
    
    """Create an immutable class Box that has private attributes length and width that takes values for length and width upon construction"""

    def __init__(self, length, width):        
        self.length = length        
        self.width = width
        
    def getlength(self):    
        return self.length

    def getwidth(self):        
        return self.width
        
    def render(self):         
        for i in range(self.length):            
            for j in range(self.width):                
                print("*", end = '')            
            print()
            
    def invert(self):                 
        self.length, self.width = self.width, self.length                 
        
    def get_area(self): 
        return self.length * self.width        
    
    def get_perimeter(self):        
        return (2 * self.length) + (2 * self.width)

    def double(self):
        self.length = 2 * self.length        
        self.width = 2 * self.width
    
    def  __eq__ (self, other):       
        """compare box X with box Y"""        
        return self.length == other.length and self.width == other.width
     
    def print_dim (self): 
        print("length is equal to ", self.length)        
        print("width is equal to ", self.width)
        
    def get_dim(self):        
        return self.length, self.width
        
    def combine (self, other):
        self.length = self.length + other.length        
        self.width = self.width + other.width      
    
    def get_hypot(self):
        return ((self.length**2) + (self.width**2))**0.5
    

##########################################
# exercise02 - MongoDB class declaration #
##########################################
class MangoDB:
    
    def __init__(self):        
        self.dbUUID = self.generateRandomUUID()    
        self.database = {}
        
    def generateRandomUUID(self):        
        return uuid.uuid1()
    
    # Take the test_scores list and insert it into the testscores collection, providing a sequential key i.e 1,2,3...
    def add_collection(self, key, scoreList):         
            self.database[key] = {}       
            i = 1        
            for e in scoreList:            
                self.database[key][i] = e            
                i += i + 1                
                
    def list_collections(self): 
            print('List of collection ' + (str)(self.database))
            
    def get_collection_size(self, key):         
            print('Size of collection ' + (str)(len(self.database[key])))
            
    def printDatabaseUUID(self):             
            print('Database UUID : ' + (str)(self.dbUUID))
            
    def wipe(self):        
        self.dbUUID = self.generateRandomUUID()        
        self.database = {}
        print('<wipe clean>')

# ------ Create your classes here /\ /\ /\ ------


def exercise03():

    '''
    1. Avocado toast is expensive but enormously yummy. What's going on with avocado prices? Read about avocado prices (https://www.kaggle.com/neuromusic/avocado-prices/home)
    2. Load the included avocado.csv file and display every other line to the screen, i.e. line 1,3,5..
    3. Open the file name under csv_file
    4. The reader should be named reader
    5. Use only the imported csv library to read and print out the avacodo file
    '''

    with open('avocado.csv') as csv_file:
        reader = csv.reader(csv_file)
        for count, line in enumerate(csv_file):
            if count % 2 == 0: 
                print(line)
                
    return csv_file, reader


def exercise02():

    '''
    Create a class called MangoDB. The MangoDB class wraps a dictionary of dictionaries. At the the root level, each key/value will be called a collection, similar to the terminology used by MongoDB, an inferior version of MangoDB ;) A collection is a series of 2nd level key/value paries. The root value key is the name of the collection and the value is another dictionary containing arbitrary data for that collection.
    For example:
        {
            'default0': {
            'version': v1.5,
            'db':'mangodb',
            'uuid':'0fd7575d-d331-41b7-9598-33d6c9a1eae3'
            },
        {
            'temperatures': {
                1: 50,
                2: 100,
                3: 120
            }
        }
    
    The above is a representation of a dictionary of dictionaries. Default0 and temperatures are collections or root keys. The default0 collection has a series of key/value pairs that make up the collection. The MangoDB class should create the default collection, similar to what's shown here:
                collection: default0
                    version: v1.0
                    db: mangodb
                    uuid: 739bd6e8-c458-402d-9f2b-7012594cd741 
    
    On a MangoDB instance being created include a randomly generated uuid using the uuid4() method and have the following methods:
        - display_all_collections() which iterates through every collection and prints to screen each collection names and the collection's content underneath and may look something like:
            collection: default0
                collection: default0
                    version: v1.0
                    db: mangodb
                    uuid: 739bd6e8-c458-402d-9f2b-7012594cd741 
            collection: temperatures
                1 50
                2 100 
        - add_collection(collection_name) allows the caller to add a new collection by providing a name. The collection will be empty but will have a name.
        - update_collection(collection_name,updates) allows the caller to insert new items into a collection i.e. 
                db = MangoDB()
                db.add_collection('temperatures')
                db.update_collection('temperatures',{1:50,2:100})
        - remove_collection() allows caller to delete the collection and its associated data
        - list_collections() displays a list of all the collections twice
        - get_collection_size(collection_name) finds the number of key/value pairs in a given collection
        - to_json(collection_name) that converts the collection to a JSON string
        - wipe entirely cleans out the db and resets it with just a default0 collection
        - get_collection_names() that returns a list of collection names, with each name prefixed with the string 'collection-' 
        Make sure to never expose the underlying data structures
        For exercise02(), perform the following:
        - Create an instance of MangoDB
        - Add a collection called testscores
        - Take the test_scores list and insert it into the testscores collection, providing a sequential key i.e 1,2,3...
        - Display the size of the testscores collection
        - Display a list of collections
        - Display the db's UUID
        - Wipe the database clean
        - Display the db's UUID again
    '''

    test_scores = [99,89,88,75,66,92,75,94,88,87,88,68,51]

    # ------ Place code below here \/ \/ \/ ------  
    
    # Create an instance of MangoDB
    mangodb = MangoDB()

    # Add a collection called testscores
    mangodb.add_collection('testscores', test_scores)   

    # Display the size of the testscores collection
    mangodb.get_collection_size('testscores')

    # Display a list of collections
    mangodb.list_collections()

    # Display the db's UUID
    mangodb.printDatabaseUUID()

    # Wipe the database clean
    mangodb.wipe()

    # Display the db's UUID again
    mangodb.printDatabaseUUID()    

    # ------ Place code above here /\ /\ /\ ------


def exercise01():

    '''
        Create an immutable class Box that has private attributes length and width that takes values for length and width upon construction (instantiation via the constructor). 
        Make sure to use Python 3 semantics. Make sure the length and width attributes are private and accessible only via getters.
        
        In addition, create...
        - A method called render() that prints out to the screen an unfilled box made with asterisks of length and width perimeter
        - A method called invert() that switches length and width with each other
        - Methods get_area() and get_perimeter() that return appropriate geometric calculations
        - A method called double() that doubles the width of the box. Hint: Pay attention to return value here
        - Implement __eq__ so that two boxes can be compared using == 
        - A method print_dim that prints to screen the length and width details of the box
        - A method get_dim that returns a tuple containg the length and width of the box
        - A method combine that takes another box as an argument and increases its length and width by the dimensions of the other box
        - A method get_hypot() that finds the length of the diagonal that cuts throught the middle
        In the function exercise01():
        - Instantiate 3 boxes of dimensions 5,10 , 3,4 and 5,10 and assign to variables box1, box2 and box3 respectively 
        - Print dimension info for each using print_dim()
        - Evaluate if box1 == box2, and also evaluate if box1 == box3, print True or False to the screen accordingly
        - Combine box3 into box1 (i.e. box1.combine())
        - Double the size of box2
        - Combine box2 into box1
        - Using a for loop, iterate through and print the tuple received from calling box2's get_dim()
        - Find the size of the diagonal of box2
    '''

    # ------ Place code below here \/ \/ \/ ------

    box1 = Box(5,10)
    box2 = Box(3,4)
    box3 = Box(5,10)

    box1.print_dim()
    box2.print_dim()
    box3.print_dim()

    print(box1 == box2)
    print(box1 == box3)

    box1.combine(box3)
    box2.double()
    box1.combine(box2)

    for d in box2.get_dim():
        print(d)

    print(box2.get_hypot())

    # ------ Place code above here /\ /\ /\ ------
    return box1, box2, box3


class TestAssignment3(unittest.TestCase):
    '''
    def test_exercise01(self):
        print('Testing exercise 1')
        b1, b2, b3 = exercise01()
        self.assertEqual(b1.get_length(),13)
        self.assertEqual(b1.get_width(),28)
        self.assertTrue(b1==Box(13,28))
        self.assertEqual(b2.get_length(),3)
        self.assertEqual(b2.get_width(),8)
        self.assertEqual(b3.get_length(),5)
        self.assertEqual(b2.get_hypot(),8.54400374531753)
        self.assertEqual(b1.double().get_length(),13)
        self.assertEqual(b1.double().get_width(),112)
        self.assertTrue(3 in b2.get_dim())
        self.assertTrue(8 in b2.get_dim())
        self.assertTrue(b2.combine(Box(1,1))==Box(4,9))
    '''
    def test_exercise02(self):
        print('Testing exercise 2')
        db = MangoDB()
        self.assertEqual(db.get_collection_size('default0'),3)
        print(db.get_collection_names())
        self.assertEqual(len(db.get_collection_names()),1)
        self.assertTrue('collection-default0' in db.get_collection_names() )
        db.add_collection('temperatures')
        self.assertTrue('collection-temperatures' in db.get_collection_names() )
        self.assertEqual(len(db.get_collection_names()),2)
        db.update_collection('temperatures',{1:50})
        db.update_collection('temperatures',{2:100})
        self.assertEqual(db.get_collection_size('temperatures'),2)
        self.assertTrue(type(db.to_json('temperatures')) is str)
        self.assertEqual(db.to_json('temperatures'),'{"1": 50, "2": 100}')
        db.wipe()
        self.assertEqual(db.get_collection_size('default0'),3)
        self.assertEqual(len(db.get_collection_names()),1)
      
    def test_exercise03(self):
        print('Exercise 3 does not have unit tests')

    if __name__ == '__main__':
        unittest.main()
