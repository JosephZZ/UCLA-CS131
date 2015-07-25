#python
import twisted
import math

#running file from interpreter
#execfile('filename')

print 'hello world'

#values and printing
#python accepts both ' and " as a quote mark

x = 1000 + 2000

print "Mary's car"
print ("Mary's red car is worth %d dollars" % x)
print ("There is no place like %s, and no college like %s" % ('home','UCLA'))

#data types
#lists
l = [1,2,3,4]

#dictionary
y = {}
y['cs131'] = 3.9
y['cs32'] = 3.8
y['cs181'] = 3.5

print 'all grades are', y, 'cs32 grade is', y['cs32']

#tuple
x = (1,2,3)
print 'tuple: ', x, x[1]


#control flow statements
l = [11,12,13,14,15]

for i in l:
    print i,
    for j in range(0,2):
        print j,
    print " "

#while
while False:
    print 'nothing'


#if else
if 3 !=4:
    t = "foo"
elif 4 != 5:
    t = "elif"
else:
    t = "bar"
print t


#functions
#prodList([1,2,3]) return 1*2*3=6
def prodList(l):
    result = 1
    for i in l:
        if( type(i) == int):
            result *= i
    return result

print prodList([1,2,"str",3])

def fact(n):
    result = 1
    for i in range(1,n+1):
        result *= i
    return result

print fact(3), fact(4), fact(1)

#frequency([1,2,1,1,4,5,4]) returns {1:3, 2:1, 4:2, 5:1}

#dict is a dictionary
# if x in dict:
#     do something...


def frequency(l):
    mydict = {}
    for i in l:
        if i in mydict:
            mydict[i] += 1
        else:
            mydict[i] = 1
    return mydict

print frequency([1,2,1,1,4,5,4])


#in python functions can return different values
#can return no value

def inverse(x):
    if( x== 0 ):
        print "Oops!"
        return #return None
    if( x== 1 ):
        return "it's the same!"
    print "the answer is:"
    return 1./x

#anonymous functions
print "lambda", (lambda x: x*x ) (3)

#python can accept functions as arguments
def applyTwice(f,x):
    return f(f(x))

print applyTwice( (lambda x:x+x), 11 )  #44 -- 11+11 then 22+22

#Classes
class BaseClass(object):
    #__init__ is the constructor of BaseClass, every class must have this!
    def __init__(self,argx):
        self.x = argx
    #every function we define has the self argument
    def getX(self):
        return self.x
    def setX(self,x):
        self.x = x

#class members are accessible
foo = BaseClass(3)
print 'foo.x ', foo.x

#we can also add memebers to the instance


"""
multiple line comment

Define a class which has at least two methods:
getString: to get a string from console input, hints: use raw_input() for console input
printString: print the string in upper case, hints: use str.upper() for upper case 

use __init__ to construct some parametrs

"""

class IntputOutString(object):
    def __init__(self):
        self.str = ""
    def getString(self):
        self.str = raw_input()
    def printString(self):
        print self.str.upper()



#list slicing and tricks
#a = [1,2,3,4,5,6,7,8]
#a[start:end:step]

a = [1,2,3,4,5,6,7,8]
print 'a:', a[1:5:2] #[2,4]


#reverse using slicing
print a[::-1]

#slice replacement
a[1:8:3] = [-1,-2,-3]
print a


#checks if w is a palindrome
#return True is it is, otherwise False
#hint: access last elemenet of list using w[-1]
def palindrome(w):
    if len(w) <= 0:
        return True
    return w == w[::-1]



"""
job interview question!
write binary search
bin_search looks for an item in a sorted list. the function should
return the index of the element
"""
def bin_search(li,element):
    return None #dummy implementation




"""
Another job interview question!
Given a set intervals (-1,2) (0,1) (3,8) (5,6)
write a function mergeIntervals that merges invervals that are 
overlapping or contained with each other
e.g. (1,5)(3,6) --> (1,6) 
e.g. (1,5)(1,6) --> (1,6)
Both endpoints are included in the interval
intervals is a list of tuples

hint: (not needed but will be nice)
use reduce -- similar to fold_left \ fold_right in Ocaml
"""

def mergeIntervals( intervals ):
    return None




