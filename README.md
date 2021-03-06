# RandomInputGenerator version 2.2

Developed by [Anish 'basso' George](www.blog.anishgeorge.com)

   This program generates random dataset for testing algorithms
and applications. It has the ability to create Data in a variety
of formats depending on your requirements. 

### Deployment

I have provided two jars for use, (in the target folder)

1. randGen.jar is the actual application, since this tool is
    developed using Scala language, you need to have scala installed
    on your system to run this .jar file (by using the 'scala' interpretter)
    or you need the scala library jar in your class path for use with 
    java

2. randGenScala.jar is for those who don't have scala, this is a 
    make-shift jar including the core scala library as well so will be
    a bit heavier than the pure application but on the other hand, you
    simply use this with your JRE

### Typical usage :

(Assuming the standard randGenScala.jar deployment)

1. Creating a dataset of 50 randomly generated Integers in range
[-30,150] on a file named data.txt 

java -jar randGenScala.jar -I -n 50 --min=-30 --max=150 -f data.txt

2. Creating a dataset of 100 Strings of length 20 each, including
alphanumerics and special characters and outputting this data to
screen 

java -jar randGenScala.jar -S -n 100 --fix=20

3. Creating a dataset of 100 Decimal numbers with max precision
of 4 digits after decimal point and within range [0,20.5] and
making 2 files of such data (file1.txt and file2.txt) 

java -jar randGenScala.jar -D -n 100 --min=0 --max=20.5 -f file1.txt
file2.txt

4. Creating a dataset of 100 randomly selected characters, all
capitals java -jar randGenScala.jar -C -n 100 --allcaps


## Complete Usage Documentation


#### SYNOPSIS 

java -jar randGenScala.jar [-I|-D|-C|-S] [Options] [-f file1 file2.. ]

##### DATA MODES: 
-I : Integer 
-D : Decimal, Double precision floating point data 
-C : Character 
-S : String

##### STYLE OPTION: 
--General : Default, simple syle, put words onto file 

--CodeJam : Style following input data style of Google Code
            Jam, where first line contains 'Total number' of lines, 
            each line preceded by a line telling 'number' of words 
            on the line

##### Major Options 
-n : followed by the total # of words e.g. -n 40

-f : Should be the last option, followed by a list of output
      filenames e.g. -f data1.txt temp_data.txt 
      
Note: 1. Each seperate file will recieve a new randomly generated data
      set 2. If -f flag is not present, data will be outputted 
      to the console

-l : # of words in a single line. In case of --CodeJam option,
      this will mean the MAX # of words on a line e.g. -l 3

-m : Available only in --CodeJam mode, Minimum # of words on a
      line e.g. -m 2

##### Data Specific Options 
######INTEGER: 
--max=<n> : Upper bound of data,
               e.g. --max=32                    [Default -> 100] 

--min=<n> : Lower bound of data,
               e.g. --min=-10                   [Default -> 0]

###### DECIMAL: 
--max=<n> : Upper bound of data, 
               e.g. --max=100.3                 [Default -> 1.0] 

--min=<n> : Lower bound of data                 [Default -> 0.0] 

--prec=<n>: Precision, max # of digits after decimal point. 
               e.g. --prec=5                    [Default -> 3]

###### STRING: 
--max=<n> : Max size of string (in characters)  [Default -> 50] 

--min=<n> : Min size of string                  [Default -> 2] 

--prefix=<str>: Prefix to add to every string 

--suffix=<str>: Suffix to add to every string

###### Common options to String and Char modes 
--caps : Allow capitals

--allcaps : No smalls, overrides --caps 

--nums : Allow Numerics

--noalphas: NO alphabets [Defaults to adding spchars if no other
               option is provided] 

--spchars : Allow Special Characters

###### Update Log:

v 2.2: Fixed major bug; finalised the Generator package's API
