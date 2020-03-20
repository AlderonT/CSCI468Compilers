CONTENTS OF THIS FILE 
---------------------

*Introduction

*Running the project

*Team Contributers

*Troubleshooting


------------------------------------------------------------

INTRODUCTION 
------------

Contained in this directory is an .exe called "SmallC.exe" 
this file is the commulative work on the project that we 
have completed.

As of Step 1, this file contains the Lexer which prints out 
each token in the form of: 
    TOKEN TYPE: [TYPE]
    VALUE: [TOKEN VALUE]
These values will be used by the parser in Step 2
to build an Abstract Syntax Tree.

As of Step 2, this files contains the Lexer and Parser which 
prints out Accepted, or Not Accepted based on weather the provided
file is accepted by the grammar or not. The Abstract Syntax Tree 
can be displayed by adding -d or --display as a second argument 
to the file.

-----------------------------------------------------------------

RUNNING THE PROJECT
-------------------
*RECOMMENDED OS: Windows 10

*On Windows
    Got to the command line and navigate to the  directory 
    containing this file. Then type:
        .\micro [file_name]
    This should run the program and everything *should* work. 
    If for some reason this does not work and it complains 
    that .\micro is not a command or a script then you will 
    have to run the file by typing the following:
        .\SmallC.exe [file_name]
    This will run the script directly (which is what .\mirco 
    does but encapsulates to make it prettier).
    If neither option works please refer to the 
    Troubleshooting section below.

*On Mac 
    Because Mac does not like .bat files you will have to run the 
    file directly by typing the following in the command line: 
        .\SmallC.exe [file_name]
    As mentioned above, this will run the file directly.
    If this option does not work Please refer to the 
    Troubleshooting section below.

*On Linux
    I am going to be honest, no-one on the team has a Linux machine 
    or any way to test this really simple script. However you 
    *should*, if StackOverflow hasn't lied to us, be able to 
    execute the following:
        ./microL.sh [file_name]
    If this does not work, please execute the file directly using:
        wine ./SmallC.exe [file_name]
    You will need to install wine in order to make this work 
    as the program SmallC.exe has been compiled into win10-x64
    and so is a windows program requiring the wine plugin to run 
    If this doesn't work please refer to the Troubleshooting 
    section below

-----------------------------------------------------------------

TEAM CONTRIBUTERS 
-----------------
// *[CONTRIBUTER]
//     -[WORKED ON THIS] 

*Allen Simpson 
    -Parserbuilder
    -Parser commands
    -Lexer

*Tysen Radovich
    -Paper
    -Lexer

-----------------------------------------------------------------

TROUBLESHOOTING
---------------

*Windows:
    Please Email the team at: 
        Allen.simpson@motionencoding.com;radovich.tysen@gmail.com
    with the issue with the program. If the issue 
    is code related and you are getting errors not 
    related to the output, please let us know. 
    If it is an issue with the output, please let 
    us know in the feedback on D2L where the 
    discrepancies where. 

*Mac/Linux: 
    If you are receiving errors on Mac/Linux, please 
    try it on Windows. We have no way to test our code 
    on Mac/Linux or confirm that things work perfectly. 
    If you are using a Mac/Linux to run our code and the 
    result's don't match what they should in any way 
    please let us know what you are using by email at:  
        Allen.simpson@motionencoding.com;radovich.tysen@gmail.com 

Thank you! for reading the README!