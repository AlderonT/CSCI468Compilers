CONTENTS OF THIS FILE 
---------------------

*Introduction

*Running the project

*Team Contributers

*Troubleshooting


------------------------------------------------------------

INTRODUCTION 
------------

Contained in this directory is an ELF executable for Linux 
called "micro" this file is the commulative work on the 
project that we have completed.

As of Step 1, this file contains the Lexer which prints out 
each token in the form of: 
    TOKEN TYPE: [TYPE]
    VALUE: [TOKEN VALUE]
These values will be used by the parser in Step 2
to build an Abstract Syntax Tree.

As of Step 2, this files contains the Lexer and Parser which 
prints out Accepted, or Not Accepted based on whether the provided
file is accepted by the grammar or not. The Abstract Syntax Tree 
can be displayed by adding -d or --display as a first argument 
to the file.

As of Step 3, this the file will generate a symbol table for the given 
code if a symbol table can be generated. If one cannot be the approapriate 
error is thrown. If the code is unable to parse it will say the code was 
not accepted. The Abstract Syntax Tree can be displayed by adding -d or 
--display as a first argument to the file.

-----------------------------------------------------------------

RUNNING THE PROJECT
-------------------
*RECOMMENDED OS: Linux

*On Linux
    run the ELF executable micro by typing:
        ./micro [-h|--help] [-d|--display]  <file_name> 
    If this doesn't work please refer to the Troubleshooting 
    section below.

*On Windows
    run the executable micro by typing:
        ./micro.exe [-h|--help] [-d|--display]  <file_name> 
    If this doesn't work please refer to the Troubleshooting 
    section below.

*Other
    If this needs to be run on Windows or Mac, please let us know 
    and we will send the appropriate executable to you.

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

*Linux:
    Please Email the team at: 
        Allen.simpson@motionencoding.com;radovich.tysen@gmail.com
    with the issue with the program. If the issue 
    is code related and you are getting errors not 
    related to the output, please let us know. 
    If it is an issue with the output, please let 
    us know in the feedback on D2L where the 
    discrepancies where. 

Thank you! for reading the README!