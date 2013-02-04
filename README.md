# MPDE - Meta programming based library for deep embedding of DSLs

MPDE is a library that provides the infrastructure for easy development of deeply embedded DSLs.

The problem with deeply embedded DSLs is that types in the DSL are usually fairly complex and corresponding type errors can be very hard to debug. The main idea of this project is to use a regular scala interface (shallow embedding) for the DSL and automatically convert it into the deep embedding by using Scala macros. 

Currently the project works only with typed macros and it is in early stages of development. Any ideas, desires, pull requests and bug reports are very welcome. We dicourage you to use it in production environment.

## Development

To build this project you will need [SBT 0.12.x](http://www.scala-sbt.org/0.12.2/docs/home.html)

To use Eclipse in sbt type the command *eclipse* to create Eclipse project files:

    > eclipse
    
In Eclipse use the *Import Wizard* to import *Existing Projects into Workspace*
