# Yin-Yang: Transparent Embedding of Deep DSLs [![Build Status](https://travis-ci.org/vjovanov/yin-yang.png?branch=master)](https://travis-ci.org/vjovanov/yin-yang)#

Yin-Yang is a macro library that provides the infrastructure for easy development of deeply embedded DSLs.

Deep embedding of DSLs requires complex types, disallows debugging, and imposes long compilation times. Yin-Yang uses a regular Scala interface (shallow embedding) for the DSL and automatically convert it into the deep embedding by using Scala macros. 

The project is still in early development. Any ideas, desires, pull requests and bug reports are very welcome. We dicourage you to use it in production environment.

## Development

To build this project you will need [SBT 0.12.x](http://www.scala-sbt.org/0.12.2/docs/home.html)

To use Eclipse in sbt type the command *eclipse* to create Eclipse project files:

    > eclipse
    
In Eclipse use the *Import Wizard* to import *Existing Projects into Workspace*
