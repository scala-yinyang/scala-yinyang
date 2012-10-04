# MPDE - Meta programming based library for deep embedding of DSLs

MPDE is a library that provides the infrastructure for easy development of deeply embedded DSLs.

The problem with deeply embedded DSLs is that types in the DSL are usually fairly complex and corresponding type errors can be very hard to debug. The main idea of this project is to use a regular scala interface (shallow embedding) for the DSL and automatically convert it into the deep embedding by using Scala macros. 

Currently the project works only with typed macros and it is in early stages of development. Any ideas, desires, pull requests and bug reports are very welcome. We dicourage you to use it in production environment.
