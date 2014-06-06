# Yin-Yang: Building Deep DSLs with a Breeze! [![Build Status](https://travis-ci.org/vjovanov/yin-yang.png?branch=master)](https://travis-ci.org/vjovanov/yin-yang)#

Yin-Yang is a macro based library that provides the infrastructure for easy development of deeply embedded DSLs.

Deep embedding of DSLs requires complex types, disallows debugging, and imposes long compilation times. Yin-Yang uses a regular Scala interface (direct embedding) for the DSL and automatically converts it into the deep embedding by using Scala macros.

For more information [www.yin-yang.org](www.yin-yang.org).

## Support

If you encounter any issues with Yin-Yang please report them [here](https://github.com/vjovanov/yin-yang/issues).

## Development

If you have any questions about the project please ask them in the development [mailing list](todo).

### Project Structure
| Component             | Description                                                | Stability          |
|:---------             |:-----------                                                |:---------:         |
| `components/core`     |  The core transformers of Yin-Yang (regular release)       | Release            |
| `components/yin-yang` |  The main Yin-Yang transformer used for defining your DSLs | Release            |
| `components/paradise` |  provides annotations for virtualization                   | Experimental       |
| `components/generator`|  Generates the LMS based deep embedding from Scala classes | Experimental       |
| `components/dsls`     |  Small DSLs used for experimentation.                      | Experimental       |

### Building the Project

To build Yin-Yang you will need [SBT 0.13.x](http://www.scala-sbt.org/0.13/tutorial/index.html). Just invoke:

    sbt package

To use Eclipse invoke:

    sbt eclipse

In Eclipse use the *Import Wizard* to import *Existing Projects into Workspace*

**Note:** Eclipse does not support long running macros. The test cases that reify DSLs at compile time will be practically impossible to navigate in Eclipse.

## License

Yin-Yang is licensted under the [Scala Licence](https://raw.githubusercontent.com/vjovanov/yin-yang/master/LICENCE).
