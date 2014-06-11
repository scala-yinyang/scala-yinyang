# Yin-Yang: Building Deep DSLs with a Breeze! #
master: [![Build Status](https://travis-ci.org/vjovanov/yin-yang.png?branch=master)](https://travis-ci.org/vjovanov/yin-yang)

Yin-Yang is a library that makes it easy use and develop deeply embedded DSLs. The DSL users will never see the complex artifacts of the deep embedding while in production they will get all their benefits. The DSL authors write the DSL in plain Scala and Yin-Yang will generate the deep embedding. All that needs is left to be done is the optimizations.

**DSL Users** will never know that the DSL is deeply embedded as they can debug and prototype their code. For example:

    vectorDSL {
    	Collection(1,2,3) map (_ + 1)
    }


**DSL Authors** will develop their interface in regular Scala:

		object Collection {
			def apply[T](els:T*) = ...
	  }
    class Collection[T](elements: Seq[T]) {
    	def map[U](f: T => U) = ...
    }

And Yin-Yang will generate the nasty `Rep[_]` types, IR nodes, and cakes. 

## Translation

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

Yin-Yang is licensed under the [Scala License](https://raw.githubusercontent.com/vjovanov/yin-yang/master/LICENCE).
