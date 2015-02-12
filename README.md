# Yin-Yang: Building Deep DSLs in a Breeze! #

[![Join the chat at https://gitter.im/scala-yinyang/scala-yinyang](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala-yinyang/scala-yinyang?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
master: [![Build Status](https://travis-ci.org/scala-yinyang/scala-yinyang.png?branch=master)](https://travis-ci.org/scala-yinyang/scala-yinyang)

Yin-Yang is a library that makes it easy to use and develop deeply embedded DSLs. The DSL users will never see the complex artifacts of the deep embedding while in production they will get all their benefits. The DSL authors write the DSL in plain Scala and Yin-Yang will generate the deep embedding. All that DSL users need to do are the domain-specific optimizations.

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

## Support

If you encounter any issues with Yin-Yang please report them [here](https://github.com/vjovanov/yin-yang/issues).

## Development

If you have any questions about the project please ask them in the development [mailing list](https://groups.google.com/forum/#!forum/scala-yinyang).

### Project Structure
| Component             | Description                                                | Stability          |
|:---------             |:-----------                                                |:---------:         |
| `components/core`     |  The core transformers of Yin-Yang (regular release)       | Release(soon)      |
| `components/yin-yang` |  The main Yin-Yang transformer used for defining your DSLs | Release(soon)      |
| `components/paradise` |  provides annotations for virtualization                   | Experimental       |
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
