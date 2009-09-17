# Current Status #

BROKEN - In the middle of a refactor.

# Note #

This is currently a work in progress, only the encode stuff works, and
it is undergoing lots of revision to make it fit for public
consumption.

The rest of this file should be considered a guide for how it should
work in the future.

# Installing #

There are two ways to install bencode:

* Use `ant` to compile to a JAR:
  * To package .class files and .clj sources use: `ant -Dclojure.jar=/path/to/clojure.jar`
  * To package only .clj sources use: `ant`
  * Add `bencode.jar` to your classpath
* Just add the `src` directory to your classpath

# Using The Encoder #

TODO

# Custom Encoding #

TODO

# Using The Parser #

TODO

# Thanks #

Thanks to Dan Larkin, from whose clojure-json I stole all the
structure of this project, and shortly I'll steal ideas from the code.


