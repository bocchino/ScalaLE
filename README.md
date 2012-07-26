ScalaLE
=======

This is the repository for the research project Scala with Logical
Effects.  To use the project, do the following:

1. Clone the repository ScalaLE-Compiler:
    `git clone git://github.com/bocchino/ScalaLE-Compiler.git ScalaLE-Compiler`
2. Clone this repository:
    `git clone git@github.com:bocchino/ScalaLE.git ScalaLE`

3. Set the following environment variables:

>`setenv SCALA_LE /path/to/ScalaLE`
>`setenv SCALA_LE_COMPILER /path/to/ScalaLE-Compiler`
>`setenv PATH ${SCALA_LE}/Implementation/bin:${PATH}`

4. Build the ScalaLE compiler:

>`cd ${SCALA_LE_COMPILER}`
>`ant`

5. Build the ScalaLE runtime:

>`cd ${SCALA_LE}/Implementation/Runtime`
>`make`

Now you are ready to go.  The ScalaLE compiler implementation is
called `slec`.  It is a shell script located at
`${SCALA_LE}/Implementation/bin/slec` that adds the ScalaLE runtime
classes to the classpath and invokes the compiler located at
`${SCALA_LE_COMPILER}/build/quick/bin/scalac`.

So far the compiler does not do much; it just parses the annotations
provided by the runtime.  The annotations are defined in

>`${SCALA_LE}/Runtime/sle/sle/Annotations.scala`.

Examples of how the annotations are used can be found in

>`${SCALA_LE}/Runtime/sle/sle/containers/*.scala`
>`${SCALA_LE}/Programs/DisjointArrayClient/sle/*.scala`

More examples, and further checking, will be coming soon.
