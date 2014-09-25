# ZMPP2 - Second Generation Interactive Fiction Interpreter for Java and Android

ZMPP2 is a Z-Machine and Glulx/Glk implementation in Scala. The goal is to write a simple, efficient, flexible and reusable solution that works in many kinds of Java VM environments, such as Java SE, Android and Java EE.

The second revision of ZMPP reflects current knowledge and ideas about how I think an IF VM should look like on the Java Virtual Machine:

- Thin abstraction 
- Provide a simple, easily integratable core
- Suspend/resume execution model for maximum flexibility
- I/O responsibilities shared between core and UI
- do as much work in the core as possible
- externalizable, serializable machine state
- working on primitive values in the VM core. Yes, Java SE is
  fast, but other environments e.g. have more expensive costs for
  object allocation
- single threaded core: The VM/Glk core implementation assumes that
  it runs within a single thread. Whether the user interface employs
  multiple threads is left to the specific UI implementation.

###Build Instructions:

The project is built with sbt 0.13.1, it is configured for Scala 2.11.0
Simply type

	sbt clean test

to build all the subprojects and run the unit tests. To build e.g. the Z-Code interpreter
start sbt and type

	project zmpp-zcode

Typing
	run <path-to-z-code-game>

will run the desktop interpreter from within sbt, while

	assembly

will build an executable jar file with all dependencies.


###Current features:

- Glulx 3.1.1, Glk 0.7.0 almost feature complete (see known issues)
- Z-Machine Standard 1.1
- run as applet or application (Java SE)
- runs a number of games already
- saves games in Quetzal format
- supports Blorb format

<b>Glulx/Swing:</b>

- supports graphics
- multiple undo
- styles
- timer events
- file access
- complete sound support (Ogg Vorbis, MOD, AIFF through JavaSound SPIs)

<b>Z-Machine:</b>

- supports all versions except V6
- multiple undo
- colors
- accented characters
- user-defined alphabets, accent tables, user dictionaries
- timed input

###Known issues:

- Glk support incomplete
- Z-Machine
  - V6 support incomplete
  - sound support

##Acknowledgements:

- The IF community for actively advancing Interactive Fiction and teaching
  me about it through your discussions. I would not be working on this
  project without you.
- Eric Forgeot and Fredrik Ramsberg for providing tons of advice and
  teaching me how internationalization works in Inform and Z-Machine
- Ruben Ortega for showing me his Kindle version of ZMPP, which inspired
  me to start a Glulx core within ZMPP
- Carlos Sanchez for lots of testing and advice for Superglus support
- Eliuk Blau for the support to implement the sound feature
- Andrew Plotkin for providing lots of unit tests and clarifications
  about the specs
