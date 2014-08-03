# chess_clojure

Using chess_clojure with chess graphical user interfaces:

Create an executable with

lein uberjar

I created a shell script called chess_engine.sh with the folowing contents and set it as
executable and placed it in /usr/games

java -jar /home/john/chess_clojure/target/chess_clojure-0.1.0-SNAPSHOT-standalone.jar

Then in the various chess programs that support xboard, point them to the script. 

eboard: Worked. A permissive engine.
knights: Crashing sometimes. ( due to illegal move ??).
  Supports computer vs computer
xboard: No
scid: No
pychess: Failed. Tried adding engine to engines.xml:  
vi /home/john/.config/pychess/engines.xml
dreamchess: Works OK.
glchess: Aborted after 3 moves 
***** My recommendation: Use eboard on Linux or knights if it doesn't crash

FIXME: description

## Installation

Download from http://example.com/FIXME.

## Usage

FIXME: explanation

    $ java -jar chess_clojure-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
