# Mu.say, print, put and note are base rows

A user method say/print/put/note that calls callsame now reaches Mu's default, which writes the receiver's gist or Str (ADR-11276 §9.57); it used to print nothing.
