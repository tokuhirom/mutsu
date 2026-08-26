unit class Terminal::ANSI::State;

#| Keep track of some aspects of the state of the screen.

has Int $.scroll-top is rw = -1;
has Int $.scroll-bottom is rw = -1;

