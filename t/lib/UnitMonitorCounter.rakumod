use OO::Monitors;

# The file-scope (`unit`) form of a keyword registered through another module's
# EXPORTHOW::DECLARE block. Real Raku accepts this for any class-like
# declarator, and `Terminal::ANSI` (the bundled `Log::Async` battery's
# dependency) is written exactly this way:
# `unit monitor Terminal::ANSI::Virtual;`.
unit monitor UnitMonitorCounter;

has Int $.start = 0;
has $!count = 0;

method inc() { $!count++ }
method bump-twice() { self.inc; self.inc }   # reentrant: must not self-deadlock
method current() { $!start + $!count }
