use lib 't/lib';
use Test;

# `unit <kw> Name;` — the file-scope form of a declarator keyword registered
# through a module's EXPORTHOW::DECLARE block. Only `unit class`/`unit role`/
# `unit grammar` used to reach the rest-of-unit body capture, so a registered
# keyword in that position fell through to the expression parser and died with
# "Unknown function: <kw>". `Terminal::ANSI::Virtual.rakumod` (the bundled
# `Log::Async` battery's dependency) is written as
# `unit monitor Terminal::ANSI::Virtual;`, which is what surfaced this.

plan 5;

use UnitMonitorCounter;

my $c = UnitMonitorCounter.new;
is $c.current, 0, 'a `unit monitor` module declares a usable type';

$c.bump-twice;
is $c.current, 2,
    'methods written after the `unit` line belong to the declared monitor '
    ~ '(and its lock is reentrant)';

my $offset = UnitMonitorCounter.new(start => 10);
is $offset.current, 10,
    'an attribute declared after the `unit` line takes named construction';

await do for ^4 { start { $c.inc for ^250 } }
is $c.current, 1002, 'the monitor still serializes concurrent method calls';

# The block form of the same declarator keeps working in the importing unit.
use OO::Monitors;
monitor Blocky { method answer() { 42 } }
is Blocky.new.answer, 42, 'the block form is unaffected';
