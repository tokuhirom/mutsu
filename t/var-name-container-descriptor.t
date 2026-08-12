use v6;
use Test;

# Rakudo's container-descriptor `.name` (`.VAR.name`) for @/% parameters:
# an unsupplied / literal-bound / slurpy / `is copy` param binds a FRESH
# anonymous container whose descriptor name is "element"; a param aliasing a
# caller's named container reports a non-"element" name (rakudo: the caller's
# name; mutsu may report the param's own name — both satisfy the contract).
# Text::CSV's `method CSV` gates its whole out/headers defaulting on exactly
# `@kh.VAR.name ne "element"` (its rakudo#2483 workaround), so getting the
# unsupplied case wrong silently rewrote csv()'s output mode (90_csv.t).

plan 14;

sub named(:@kh) { @kh.VAR.name }
is named(), "element", 'unsupplied :@kh binds an "element" container';
my @x = 1, 2;
isnt named(kh => @x), "element", 'supplied :@kh (from a variable) is not "element"';
is named(), "element", 'unsupplied again after a supplied call (no stale meta cache)';

sub named-default(:@kh = [9]) { @kh.VAR.name }
is named-default(), "element", 'unsupplied :@kh with a default is still "element"';

sub named-hash(:%o) { %o.VAR.name }
is named-hash(), "element", 'unsupplied :%o binds an "element" container';
my %y = a => 1;
isnt named-hash(o => %y), "element", 'supplied :%o is not "element"';
is named-hash(), "element", 'unsupplied :%o again after a supplied call';

sub positional(@a) { @a.VAR.name }
is positional(@x), '@x', 'positional @a bound from @x reports the caller name';
is positional([1, 2]), "element", 'positional @a bound from a literal is "element"';

sub positional-hash(%h) { %h.VAR.name }
is positional-hash(%y), '%y', 'positional %h bound from %y reports the caller name';

sub slurpy-arr(*@r) { @r.VAR.name }
is slurpy-arr(1, 2), "element", 'slurpy *@r is a fresh "element" container';

sub copied(@a is copy) { @a.VAR.name }
is copied(@x), "element", 'is copy binds a fresh "element" container';

my @z = 3, 4;
is @z.VAR.name, '@z', 'a plain my @z keeps its own name';
my %w = k => 1;
is %w.VAR.name, '%w', 'a plain my %w keeps its own name';

done-testing;
