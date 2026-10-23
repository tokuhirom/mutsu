use Test;

plan 8;

# An `is rw` / `is raw` / sigilless parameter is the caller's container, whose
# own constraint applies to a store through it. A *caller's* same-named
# parameter, further up the call chain, must not constrain the store.
sub clip-to($min, $v is rw, $max) { $v = ($min max $v) min $max }
sub hsl(@ ($h, $s is copy)) { clip-to 0, $s, 1; $s }
my subset Tagged of Pair where { .key ~~ 'hsl' };
multi sub from-ast(Tagged $v) { hsl((0, 1.5)) }

is from-ast((hsl => 1)), 1, 'a caller\'s `Tagged $v` does not constrain `$v is rw`';

sub store-rat($v is rw) { $v = 2.5 }
sub copy-and-store($s is copy) { store-rat($s); $s }
sub by-pair(Pair $v) { copy-and-store(1.5) }
is by-pair((a => 1)), 2.5, 'the same through a plain `Pair $v` caller';
sub by-int(Int $v) { copy-and-store(1.5) }
is by-int(1), 2.5, 'the same through an `Int $v` caller';

sub store-raw($v is raw) { $v = 2.5 }
sub copy-raw($s is copy) { store-raw($s); $s }
sub raw-caller(Pair $v) { copy-raw(1.5) }
is raw-caller((a => 1)), 2.5, 'an `is raw` parameter likewise';

sub store-sigilless(\v) { v = 2.5 }
sub copy-sigilless($s is copy) { store-sigilless($s); $s }
sub sigilless-caller(Pair $v) { copy-sigilless(1.5) }
is sigilless-caller((a => 1)), 2.5, 'a sigilless parameter likewise';

# The bound container's own constraint still applies.
sub assign-str($v is rw) { $v = "x" }
my Int $typed = 1;
throws-like { assign-str($typed) }, X::TypeCheck::Assignment, 'a typed caller variable still constrains the store';
sub assign-str-raw($v is raw) { $v = "x" }
throws-like { assign-str-raw($typed) }, X::TypeCheck::Assignment, '... through an `is raw` parameter';
my $untyped = 1;
assign-str($untyped);
is $untyped, 'x', 'an untyped caller variable takes any value';
