use Test;

# `f(...) = v` for an rw-capable routine writes through the container the
# routine hands back. When the routine hands back a *value* — a `\raw`
# parameter that bound an rvalue, a readonly `$x` parameter, a literal tail —
# there is no container, and the assignment must be refused with
# X::Assignment::RO instead of silently landing in a cell nobody shares.

plan 22;

sub raw-tail(\x) is raw { x }
sub rw-tail(\x)  is rw  { x }
sub ro-param($x) is rw  { $x }
sub plain(\x)           { x }
sub literal-tail()  is raw { 42 }
sub own-lexical()   is rw  { my $y = 1; $y }

# --- a raw parameter that bound an rvalue is NOT writable ------------------
dies-ok { raw-tail(42) = 9 },      'f(42) = 9 through an is-raw sub dies';
dies-ok { raw-tail("s") = 9 },     'a Str literal argument dies too';
dies-ok { rw-tail(42) = 9 },       'the is-rw spelling dies the same way';
dies-ok { literal-tail() = 9 },    'a literal tail dies';
my $one = 1;
dies-ok { raw-tail($one + 1) = 9 }, 'a computed argument dies';

# The refusal names the immutable value, as Rakudo does.
my $msg = 'no exception';
{ raw-tail(42) = 9; CATCH { default { $msg = .message } } }
is $msg, 'Cannot modify an immutable Int (42)', 'the refusal names the value';

# --- a raw parameter that bound a container IS writable --------------------
my $v = 42;
lives-ok { raw-tail($v) = 9 }, 'f($v) = 9 writes through the caller container';
is $v, 9, 'the caller variable actually changed';

my @arr = 1, 2, 3;
lives-ok { raw-tail(@arr) = (7, 8) }, 'an Array argument is a container';
is-deeply @arr, [7, 8], 'the assignment list-assigns into the caller array';

my %h = a => 1;
lives-ok { raw-tail(%h) = (b => 2) }, 'a Hash argument is a container';
is-deeply %h, {b => 2}, 'the assignment replaces the caller hash contents';

# --- a readonly parameter is never writable, container or not -------------
dies-ok { ro-param(42) = 9 },  'a readonly $x parameter with a literal dies';
my $w = 42;
dies-ok { ro-param($w) = 9 },  'a readonly $x parameter with a variable dies';
is $w, 42, 'and the caller variable is untouched';

# --- a non-rw routine is refused before any of this ------------------------
dies-ok { plain(42) = 9 }, 'a routine that is not rw-capable is refused';

# --- the routine's OWN lexical is still a container ------------------------
lives-ok { own-lexical() = 9 }, 'an is-rw routine may hand back its own lexical';

# --- ++ through the same path -------------------------------------------
dies-ok { ++raw-tail(42) }, 'prefix ++ on a value result dies';
my $inc = 5;
lives-ok { ++raw-tail($inc) }, 'prefix ++ through a container works';
is $inc, 6, 'and it incremented the caller variable';

# --- binding the result is not an assignment and must still work ----------
my $bound := raw-tail(42);
is $bound, 42, 'binding an immutable raw result still works';
my $src = 3;
my $alias := raw-tail($src);
$alias = 11;
is $src, 11, 'binding a container raw result still aliases';

# vim: ft=raku
