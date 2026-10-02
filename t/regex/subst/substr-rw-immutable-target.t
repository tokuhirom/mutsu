use Test;

# `.substr-rw(...) = v` needs a writable container behind its invocant:
# a name bound straight to a value (`:=`, sigilless, `constant`) or a
# readonly parameter has none, and Rakudo dies with "'substr-rw' requires
# a writeable container" instead of silently rewriting the binding.
# mutsu#10893.

plan 12;

my $k := "lit";
throws-like { $k.substr-rw(0, 1) = "x" }, X::AdHoc,
    message => "'substr-rw' requires a writeable container",
    'a `:=`-bound literal is refused';
is $k, 'lit', 'and the binding is unchanged';

my \sl = "lit";
throws-like { sl.substr-rw(0, 1) = "x" }, X::AdHoc,
    message => "'substr-rw' requires a writeable container",
    'a sigilless binding is refused';
is sl, 'lit', 'and keeps its value';

constant $c = "abc";
throws-like { $c.substr-rw(0, 1) = "x" }, X::AdHoc,
    message => "'substr-rw' requires a writeable container",
    'a sigiled constant is refused';

sub ro($p) { $p.substr-rw(0, 1) = "x" }
my $s = "abc";
throws-like { ro($s) }, X::AdHoc,
    message => "'substr-rw' requires a writeable container",
    'a readonly parameter is refused';
is $s, 'abc', 'and the caller is unchanged';

my $b := "lit";
throws-like { substr-rw($b, 0, 1) = "x" }, X::AdHoc,
    message => "'substr-rw' requires a writeable container",
    'the sub form refuses a `:=`-bound literal too';

# The writable shapes keep working.
my $w = "abc";
for $w { .substr-rw(0, 1) = "x" }
is $w, 'xbc', 'a `for` topic aliased to a variable is writable';

sub raw(\p) { p.substr-rw(0, 1) = "y" }
my $r = "abc";
raw($r);
is $r, 'ybc', 'a sigilless parameter bound to a variable is writable';

my $t = "abc";
my $u := $t;
$u.substr-rw(0, 1) = "z";
is $t, 'zbc', 'a `:=` alias of a variable is writable';

sub rw($p is rw) { $p.substr-rw(0, 1) = "q" }
my $q = "abc";
rw($q);
is $q, 'qbc', 'an `is rw` parameter is writable';
