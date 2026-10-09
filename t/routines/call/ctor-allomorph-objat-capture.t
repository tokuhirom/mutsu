use v6;
use Test;

# The basic-type constructors share one table (native_ctor) between the
# interpreter's `.new` and the VM's native fast path.
plan 14;

is Int.new(5).raku, '5', 'Int.new(5)';
is Num.new(2.5).raku, '2.5e0', 'Num.new(2.5)';
is Str.new.raku, '""', 'Str.new is the empty string';
is IntStr.new(42, "x").raku, 'IntStr.new(42, "x")', 'IntStr.new';
is RatStr.new(1.5, "y").Str, 'y', 'RatStr.new keeps the string';
is ObjAt.new("foo").raku, 'ObjAt.new("foo")', 'ObjAt.new';
is ValueObjAt.new("bar").WHICH, 'ValueObjAt|bar', 'ValueObjAt.new';
is Capture.new(list => (1, 2), hash => {a => 1}).raku, '\(1, 2, :a(1))', 'Capture.new :list :hash';
throws-like { Capture.new(1) }, Exception,
    message => /'only takes named arguments'/, 'Capture.new with a positional dies';
my $f = Failure.new("boom");
is $f.handled, False, 'Failure.new starts unhandled';
$f.handled = True;
is $f.handled, True, 'Failure.handled can be set';
is Buf.new(1, 2).raku, 'Buf.new(1,2)', 'Buf.new';
is utf8.new(65).raku, 'utf8.new(65)', 'utf8.new';
is Buf[uint16].new(300).raku, 'Buf[uint16].new(300)', 'Buf[uint16].new';
