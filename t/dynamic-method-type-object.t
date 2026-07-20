use v6;
use Test;

# An indirect method call `$obj.$name` where `$name` is a TYPE OBJECT
# dispatches the method named by the type's short name: `$string.$type` with
# `$type = Int` calls `.Int`. mutsu previously stringified the type object to
# its gist (`(Int)`) and threw "No such method '(Int)'".
# From raku-doc Language/signatures.rakudoc (doc-diff finding [11]).

plan 8;

sub can-turn-into(Str $string, Any:U $type) {
    so $string.$type
}
ok  can-turn-into("3",        Int), '"3".$Int is True';
ok  can-turn-into("6.5",      Int), '"6.5".$Int is True';
ok  can-turn-into("6.5",      Num), '"6.5".$Num is True';
nok can-turn-into("a string", Num), '"a string".$Num is False';

# The type object's method is really invoked (not just truthiness).
my $t = Int;
is "6.5".$t, 6, 'type object $Int calls .Int (6.5.Int == 6)';

my $s = Str;
is "6.5".$s, "6.5", 'type object $Str calls .Str';

# A user-defined class type object resolves the method named by the class
# (here `.Widget`), rather than the type object's gist `(Widget)`.
class Widget { }
my $w = Widget;
throws-like { 42.$w }, X::Method::NotFound,
    'user class type object looks up the class-named method (.Widget)';

# A plain Str name specifier still names a method (mutsu extension, unchanged).
my $m = "uc";
is "abc".$m, "ABC", 'Str name specifier still dispatches by method name';
