use v6;
use Test;

# An indirect method call `$obj.$name` where `$name` is a TYPE OBJECT
# dispatches the method named by the type's short name: `$string.$type` with
# `$type = Int` calls `.Int`. mutsu previously stringified the type object to
# its gist (`(Int)`) and threw "No such method '(Int)'".
# From raku-doc Language/signatures.rakudoc (doc-diff finding [11]).

plan 15;

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

# Only the quoted `.""` operator dispatches a method by a string name. An
# unquoted `.$name` invokes the name value as a Callable with the receiver as
# its first argument.
my $m = "uc";
throws-like { "abc".$m() }, X::Method::NotFound,
    method => 'CALL-ME', typename => 'Str',
    'unquoted Str name specifier must be Callable';

my $receiver = "abc";
throws-like { $receiver.$m() }, X::Method::NotFound,
    method => 'CALL-ME', typename => 'Str',
    'mutable receiver uses the same unquoted Callable semantics';

is "abc"."$m"(), "ABC", 'quoted dynamic name dispatches by string';

my $callable = -> $invocant, $suffix = "" { $invocant.uc ~ $suffix };
is "abc".$callable("!"), "ABC!", 'unquoted Callable receives invocant first';

class CallableName {
    method CALL-ME($invocant, $suffix) { $invocant.uc ~ $suffix }
}
my $callable-name = CallableName.new;
is "abc".$callable-name("?"), "ABC?", 'object with CALL-ME is a valid name specifier';

is "abc".?$callable(), "ABC", '.? invokes a valid Callable name';
is-deeply "abc".*$callable(), ("ABC",), '.* wraps a Callable result';
is-deeply "abc".+$callable(), ("ABC",), '.+ wraps a Callable result';
