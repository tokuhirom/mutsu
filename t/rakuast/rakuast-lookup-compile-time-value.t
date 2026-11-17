use experimental :rakuast;
use Test;

# A lookup's compile-time value is its resolution's (#14916), measured on
# rakudo 2026.09: a type name (`Type::Simple.compile-time-value` and
# `maybe-compile-time-value`) is the type it names; a lexical variable's
# `maybe-compile-time-value` is its container's type object (`Int` for
# `my Int $x`, `Any` untyped), and `&say` is the routine. A node built by hand
# is not resolved and dies.

plan 16;

my %seen;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Type::Simple {
            %seen{'type ' ~ $n.name.canonicalize} =
                [$n.compile-time-value, $n.maybe-compile-time-value];
        }
        if $n ~~ RakuAST::Var::Lexical && $n.name eq '$x' | '$y' | '$p' | '$_' | '&say' {
            %seen{'var ' ~ $n.name} = [$n.maybe-compile-time-value, $n.resolution.^name];
        }
        $n.visit-children(&walk);
    }
    walk($*CU.statement-list);
}

class Foo { }
my Int $x = 1;
my $y = Foo;
sub f($p) { $p }
my @probe = Str, $x, $y, &say, $_, f(1);

ok %seen{'type Str'}[0] === Str, 'a core type name is the type object (compile-time-value)';
ok %seen{'type Str'}[1] === Str, 'maybe-compile-time-value answers it too';
ok %seen{'type Foo'}[0] === Foo, "a unit's class name is the class";
ok %seen{'type Int'}[0] === Int, "a variable's declared type is the type object";
ok %seen{'var $x'}[0] === Int, 'a typed variable is its container type';
ok %seen{'var $y'}[0] === Any, 'an untyped variable is Any';
ok %seen{'var $p'}[0] === Any, 'a parameter is Any';
ok %seen{'var $_'}[0] === Any, 'the topic is Any';
is %seen{'var &say'}[1], 'RakuAST::Declaration::External::Setting',
    '&say resolves to the setting';
ok %seen{'var &say'}[0] ~~ Callable, '&say is the routine';
is %seen{'var &say'}[0].name, 'say', 'the routine is say';

my $hand = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int'));
throws-like { $hand.compile-time-value }, Exception,
    message => /"'Int' has not been resolved."/, 'a hand-built type name is unresolved';
throws-like { $hand.maybe-compile-time-value }, Exception,
    message => /"'Int' has not been resolved."/, 'so is its maybe-compile-time-value';
throws-like { RakuAST::Var::Lexical.new('$v').maybe-compile-time-value }, Exception,
    message => /'This element has not been resolved.'/, 'a hand-built variable is unresolved';
is RakuAST::IntLiteral.new(3).maybe-compile-time-value, 3, 'a literal still answers its value';
is RakuAST::StrLiteral.new('x').maybe-compile-time-value, 'x', 'a string literal too';
