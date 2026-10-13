use Test;

# A role attribute default resolves names the composing class's scope
# lacks through the role's own declaring scope, which is consulted as a
# lookup tier rather than copied into the env per construction (#13018).

plan 9;

my $d;
{
    my $h = 5;
    my class Hidden { method name { 'hidden' } }
    role R2 { has $.h = $h + 1; has $.k = Hidden; has &.f = -> { $h * 2 } }
    $d = class D does R2 {}
}

my $o = $d.new;
is $o.h, 6, 'a role-scope lexical the constructing scope lacks';
is $o.k.name, 'hidden', 'a role-scope lexical class';
is $o.f.(), 10, 'a closure built by the default keeps the role scope';

sub construct-in-routine() { $d.new.h }
is construct-in-routine(), 6, 'constructed from inside a routine frame';

is $d.new(:h(1)).h, 1, 'an explicit argument still wins over the default';

# The constructing scope's own names stay authoritative.
my $outer = 'outer';
role R { has $.o = $outer ~ '!'; has Int $.t = Int }
class C does R {}
is C.new.o, 'outer!', 'a name both scopes see';
is C.new.t.^name, 'Int', 'a type-object default';

# Nothing the evaluation borrows from the role scope leaks into the caller.
my $leaked = do {
    my $x = $d.new;
    (try EVAL q[$h]) // 'not visible';
};
is $leaked, 'not visible', 'the role scope does not leak into the caller';

my @made = (^20).map({ $d.new.h });
is-deeply @made, [6 xx 20], 'repeated construction is stable';
