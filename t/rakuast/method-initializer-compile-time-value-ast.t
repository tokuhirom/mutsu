use Test;
use experimental :rakuast;

plan 4;

# An attribute default's `Method::Initializer` thunk answers
# `.compile-time-value` with the anonymous method it stands for, as static's
# analyzer asks of every routine node (#18814).

my @found;
sub walk($n) {
    @found.push($n) if $n ~~ RakuAST::Method::Initializer;
    $n.visit-children(&walk);
}
walk(Q{class C { has Int $.x = 42 }}.AST);

is @found.elems, 1, 'the attribute default holds one Method::Initializer';
my $method = @found[0].compile-time-value;
isa-ok $method, Method, 'its compile-time-value is a Method';
is $method.name, '', 'an anonymous one';
ok @found[0].compile-time-value === $method, 'the same object on every call';
