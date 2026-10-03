use Test;

# `my class T { ... }.new(...)` as a routine's last statement: the `.new` is a
# postfix on the type object, not a new `$_.new` statement -- also when the
# class body uses an attribute twigil. From Badger's test helper
# `make-query-class`.

plan 4;

sub mk(Any:U $rc) {
    my class T {
        has @.last-params;
        has $.rc;
        method query(::?CLASS:D: $, *@!last-params) { $.rc.new }
    }.new(:$rc)
}
my $r = mk(class X0 { method rows { 3 } });
is $r.^name, 'T', 'the routine returns the instance';
is $r.query(1, 2, 3).rows, 3, 'its method runs';
is-deeply $r.last-params, [2, 3], 'the attribute parameter bound';

my $x = do { my class U { has $.l; method q($!l) { 1 } }.new(l => 7) };
is $x.l, 7, 'in a do block';
