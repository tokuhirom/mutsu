use Test;

# #18267 stage 5: an omitted positional whose default is a shareable constant
# (`\()`, a literal) binds on the method fast binder, and a call that asks for
# a container (the right side of `:=`) takes the plain-method lane under a key
# of its own. Every call repeats, so the replayed calls are the ones checked.

plan 9;

class C {
    has $.v is rw = 1;
    method cap(Capture:D $params is copy = \()) { $params = \(|$params, :x); $params.raku }
    method n($a, $n = 5) { $n }
    method cp($v is copy = 3) { $v++; $v }
    method ro($r = 1) { $r = 2 }
    method raw-ret() is raw { $!v }
    method get(Mu $x is raw) is raw { $x }
}
my $c = C.new;

is-deeply (^3).map({ $c.cap }).List, ('\(:x)' xx 3).List,
    'an is copy Capture parameter defaulting to \() is its own container';
is-deeply (^3).map({ $c.cap(\(1)) }).List, ('\(1, :x)' xx 3).List,
    'the same parameter given an argument';
is-deeply (^3).map({ $c.n(1) }).List, (5, 5, 5), 'an omitted literal default';
is-deeply (^3).map({ $c.n(1, 2) }).List, (2, 2, 2), 'the same parameter supplied';
is-deeply (^3).map({ $c.cp }).List, (4, 4, 4), 'an is copy literal default is writable and fresh';

my $errors = 0;
for ^3 { try { $c.ro; CATCH { default { $errors++ } } } }
is $errors, 3, 'a defaulted parameter without is copy stays read-only';

my @attr;
my @var;
my @plain;
for ^3 -> $i {
    my $r := $c.raw-ret;
    $r = $i + 10;
    @attr.push: $c.v;
    my $y = $i;
    my $z := $c.get($y);
    $z = $i + 20;
    @var.push: $y;
    @plain.push: $c.get($y);
}
is-deeply @attr, [10, 11, 12], 'binding an is raw method result aliases the attribute';
is-deeply @var, [20, 21, 22], 'binding an is raw relay aliases the caller variable';
is-deeply @plain, [20, 21, 22], 'the same calls without a bind return the value';
