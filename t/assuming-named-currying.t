use Test;

plan 4;

sub tester(:$a, :$b, :$c) {
    "a$a b$b c$c";
}

my $w = &tester.assuming(b => 'x');
is $w(a => 'w', c => 'y'), 'aw bx cy', 'currying one named argument works';

my $v = $w.assuming(c => 'z');
is $v(a => 'q'), 'aq bx cz', 'currying can chain on already-curried callable';

my $u = (sub { }).assuming(:a);
ok $u.can('Failure'), 'unexpected named primer produces Failure mixin';

my $t = (sub (Str :$a!) { }).assuming(:a);
ok $t.can('Failure'), 'typed named primer mismatch produces Failure mixin';
