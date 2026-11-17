use Test;

# `my \r = f(...)`: a sigilless term initialized by a routine call binds
# whatever the call hands back. An `is raw` / `is rw` routine returning its
# `is rw` parameter hands back the caller's container, so the term aliases
# it; any other return is a value, and the term is readonly (#17678).

plan 9;

sub raw-back($x is rw) is raw { $x }
sub rw-back($x is rw) is rw { $x }
sub plain-back($x) { $x }
sub local-var() { my $v = 1; $v }

sub in-sub() {
    my $x = 1;
    my \r = raw-back($x);
    r = 5;
    is $x, 5, 'an is raw routine\'s returned container is aliased (in a sub)';
}
in-sub();

my $y = 1;
my \s = raw-back($y);
s = 6;
is $y, 6, '... and at the top level';

my $w = 1;
my \t = rw-back($w);
t = 8;
is $w, 8, 'an is rw routine\'s too';
t++;
is $w, 9, '++ writes through as well';

my $p = 1;
my \u = plain-back($p);
dies-ok { u = 3 }, 'a routine returning its readonly parameter: readonly';
is $p, 1, '... and the argument is untouched';

my \v = local-var();
throws-like { v = 3 }, X::Assignment::RO, 'a routine returning its own lexical returns a value';
is v, 1, '... which the term holds';

my \k = (sub { 42 })();
throws-like { k = 1 }, X::Assignment::RO, 'an anonymous sub call is a value too';
