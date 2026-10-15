use v6;
use Test;
use YAMLish;

# MUGS::Core (ecosystem): a YAMLish parse that fails (here, on binary junk)
# must not leave a subrule argument-binding error behind for the next `~~`.
plan 3;

my $invalid = buf8.new(^256).decode('latin-1');
my $r = try load-yaml($invalid);
ok !$r.defined, 'invalid YAML does not load';
my $ok = False;
lives-ok { $ok = (5 ~~ Int) }, 'a later smartmatch does not throw';
ok $ok, 'and still matches';
