use Test;

# A block has no `$/` of its own: a match inside it sets the `$/` of the scope
# it is written in, even when another routine calls it (#12720, ADR-12529
# phase 3). That holds for `.subst` / `.match` as well as for `~~`, and for a
# block created before the scope's own later matches.

plan 12;

sub t(&c) { c() }
my $s = 'abc';

t { $s.subst(/b/, 'B') };
is ~$/, 'b', '.subst in a block called by a routine sets the outer $/';

t { $s.match(/c/) };
is ~$/, 'c', '.match in a block called by a routine sets the outer $/';

t { $s ~~ /a/ };
is ~$/, 'a', '~~ in a block called by a routine sets the outer $/';

t { 'xy' ~~ /(y)/ };
is ~$0, 'y', '... and so do its positional captures';

my &later = { 'abc'.match(/c/) };
'zz' ~~ /z/;
t(&later);
is ~$/, 'c', 'a block made before the scope matched again still reaches its $/';

'pq' ~~ /(q)/;
my $seen;
t { $seen = ~$0 };
is $seen, 'q', 'a block reads the $0 of its scope';
t { $seen = $/.Str };
is $seen, 'pq'.substr(1), 'a block reads the $/ of its scope';

sub h { 'xyz' ~~ /y/; t { 'q' ~~ /q/ }; ~$/ }
is h(), 'q', 'a block in a sub sets that sub $/';
is ~$/, 'q', '... and not its caller one';

sub sb { my $x = 'abc'; t { $x.subst(/b/, 'B') }; ~$/ }
is sb(), 'b', '.subst in a block passed from a sub sets the sub $/';

my @l = <a1 b2>.map({ /\d/ ?? ~$/ !! '' });
is-deeply @l, ['1', '2'], 'a map callback matches into the caller scope per item';

my @m = <x y>.map(-> $/ { ~$/ });
is-deeply @m, ['x', 'y'], 'a block that binds its own $/ keeps it';
