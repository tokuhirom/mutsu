use v6;
use Test;

plan 7;

# IO::Path::Parts is a Positional in Rakudo: .raku / .gist render the three
# parts positionally, each via its own .raku, joined by `,` (no spaces), NOT
# the generic named-attribute walk. (.Str is the object-address form
# `IO::Path::Parts<addr>` and is intentionally not matched — see PLAN 8.15.)
# Pin for PLAN 8.15.
my $p = IO::Path::Parts.new('C:', '/some/dir', 'foo.txt');
is $p.raku, 'IO::Path::Parts.new("C:","/some/dir","foo.txt")', '.raku renders the parts positionally';
is $p.gist, 'IO::Path::Parts.new("C:","/some/dir","foo.txt")', '.gist equals .raku';
is $p.perl, 'IO::Path::Parts.new("C:","/some/dir","foo.txt")', '.perl equals .raku';

# Empty-ish parts still render each part's .raku (empty string is `""`).
my $q = IO::Path::Parts.new('', '.', 'x');
is $q.raku, 'IO::Path::Parts.new("",".","x")', 'empty volume renders as ""';

# IO::Path.parts produces the same object and repr.
my $r = 'foo/bar.txt'.IO.parts;
is $r.raku, 'IO::Path::Parts.new("","foo","bar.txt")', '.IO.parts repr';

# Named construction round-trips through the same positional repr.
my $s = IO::Path::Parts.new(:volume(''), :dirname('a'), :basename('b'));
is $s.raku, 'IO::Path::Parts.new("","a","b")', 'named-arg construction repr';

# A part with a quote in it is escaped by the inner .raku.
my $t = IO::Path::Parts.new('a"b', 'd', 'f');
is $t.raku, 'IO::Path::Parts.new("a\"b","d","f")', 'inner .raku escapes quotes';

done-testing;
