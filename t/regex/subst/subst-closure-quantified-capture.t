use v6;
use Test;

plan 5;

# Inside a .subst replacement closure, a quantified capture group must expose
# the LIST of per-iteration matches as $0 — not just the last one. This is
# the URI::Escape uri-unescape shape.
my $out = '%7C%25abc'.subst(:g, / [ '%' (<.xdigit> ** 2) ]+ /, -> $/ {
    $0.flatmap({ :16(~$_).chr }).join
});
is $out, '|%abc', 'quantified $0 in a subst closure carries all iterations';

my $elems;
'%41%42'.subst(:g, / [ '%' (<.xdigit> ** 2) ]+ /, -> $/ { $elems = $0.elems; '' });
is $elems, 2, '$0.elems sees both iterations';

# Buf-building form (the UTF-8 decode branch of uri-unescape).
my $utf8 = '%C3%A5'.subst(:g, / [ '%' (<.xdigit> ** 2) ]+ /, -> $/ {
    Buf.new($0.flatmap({ :16(~$_) })).decode('UTF-8')
});
is $utf8, 'å', 'quantified captures rebuild a UTF-8 sequence';

# A non-quantified group keeps its scalar shape.
is 'a1b'.subst(/ (\d) /, -> $/ { "<{$0}>" }), 'a<1>b',
    'plain $0 in a subst closure still works';

# Multiple groups where only the second is quantified.
my $shape;
'x-ababy' ~~ / (\w) '-' [ (ab) ]+ /;
'x-abab'.subst(/ (\w) '-' [ (ab) ]+ /, -> $/ { $shape = "{$0}:{$1.elems}"; '' });
is $shape, 'x:2', 'mixed plain + quantified groups keep their shapes';
