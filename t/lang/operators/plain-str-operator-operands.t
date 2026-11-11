use Test;

# Two plain `Str` operands of `eq`/`ne`/`lt`/... and `~` skip the operand
# coercion chain (a plain Str is its own `.Stringy`); everything that chain
# exists for must still apply to the operands that need it. From
# MCP::Server::Tool::Web's tokenizer (`$ch eq ' '` per character).
plan 17;

is 'a' eq 'a', True, 'eq';
is 'a' ne 'a', False, 'ne';
is 'a' lt 'b', True, 'lt';
is 'b' le 'a', False, 'le';
is 'b' gt 'a', True, 'gt';
is 'a' ge 'a', True, 'ge';
is "e\x[301]" eq "\x[e9]", True, 'eq compares graphemes';
is 'ab' ~ 'cd', 'abcd', '~';

is-deeply (('a' | 'b') eq 'b').so, True, 'a junction operand still autothreads';
is <42> eq '42', True, 'an allomorph compares by its string';
is ('x' but role { method Str { 'y' } }) eq 'x', True, 'a mixin over a Str compares its payload';

class Named { method Str { 'named' } }
is Named.new eq 'named', True, 'an object uses its own Str';
is 'pre-' ~ Named.new, 'pre-named', '~ with an object operand';

my $p := Proxy.new(FETCH => { 'fetched' }, STORE => -> $, $ { });
is $p eq 'fetched', True, 'a Proxy operand is fetched';

{
    sub infix:<eq>($a, $b) { 'mine' }
    is 'a' eq 'b', 'mine', 'a user eq candidate still wins';
}
{
    sub infix:<~>($a, $b) { ('[', $a, '|', $b, ']').join }
    is 'a' ~ 'b', '[a|b]', 'a user ~ candidate still wins';
}

my @w;
{
    CONTROL { when CX::Warn { @w.push: .message; .resume } }
    my $r = Nil eq '';
}
is @w.elems, 1, 'Nil in a string comparison still warns';
