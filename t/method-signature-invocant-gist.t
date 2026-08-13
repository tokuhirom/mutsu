use Test;

# A method's signature.gist renders the invocant as `Type $name::` (an
# anonymous `$::` when the invocant has no explicit name, or `$name::` when
# it does), never the old `Type:` form — verified against Rakudo v2026.06.
# Found by the ADR-0019 E9-pre raku verification campaign.

plan 6;

class C {
    multi method m(Int $x) { }
    multi method m(Str $x) { }
}
is C.^lookup('m').candidates.map(*.signature.gist).join(" | "),
    '(C $:: Int $x, *%_) | (C $:: Str $x, *%_)',
    'implicit invocant on multi candidates renders as "Type $::"';

class D { method n() { } }
is D.^lookup('n').signature.gist, '(D $:: *%_)',
    'implicit invocant on a zero-arg method';

class E { method p(Int $x, Str $y) { } }
is E.^lookup('p').signature.gist, '(E $:: Int $x, Str $y, *%_)',
    'implicit invocant followed by ordinary positionals';

class F { method q(*@a) { } }
is F.^lookup('q').signature.gist, '(F $:: *@a, *%_)',
    'implicit invocant followed by a slurpy';

class G { method m($self:) { } }
is G.^lookup('m').signature.gist, '($self:: *%_)',
    'explicit named invocant with no type renders as "$name::"';

class H { method m(H $x:) { } }
is H.^lookup('m').signature.gist, '(H $x:: *%_)',
    'explicit named and typed invocant renders as "Type $name::"';
