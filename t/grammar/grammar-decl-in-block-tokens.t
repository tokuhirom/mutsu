use Test;

# A `grammar`/`class` declaration is package-scoped (`our`) in Raku, not scoped
# to an enclosing bare block. mutsu's block-scope registry save/restore preserved
# the CLASS but restored `token_defs` wholesale, dropping every token declared in
# a grammar body inside a block. A grammar subclassing it from another scope then
# could not resolve `<id>`, and the engine's method-subrule fallback reported
# "No such method 'id' for invocant of type 'Match'".
#
# This is exactly 99problems-41-to-50.t's shape: its P46 block declares the base
# grammar that its P47 block subclasses.

plan 6;

{
    grammar BaseG {
        token id  { <[A..Z]> }
        token num { \d+ }
    }
}

grammar DerivedG is BaseG {
    rule TOP { <id> <num> }
}

ok DerivedG.parse('A 42'), 'base grammar declared in a block keeps its tokens for a later subclass';
is DerivedG.parse('A 42')<id>.Str,  'A',  'inherited <id> captures';
is DerivedG.parse('A 42')<num>.Str, '42', 'inherited <num> captures';

# The base itself stays usable after its block.
ok BaseG.parse('X', :rule<id>), 'base grammar itself still parses after its block exits';

# Proto tokens declared in a block survive with their candidates.
{
    grammar ProtoG {
        proto token thing {*}
        token thing:sym<a> { <sym> }
        token thing:sym<b> { <sym> }
    }
}
grammar ProtoD is ProtoG {
    rule TOP { <thing> }
}
ok ProtoD.parse('a'), 'proto token candidates survive their declaring block (a)';
ok ProtoD.parse('b'), 'proto token candidates survive their declaring block (b)';

# vim: expandtab shiftwidth=4
