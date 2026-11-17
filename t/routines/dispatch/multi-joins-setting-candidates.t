use Test;

# A scope that adds a `multi` to a setting multi routine derives a proto that
# holds the setting's candidates plus its own, as rakudo does (#17923):
# `.candidates` lists the setting's first, then the program's. A program
# `proto`, or an `only` sub of the same name, owns its family alone.
# Counts checked against rakudo (RakuAST frontend).

plan 9;

my $setting-plus = &infix:<+>.candidates.elems;

{
    multi infix:<+>(Str $l, Int $r) { 1 }
    is &infix:<+>.candidates.elems, $setting-plus + 1,
        'a block-local multi joins the setting candidates';
    is &infix:<+>.candidates[*-1].signature.raku, ':(Str $l, Int $r)',
        '... listed after them';
    is &infix:<+>("a", 1), 1, '... and dispatch still reaches it';
    is &infix:<+>(2, 3), 5, '... and the setting candidates';
}

{
    multi infix:<+>(Str $l, Int $r) { 1 }
    multi infix:<+>(Int $l, Str $r) { 2 }
    is &infix:<+>.candidates.elems, $setting-plus + 2, 'two multis join together';
}

{
    proto infix:<+>(|) {*}
    multi infix:<+>(Str $l, Int $r) { 1 }
    is &infix:<+>.candidates.elems, 1, 'a program proto owns its family';
}

sub only-mine(Int $x) { }
{
    multi only-mine(Str $x) { }
    is &only-mine.candidates.elems, 1, 'a user routine is no setting family';
}

my $setting-say = &say.candidates.elems;
{
    multi say(Int $a, Int $b) { 5 }
    is &say.candidates.elems, $setting-say + 1, 'a multi say joins too';
    is &say.cando(\(1, 2)).elems, 2, 'cando sees the joined family';
}
