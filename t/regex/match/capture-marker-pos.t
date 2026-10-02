use Test;

# A `<(` / `)>` capture marker narrows a Match's `.from`/`.to`, but not
# `.pos`: the position the cursor reached. `Grammar.parse` asks whether the
# cursor covered the subject, so a marker must not fail the parse (#10570).

plan 16;

{
    my $m = '<abc>' ~~ / '<' <( \w+ )> '>' /;
    is $m.from, 1, '<( moves .from';
    is $m.to, 4, ')> moves .to';
    is $m.pos, 5, ')> does not move .pos';
    is ~$m, 'abc', 'the narrowed text';
    is $m.raku, 'Match.new(:orig("<abc>"), :from(1), :pos(5))', '.raku reports the cursor .pos';
}

{
    my $m = '<abc>' ~~ / '<' ~ '>' [<( \w+ )>] /;
    is $m.raku, 'Match.new(:orig("<abc>"), :from(1), :pos(5))',
        'a marker inside a ~ goal match: .pos is the end of the closer';
}

{
    grammar Goal { token TOP { '<' ~ '>' [<( \w+ )>] } }
    my $m = Goal.parse('<abc>');
    ok $m.defined, 'Grammar.parse succeeds with markers inside a ~ goal match';
    is $m.raku, 'Match.new(:orig("<abc>"), :from(1), :pos(5))', '...and reports the narrowed span';
}

{
    grammar Narrowed { token TOP { '<' <( \w+ )> '>' } }
    my $m = Narrowed.parse('<abc>');
    ok $m.defined, 'Grammar.parse succeeds when <( and )> narrow the start rule';
    is "{$m.from} {$m.to} {$m.pos}", '1 4 5', '...with the narrowed .from/.to and the full .pos';
    is ~$m, 'abc', '...and the narrowed text';
}

{
    grammar Short { token TOP { '<' <( \w+ )> } }
    nok Short.parse('<abc>').defined, 'a cursor that stops short still fails the parse';
    ok Short.parse('<abc').defined, 'a cursor that reaches the end parses';
}

{
    my $m = 'xaby'.match(/ a <( b )> y /);
    is "{$m.to} {$m.pos}", '3 4', '.match keeps .pos apart from .to';
    is ('ab ab' ~~ m:g/ a <( b )> /).map(*.pos).join(','), '2,5', 'm:g with a marker';
    my $no-marker = 'xaby' ~~ / a b /;
    is "{$no-marker.to} {$no-marker.pos}", '3 3', 'without a marker .pos is .to';
}
