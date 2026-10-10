use Test;

plan 14;

# #12472: a built-in routine exposes the candidates Rakudo declares.
is &say.candidates.elems, 3, '&say has three candidates';
is &abs.candidates.elems, 8, '&abs has eight candidates';
ok &say.candidates.all.multi, 'the say candidates are multi';

is &say.cando(\()).elems, 2, 'say() matches two candidates';
is &say.cando(\(1, 2)).elems, 1, 'say(1, 2) matches the slurpy candidate';

my @loc = &say.candidates.map({ .file ~ ":" ~ .line });
is @loc[0], 'SETTING::src/core.c/io_operators.rakumod:85', 'first say candidate location';
ok @loc.all.starts-with('SETTING::'), 'every location is in the setting';
ok &say.candidates.map(*.line).all > 0, 'every candidate has a line';

is Str.^can("Numeric").map(*.candidates.Slip).elems, 3,
    'Str.^can("Numeric") candidates (the sourcery count)';
is Str.^can("Int").map(*.candidates.elems).List, (1, 2, 1),
    'Str.^can("Int") per-method candidate counts';
is 42.^can("base").map(*.candidates.elems).List, (5,), '42.base has five candidates';

# A user routine of the same name owns the name: only its own candidates.
{
    my multi sub zzz-user(Int $x) { 1 }
    my multi sub zzz-user(Str $x) { 2 }
    is &zzz-user.candidates.elems, 2, 'user multi keeps its own candidates';
}
is &say.candidates[0].signature.params.elems, 0, 'say() candidate has no parameters';
is &say.candidates[1].signature.params[0].name, '\x'.substr(1), 'say(\x) names its parameter';

done-testing;
