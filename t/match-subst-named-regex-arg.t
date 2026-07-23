use Test;

# A named regex/token passed as a *value* — `&Named`, or a hash/array slot
# holding it — must work as the pattern argument of `.match` and `.subst`.
# mutsu represents `&Named` as a `Routine` (is_regex); `.match`/`.subst`
# previously only accepted a `Regex` value (`rx/.../`) or a Str, so `&Named`
# fell through to literal-string matching of its stringified `sub Named {...}`
# form and never matched. (`$str ~~ &Named` already worked.)

plan 12;

my regex Word  { \w+ }
my regex Digits { \d+ }
my regex KV { $<k>=(\w+) '=' $<v>=(\w+) }

# --- .match with &Named ---
{
    my $s = "foo bar baz";
    is $s.match(&Word).Str, 'foo', '.match(&Named) single match';
    is $s.match(&Word, :g).elems, 3, '.match(&Named, :g) global count';
    is $s.match(&Word, :g).map(*.Str).join(','), 'foo,bar,baz', '.match(&Named, :g) values';
    ok !"...".match(&Word).defined, '.match(&Named) with no match is undefined';
}

# --- named captures of the named regex are top-level on the Match ---
{
    my $m = "a=1 b=2".match(&KV, :g);
    is $m.elems, 2, '.match(&KV, :g) count';
    is $m[0]<k>, 'a', 'named capture <k> is top-level (0)';
    is $m[1]<v>, '2', 'named capture <v> is top-level (1)';
}

# --- a hash slot holding &Named (the Text::CodeProcessing shape) ---
{
    my %search = word => &Word, digits => &Digits;
    my $s = "ab 12 cd 34";
    is $s.match(%search<digits>, :g).map(*.Str).join(','), '12,34',
        '.match(%hash{key}) where the slot holds &Named';
}

# --- .subst with &Named ---
{
    is "foo bar baz".subst(&Word, 'X'), 'X bar baz', '.subst(&Named) first match';
    is "foo bar baz".subst(&Word, 'X', :g), 'X X X', '.subst(&Named, :g) global';
    is "a1b2c3".subst(&Digits, '#', :g), 'a#b#c#', '.subst(&Named, :g) digits';
}

# --- .subst with &Named and a replacement closure using the match ---
{
    is "foo bar".subst(&Word, -> $m { $m.Str.uc }, :g), 'FOO BAR',
        '.subst(&Named, closure, :g)';
}
