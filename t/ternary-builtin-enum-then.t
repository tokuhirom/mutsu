use v6;
use Test;

# A built-in enum value (Order's Less/Same/More, Endian's *Endian) is a complete
# nullary term, so it is a valid then-branch of a statement-level `?? !!` ternary
# — it must not be mistaken for the head of a listop call that gobbles the `!!`.
# Regression pin for dist Version::Semverish (`... ?? More !! Less` in sink context).

# statement-level (sink) ternary — these previously failed to parse
{
    1 ?? More !! Less;
    0 ?? More !! Less;
    pass 'statement-level `?? More !! Less` parses';
}

# value is correct in expression context
is (1 ?? More  !! Less), More, '1 ?? More !! Less is More';
is (0 ?? More  !! Less), Less, '0 ?? More !! Less is Less';
is (1 ?? Same  !! More), Same, 'Same as then-branch';

# Endian enum values too
is (1 ?? LittleEndian !! BigEndian), LittleEndian, 'Endian enum then-branch';

# inside an else block (the Version::Semverish idiom)
my $x = 1;
my $r = do {
    if $x == 2 { Same }
    else       { $x == 1 ?? More !! Less }
};
is $r, More, 'ternary with enum branches inside an else block';

# ordinary type-object then-branches still work
is (1 ?? Int !! Str), Int, 'type object then-branch still works';

done-testing;
