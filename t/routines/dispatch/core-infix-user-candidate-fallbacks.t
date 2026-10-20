use Test;
use MONKEY-SEE-NO-EVAL;

for (
    ['..', '(2 .. 4).raku', '2..4'],
    ['..^', '(2 ..^ 4).raku', '2..^4'],
    ['^..', '(2 ^.. 4).raku', '2^..4'],
    ['^..^', '(2 ^..^ 4).raku', '2^..^4'],
    ['but', '(2 but False).Str', '2'],
    ['unicmp', '("a" unicmp "b").Str', 'Less'],
    ['coll', '("a" coll "b").Str', 'Less'],
    ['?|', '(False ?| True).Str', 'True'],
    ['?&', '(False ?& True).Str', 'False'],
    ['?^', '(False ?^ True).Str', 'True'],
    ['<=>', '(2 <=> 3).Str', 'Less'],
    ['=~=', '(2 =~= 2).Str', 'True'],
    ['o', '({ $_ + 1 } o { $_ * 2 })(3).Str', '7'],
    ['...', '(1 ... 3).join(",")', '1,2,3'],
    ['...^', '(1 ...^ 3).join(",")', '1,2'],
    ['+&', '(2 +& 3).Str', '2'],
    ['+|', '(2 +| 1).Str', '3'],
    ['+^', '(3 +^ 1).Str', '2'],
    ['+<', '(2 +< 2).Str', '8'],
    ['+>', '(8 +> 2).Str', '2'],
    ['%%', '(2 %% 3).Str', 'False'],
    ['min', '(2 min 3).Str', '2'],
    ['max', '(2 max 3).Str', '3'],
) -> @case {
    my ($op, $native, $expected) = @case;
    my $source = 'my class T {}; multi infix:«' ~ $op ~
        '»(T $a, T $b) { "USER" }; ((T ' ~ $op ~ ' T), ' ~ $native ~ ')';
    is-deeply EVAL($source), ('USER', $expected),
        "$op selects the user candidate and retains the core body";
}

is-deeply EVAL(q:to/END/), (True, 1), 'core but composes a role and initializes its attributes';
    my class T { }
    multi infix:«but»(T $a, T $b) { 'USER' }
    my $initialized = 0;
    role R { has $.value = ($initialized = 1) }
    my $mixed = 2 but R;
    ($mixed ~~ R, $mixed.value)
    END

dies-ok { EVAL('class NullaryType {}; NullaryType 42') },
    'a declared type is not a no-paren listop';
done-testing;
