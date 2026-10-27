use Test;

plan 9;

sub setit {
    $GLOBAL::t-scalar = 'scalar';
    %GLOBAL::t-hash = default => 'db1';
    @GLOBAL::t-array = 1, 2;
}
setit();

my $r = await start {
    (
        $GLOBAL::t-scalar // 'none',
        %GLOBAL::t-hash<default> // 'none',
        @GLOBAL::t-array.elems,
        $*t-scalar // 'none',
        %*t-hash<default> // 'none',
        (try { @*t-array.elems } // 'none'),
    )
};
is $r[0], 'scalar', 'a $GLOBAL:: scalar assigned in a sub is visible in start';
is $r[1], 'db1', 'a %GLOBAL:: hash assigned in a sub is visible in start';
is $r[2], 2, 'an @GLOBAL:: array assigned in a sub is visible in start';
is $r[3], 'scalar', 'the dynamic spelling of the scalar is visible in start';
is $r[4], 'db1', 'the dynamic spelling of the hash is visible in start';
is $r[5], 2, 'the dynamic spelling of the array is visible in start';

my $p = start {
    react { whenever Supply.from-list(^2) { } }
    $GLOBAL::t-scalar
};
is await($p), 'scalar', 'visible after a react block inside start';

$GLOBAL::t-top = 'top';
is await(start { $GLOBAL::t-top }), 'top', 'a top-level assignment is still visible';

sub bump { $GLOBAL::t-n = 5 }
bump();
my @all = (^3).map: { start { $GLOBAL::t-n } };
is-deeply (await @all).List, (5, 5, 5), 'every worker sees the value';
