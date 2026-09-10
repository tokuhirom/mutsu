use Test;

plan 17;

# Narrowness in Raku is computed from the POSITIONAL parameters. Named
# parameters decide applicability and contribute exactly ONE boolean narrowness
# step ("declares a named at all"); beyond that, equally-narrow candidates are
# resolved by DECLARATION ORDER. Every expectation below was taken from rakudo
# (`&f.cando(\(...))` reports the same ordering).

# --- a named parameter's TYPE does not narrow -------------------------------
{
    proto h(:$a) {*}
    multi h(    :$a) { "untyped" }
    multi h(Str :$a) { "typed" }
    is h(a => "x"), "untyped", 'untyped named candidate declared first wins';

    proto h2(:$a) {*}
    multi h2(Str :$a) { "typed" }
    multi h2(    :$a) { "untyped" }
    is h2(a => "x"), "typed", '...and reversing the declarations reverses it';

    proto h3(:$a) {*}
    multi h3(    :$a) { "untyped" }
    multi h3(Any :$a) { "any" }
    is h3(a => "x"), "untyped", 'Any :$a is no narrower than an untyped :$a';
}

# --- how MANY nameds a candidate declares does not narrow -------------------
{
    proto K($in, :$suffix, :$len, :$rate) {*}
    multi K($in, :$suffix, :$rate)        { "two" }
    multi K($in, :$suffix, :$len, :$rate) { "three" }
    is K(1, suffix => 6, rate => 1152), "two",
        'both candidates bind, so the one declared first wins';
    is K(1, suffix => 6, len => 3, rate => 1152), "three",
        'only the wider candidate accepts :len, so it is the only match';

    proto J($in, :$suffix, :$len, :$rate) {*}
    multi J($in, :$suffix, :$len, :$rate) { "three" }
    multi J($in, :$suffix, :$rate)        { "two" }
    is J(1, suffix => 6, rate => 1152), "three",
        'reversed declaration order reverses the winner';
}

# --- but declaring ANY named IS one narrowness step -------------------------
{
    proto p1() {*}
    multi p1()     { "none" }
    multi p1(:$x)  { "named" }
    is p1(), "named", 'a candidate declaring a named beats one declaring none';

    proto p2() {*}
    multi p2(:$x)  { "named" }
    multi p2()     { "none" }
    is p2(), "named", '...in either declaration order';

    proto p3() {*}
    multi p3()         { "none" }
    multi p3(:$x, :$y) { "named2" }
    is p3(), "named2", 'two nameds beat none just as one does';

    proto p5($a) {*}
    multi p5($a)       { "plain" }
    multi p5($a, :$x)  { "named" }
    is p5(1), "named", 'the same holds alongside a positional parameter';
}

# --- positional narrowness still outranks the named step --------------------
{
    proto q1($a) {*}
    multi q1(Int  $a)      { "int" }
    multi q1(Cool $a, :$x) { "cool-named" }
    is q1(1), "int", 'a narrower positional type beats a declared named';

    proto q2($a) {*}
    multi q2(Cool $a, :$x) { "cool-named" }
    multi q2(Int  $a)      { "int" }
    is q2(1), "int", '...in either declaration order';
}

# --- ...and the named step outranks the optional-positional count -----------
{
    proto r1($a) {*}
    multi r1($a)       { "req" }
    multi r1($a?, :$x) { "opt-named" }
    is r1(1), "opt-named", 'a declared named beats a required (non-optional) positional';

    proto r3($a) {*}
    multi r3($a)         { "plain" }
    multi r3(*@a, :$x)   { "slurpy-named" }
    is r3(1), "plain", 'but a slurpy is still much wider than a plain positional';
}

# --- a candidate that cannot accept a passed named is simply not applicable --
{
    proto N($in, :$a, :$b) {*}
    multi N($in, :$a) { "onlyA" }
    is N(1, a => 1), "onlyA", 'the only accepting candidate is chosen';
    is (try N(1, b => 2)) // "no-candidate", "no-candidate",
        'no candidate accepts :b, so the call fails';

    proto M($in, :$a, :$b) {*}
    multi M($in, :$b)      { "onlyB" }
    multi M($in, :$a, :$b) { "both" }
    is M(1, a => 1, b => 2), "both",
        'the earlier-declared candidate does not accept :a, so the later one runs';
}
