use Test;

# Routine.add_dispatchee with an anonymous / closure-carrying sub (#10929):
# the proto holds the dispatchee Value itself, so the candidate keeps its
# captured lexicals. Used by ecosystem `shorten-sub-commands` and
# `CLI::Version` (CLI::Ecosystem dependency closure).
plan 13;

{
    my proto sub p(|) {*}
    my multi sub p(Int $x) { "int $x" }
    BEGIN &p.add_dispatchee: my sub (Str:D $s, |c) { "str $s" };
    is p(1), "int 1", "existing candidate still dispatches";
    is p("a"), "str a", "anonymous BEGIN-time dispatchee is selected";
    is &p.dispatchees.elems, 2, ".dispatchees counts the added candidate";
}

{
    my proto sub q(|) {*}
    my multi sub q(Int $x) { "int $x" }
    my @subs = (1..3).map: -> $n { sub (Str $s) { "$s$n" } };
    &q.add_dispatchee(@subs[1]<>);
    my ($sx, $i5) = "x", 5;
    is q($sx), "x2", "closure dispatchee keeps the value it captured";
    is q($i5), "int 5", "original candidate unaffected";
}

{
    # A closure called from a different scope sees its own lexicals.
    my proto sub r(|) {*}
    sub make-candidate($tag) { sub (Rat $v) { "$tag $v" } }
    &r.add_dispatchee(make-candidate("rat"));
    sub call-it { my $tag = "wrong"; my $v = 1.5; r($v) }
    is call-it(), "rat 1.5", "captured lexical wins over the caller's same-named one";
}

{
    my proto sub s(|) {*}
    sub named(Int $i) { "named $i" }
    my $ret = &s.add_dispatchee(&named);
    is $ret.name, "s", "add_dispatchee returns the proto";
    my ($i3, $i4, $sv) = 3, 4, "v";
    is s($i3), "named 3", "named dispatchee";
    my $k = 10;
    &s.add_dispatchee(sub (Str $x) { "$x $k" });
    is s($sv), "v 10", "second, anonymous dispatchee";
    is s($i4), "named 4", "first dispatchee still selected";
}

{
    # callsame from an added dispatchee reaches the next candidate.
    my proto sub c(|) {*}
    my multi sub c(Any $x) { "any $x" }
    my $tag = "T";
    &c.add_dispatchee(sub (Int $x) { "int $x $tag, " ~ callsame });
    my $one = 1;
    is c($one), "int 1 T, any 1", "callsame defers to the declared candidate";
    sub through(&g) { my $tag = "no"; g($one) }
    is through(&c), "int 1 T, any 1", "the proto as a code value dispatches to it too";
    is &c.candidates.elems, 2, ".candidates lists the added value";
}
