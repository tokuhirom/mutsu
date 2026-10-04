# ADR-0135 §8, Slice E: `MUTSU_VM_STATS` counts every use of the regex tree
# walk's code on a `regex-walk:` line, grouped as `walked` (a whole match the
# walk answered), `bridged` (a compiled run handed a piece back) and `leaf`
# (one atom the walk's single-atom arm matched), each with the reason. This is
# the ratchet the walk's deletion is read off, so each group must name what
# put the match there.
use Test;

plan 16;

sub walk-line(Str $code, *%extra-env) {
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    my $line = $err.lines.first(*.contains('regex-walk:')) // '';
    ($out, $line)
}

{
    my ($out, $line) = walk-line('say ("abab" ~~ m:ov/ab/).elems, " ", ("abc" ~~ m:ex/\w+/).elems');
    is $out, "2 6\n", ':ov and :ex answer';
    like $line, /'walked=0 ()'/,
        'every end at a position runs on the compiled engine (Goal::Ends)';
}

{
    my ($out, $line) = walk-line('say ~("ab" ~~ /b/)', :MUTSU_RX_VM<off>);
    is $out, "b\n", 'the walk answers with the compiled engine off';
    like $line, /'walked=1 (context:vm-off=1)'/,
        'the context that keeps the compiled engine out is named';
}

{
    my ($out, $line) = walk-line(
        'class C { method m { "a" } }; grammar G { token TOP { <x(C.new)> }; token x($o) { a <?{ $o.m eq "a" }> } }; say ~G.parse("a")');
    is $out, "a\n", 'a call with an object argument still parses';
    like $line, /'walked=0 () bridged=0 ()'/,
        'a call whose argument must be bound for its match runs as a frame (no bridge)';
}

{
    my ($out, $line) = walk-line(
        'grammar G { token TOP { <x("b")> }; token x($*W) { a <y> }; token y { . <?{ $/.Str eq $*W }> } }; say ~G.parse("ab")');
    is $out, "ab\n", 'a `$*` rule parameter reaches a subrule the callee calls';
    like $line, /'walked=0 () bridged=0 ()'/,
        'a call that binds a `$*` parameter runs as a frame (no bridge)';
}

{
    my ($out, $line) = walk-line(
        'grammar G { token TOP { :my $*D = 1; <a> <a> }; token a { :my $*E = 2; \\w <?{ $*D + $*E == 3 }> } }; say ~G.parse("ab")');
    is $out, "ab\n", 'a grammar whose rules declare `:my $*x` parses';
    like $line, /'walked=0 () bridged=0 ()'/,
        'rule declarations keep neither the match nor its calls off the compiled engine';
}

{
    my ($out, $line) = walk-line(
        'grammar G { token TOP { <w>+ % "," <.ws>* }; token w { \\w+ } }; say G.parse("ab,cd")<w>.elems');
    is $out, "2\n", 'a quantified call parses';
    like $line, /'walked=0 () bridged=0 ()'/,
        'a quantified call is a loop of frame calls (no scan, no single-candidate arm)';
}

{
    my ($out, $line) = walk-line('say ~("aa" ~~ /(a)$0/)');
    is $out, "aa\n", 'a backreference still matches';
    is $line, '[mutsu vm-stats] regex-walk: walked=0 () bridged=0 () leaf=0 ()',
        'a backreference is matched by the compiled engine, not the walk';
}

{
    my ($out, $line) = walk-line('say ~("abc" ~~ /b c/)');
    is $out, "bc\n", 'a plain match';
    is $line, '[mutsu vm-stats] regex-walk: walked=0 () bridged=0 () leaf=0 ()',
        'a fully compiled match uses no walk code at all';
}
