# ADR-0135 §8, Slice E: `MUTSU_VM_STATS` counts every use of the regex tree
# walk's code on a `regex-walk:` line, grouped as `walked` (a whole match the
# walk answered), `bridged` (a compiled run handed a piece back) and `leaf`
# (one atom the walk's single-atom arm matched), each with the reason. This is
# the ratchet the walk's deletion is read off, so each group must name what
# put the match there.
use Test;

plan 10;

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
        'grammar G { token TOP { <x(1)> }; token x($n) { a } }; say ~G.parse("a")');
    is $out, "a\n", 'a call with arguments still parses';
    like $line, /'bridged=1 (args=1)'/, 'a call with arguments is bridged, reason args';
}

{
    my ($out, $line) = walk-line('say ~("aa" ~~ /(a)$0/)');
    is $out, "aa\n", 'a backreference still matches';
    like $line, /'leaf=1 (backref=1)'/, 'a backreference is a leaf of the walk';
}

{
    my ($out, $line) = walk-line('say ~("abc" ~~ /b c/)');
    is $out, "bc\n", 'a plain match';
    is $line, '[mutsu vm-stats] regex-walk: walked=0 () bridged=0 () leaf=0 ()',
        'a fully compiled match uses no walk code at all';
}
