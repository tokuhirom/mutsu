# ADR-0135 §8, Slice E (D7): the regex tree walk is deleted, so every match
# runs the compiled backtracking engine. `MUTSU_VM_STATS` no longer reports a
# `regex-walk:` line, `MUTSU_RX_VM=off` no longer selects another engine, and
# no pattern of these shapes is declined by the compiler. Each case was once
# answered (or bridged) by the walk; the values are rakudo's.
use Test;

plan 16;

sub stats(Str $code, *%extra-env) {
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my @err = $proc.err.slurp(:close).lines;
    ($out, @err.first(*.contains('regex-vm:')) // '', @err.grep(*.contains('regex-walk')).elems)
}

{
    my ($out, $vm, $walk) = stats('say ("abab" ~~ m:ov/ab/).elems, " ", ("abc" ~~ m:ex/\w+/).elems');
    is $out, "2 6\n", ':ov and :ex answer';
    is $walk, 0, 'no regex-walk line is reported';
}

{
    my ($out, $vm) = stats('say ~("ab" ~~ /b/)', :MUTSU_RX_VM<off>);
    is $out, "b\n", 'MUTSU_RX_VM=off is ignored';
    like $vm, /'declined=0 ' .* 'runs=' <[1..9]>/, 'the compiled engine answered';
}

{
    my ($out, $vm) = stats(
        'class C { method m { "a" } }; grammar G { token TOP { <x(C.new)> }; token x($o) { a <?{ $o.m eq "a" }> } }; say ~G.parse("a")');
    is $out, "a\n", 'a call with an object argument parses';
    like $vm, /'declined=0 '/, 'nothing in it is declined';
}

{
    my ($out, $vm) = stats(
        'grammar G { token TOP { <x("b")> }; token x($*W) { a <y> }; token y { . <?{ $/.Str eq $*W }> } }; say ~G.parse("ab")');
    is $out, "ab\n", 'a `$*` rule parameter reaches a subrule the callee calls';
    like $vm, /'declined=0 '/, 'nothing in it is declined';
}

{
    my ($out, $vm) = stats(
        'grammar G { token TOP { :my $*D = 1; <a> <a> }; token a { :my $*E = 2; \\w <?{ $*D + $*E == 3 }> } }; say ~G.parse("ab")');
    is $out, "ab\n", 'a grammar whose rules declare `:my $*x` parses';
    like $vm, /'declined=0 '/, 'nothing in it is declined';
}

{
    my ($out, $vm) = stats(
        'grammar G { token TOP { <w>+ % "," <.ws>* }; token w { \\w+ } }; say G.parse("ab,cd")<w>.elems');
    is $out, "2\n", 'a quantified call parses';
    like $vm, /'declined=0 '/, 'nothing in it is declined';
}

{
    my ($out, $vm) = stats('say ~("aa" ~~ /(a)$0/)');
    is $out, "aa\n", 'a backreference matches';
    like $vm, /'declined=0 '/, 'it is compiled';
}

{
    my ($out, $vm) = stats('say ~("abc" ~~ /b c/)');
    is $out, "bc\n", 'a plain match';
    like $vm, /'compiled=' <[1..9]> \d* ' declined=0 '/, 'it is compiled';
}
