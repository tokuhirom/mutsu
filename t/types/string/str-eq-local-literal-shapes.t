use Test;

# `$local eq 'literal'` and `$local ne 'literal'` compile to one fused op that
# compares the slot's plain Str in place (ADR-18357). Every value a local can
# hold, and every scope that changes what `eq` means, keeps its answer.

plan 18;

sub is-space(Str $ch --> Bool:D) {
    $ch eq ' ' || $ch eq "\n" || $ch eq "\t" || $ch eq "\f" || $ch eq "\r";
}
is "a b\nc\td".comb.grep(&is-space).elems, 3, 'a predicate of fused compares';

my $s = 'abc';
ok $s eq 'abc', 'eq on a plain Str';
nok $s eq 'abd', 'eq that differs';
ok $s ne 'abd', 'ne';
nok $s ne 'abc', 'ne that matches';
ok $s eq "a\x[62]c", 'a literal with an escape';

my $n = 42;
ok $n eq '42', 'an Int local stringifies';
my $a = <42>;
ok $a eq '42', 'an IntStr allomorph local';
my $j = 'a' | 'b';
ok ?($j eq 'b'), 'a Junction local autothreads';
my $m = 'hi' but role { };
ok $m eq 'hi', 'a mixin local';
my $nfd = "e\x[301]";
ok $nfd eq "\x[e9]", 'graphemes compare after normalization';

my $captured = 'x';
my &peek = { $captured };
$captured = 'y';
ok $captured eq 'y', 'a variable a closure captures';
is peek(), 'y', '... and the closure sees the same value';

{
    my $u;
    my $warned = False;
    CONTROL { when CX::Warn { $warned = True; .resume } }
    ok $u eq '', 'an undefined local compares as the empty string';
    ok $warned, '... and warns as the unfused form does';
}

{
    multi sub infix:<eq>(Str $x, Str $y where 'magic') { 'user eq' }
    my $l = 'x';
    is ($l eq 'magic'), 'user eq', 'a user candidate in scope answers its case';
    is ($l eq 'y'), False, '... and the core operator the rest';
}

my @seen;
for <a b c> -> $c { @seen.push($c) if $c ne 'b' }
is-deeply @seen, [<a c>], 'ne in a loop over a pointy parameter';
