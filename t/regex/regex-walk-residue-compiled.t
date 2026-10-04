use Test;

# The last places a compiled run reached the regex tree walk (ADR-0135 §8,
# Slice E, twenty-first part): an LTM measurement of a user `<ws>` or of a
# class holding a token, a `:m` match outside a published subject, and an
# empty quantifier range. Expected values are rakudo's; the walk is deleted
# since (D7), so each must still be compiled (`declined=0`).

plan 13;

sub walk-line(Str $code) {
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my @err = $proc.err.slurp(:close).lines;
    ($out, @err.first(*.contains('regex-vm:')) // '', @err.first(*.contains('regex-eager:')) // '')
}

{
    my ($out, $walk) = walk-line(q:to/CODE/);
        grammar G {
            token TOP { ^ <ws> 'x' $ }
            token ws { <!ww> \s*! [ '#' \h*! <content> \n \s* ]* }
            token content { \N* }
        }
        say G.parse("# c\nx").chars
        CODE
    is $out, "5\n", 'a start rule led by a user <ws> parses';
    like $walk, /'declined=0 '/, 'its LTM measurement is compiled';
}

{
    my ($out, $walk) = walk-line(q:to/CODE/);
        grammar G { token alpha { 'Z' }; token TOP { <+alpha -[q]>+ } }
        say G.parse("ZZ").Str
        CODE
    is $out, "ZZ\n", 'a class holding a grammar token matches';
    like $walk, /'declined=0 '/, 'its LTM measurement is compiled';
}

{
    my ($out, $walk) = walk-line('say ("a" ~~ /a $<q>=[x] ** 0 % ","/).raku');
    is $out, qq{Match.new(:orig("a"), :from(0), :pos(1), :hash(Map.new((:q(Match.new(:orig("a"), :from(1), :pos(1)))))))\n},
        'an aliased ** 0 % sep files an empty match';
    like $walk, /'declined=0 '/, 'and is compiled';
}

is ("a" ~~ /a (x) ** 0 % ","/).raku, 'Match.new(:orig("a"), :from(0), :pos(1), :list(([],)))',
    'a capture under ** 0 % sep is an empty list';
is ("ax" ~~ /a x ** 0..0 % "," x/).Str, 'ax', '** 0..0 % sep matches nothing';

throws-like ｢"xx" ~~ /x ** 2..1/｣, X::Syntax::Regex::QuantifierValue,
    'an empty range raises';
throws-like ｢"xx" ~~ /x ** 2..1 % ","/｣, X::Syntax::Regex::QuantifierValue,
    'an empty range on a separated quantifier raises too';

{
    my ($out, $walk, $eager) = walk-line(
        'grammar G { token TOP { <e> }; token e { <e> "+" \d | \d } }; say ~G.parse("1+2")');
    is $out, "1+2\n", 'a left-recursive rule parses';
    like $walk, /'declined=0 '/, 'its call is compiled';
    like $eager, /'regex-eager: calls=' \d+ ' (lr-seed=' \d+ ')'/,
        'it is counted as an eager call of the compiled engine';
}
