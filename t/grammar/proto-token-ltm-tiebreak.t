use v6;
use Test;

# Rakudo's Longest-Token-Matching breaks an equal-length tie between two proto
# `:sym<>` candidates by declaration order: the FIRST-declared candidate wins
# (PLAN.md 8.20). A globstar `**` (a literal `<sym>` candidate) declared before
# a char-class fall-through must win the 2-char tie against it.

plan 4;

{
    grammar G {
        token TOP { <pp>+ % '/' }
        proto token pp {*}
        token pp:sym<**> { <sym> }        # literal, declared first
        token pp:sym<m>  { <-[/]>+ }      # char-class fall-through
    }
    class Act {
        method TOP($/) { make $<pp>.map(*.ast).join('|') }
        method pp:sym<**>($/) { make 'GLOBSTAR' }
        method pp:sym<m>($/)  { make "M:$/" }
    }
    is G.parse('d/**', :actions(Act)).ast, 'M:d|GLOBSTAR',
        'tie broken toward first-declared literal <sym> candidate';
}

{
    # Swapping the declaration order flips the tie-break winner (Rakudo semantics).
    grammar G2 {
        token TOP { <pp>+ % '/' }
        proto token pp {*}
        token pp:sym<m>  { <-[/]>+ }      # char-class, declared first now
        token pp:sym<**> { <sym> }        # literal, declared second
    }
    class Act2 {
        method TOP($/) { make $<pp>.map(*.ast).join('|') }
        method pp:sym<**>($/) { make 'GLOBSTAR' }
        method pp:sym<m>($/)  { make "M:$/" }
    }
    is G2.parse('d/**', :actions(Act2)).ast, 'M:d|M:**',
        'swapping declaration order flips the tie-break winner';
}

{
    # A strictly longer candidate still wins on length (not a tie).
    grammar G3 {
        token TOP { <x> }
        proto token x {*}
        token x:sym<any> { \w+ }          # matches the whole thing (longer)
        token x:sym<a>   { <sym> }        # shorter
    }
    class Act3 {
        method TOP($/) { make $<x>.ast }
        method x:sym<any>($/) { make 'ANY' }
        method x:sym<a>($/)   { make 'A' }
    }
    is G3.parse('abc', :actions(Act3)).ast, 'ANY',
        'longest-token wins regardless of declaration order';
}

{
    # Fall-through only wins when the specific candidates do not match.
    grammar G4 {
        token TOP { <x> }
        proto token x {*}
        token x:sym<foo> { <sym> }
        token x:sym<any> { \w+ }
    }
    class Act4 {
        method TOP($/) { make $<x>.ast }
        method x:sym<foo>($/) { make 'FOO' }
        method x:sym<any>($/) { make 'ANY' }
    }
    is G4.parse('bar', :actions(Act4)).ast, 'ANY',
        'fall-through candidate wins when the specific one does not match';
}
