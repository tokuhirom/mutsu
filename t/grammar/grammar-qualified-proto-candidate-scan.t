use v6;
use Test;

# Resolving a `<subrule>` walks every registered token key to collect the
# rule's `:sym<>` proto candidates, merging an ancestor grammar's candidates
# through the MRO and deduplicating by candidate identity. That scan was
# rewritten to work off the interned key symbols instead of materializing a
# String per key (#7576); these cases pin the semantics it has to keep —
# derived-adds-to-base, MRO dedup, the declaration-order LTM tie-break, and
# the `<Pkg::rule>` qualified form that resolves through its own copy of the
# scan. All expectations verified against rakudo v2026.07.

plan 8;

{
    grammar Base {
        proto token item {*}
        token item:sym<num> { \d+ }
        token item:sym<word> { \w+ }
    }
    grammar Deriv is Base {
        token item:sym<bang> { '!' }
    }

    ok Deriv.parse('!', :rule<item>).defined,
        'a derived grammar candidate matches';
    ok Deriv.parse('12', :rule<item>).defined,
        'an inherited proto candidate still matches in the derived grammar';
    nok Base.parse('!', :rule<item>).defined,
        'the base grammar is not extended by its descendant';

    grammar Qual {
        rule TOP { <Base::item> }
    }
    ok Qual.parse('99').defined, 'a <Pkg::rule> reference resolves';
    is Qual.parse('99')<Base::item>.Str, '99',
        'the qualified reference captures under its written name';
    nok Qual.parse('!').defined,
        'the qualified reference sees only the named package candidates';
}

{
    # An equal-length tie between an inherited candidate and a derived one is
    # broken by declaration order, and the derived grammar still reaches its
    # own candidate when the base one cannot match.
    grammar B2 {
        proto token pp {*}
        token pp:sym<ab> { <sym> }
    }
    grammar D2 is B2 {
        token pp:sym<ax> { 'a' \w }
    }
    class A2 {
        method pp:sym<ab>($/) { make 'BASE' }
        method pp:sym<ax>($/) { make 'DERIVED' }
    }
    is D2.parse('ab', :rule<pp>, :actions(A2)).ast, 'DERIVED',
        'the later-declared derived candidate wins the equal-length tie';
    is D2.parse('az', :rule<pp>, :actions(A2)).ast, 'DERIVED',
        'the derived candidate matches where the inherited one cannot';
}
