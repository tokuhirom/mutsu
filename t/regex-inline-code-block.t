use Test;

plan 13;

# A plain `{ … }` block inside a regex runs INLINE, left-to-right, during the
# match (raku semantics) — not deferred until the match has finished. Its writes
# to in-regex `:my`/`:let` lexicals are therefore visible to the atoms that come
# after it in the same match.

# 1. A `<?{ … }>` assertion sees a `:my`-declared lexical.
grammar AssertSeesMy {
    token TOP { :my $x = 'y'; <?{ $x eq 'y' }> 'z' }
}
ok AssertSeesMy.parse('z').defined, 'code assertion reads an in-regex :my lexical';

# 2. An inline block writes the lexical; a later assertion sees the new value.
grammar BlockThenAssert {
    token TOP { :my $x = 'n'; { $x = 'y' } <?{ $x eq 'y' }> 'z' }
}
ok BlockThenAssert.parse('z').defined, 'inline block write is visible to a later assertion';

# 3. An inline block writes the lexical; a later interpolation matches its value.
grammar BlockThenInterp {
    token TOP { :my $x = 'n'; { $x = 'ab' } $x 'c' }
}
ok BlockThenInterp.parse('abc').defined, 'inline block write is visible to a later interpolation';
nok BlockThenInterp.parse('nc').defined, 'the pre-block value no longer matches';

# 4. The block may sit inside a lookahead — the lookaround consumes nothing, but
#    the write it performed is real. This is how YAMLish measures block indent.
grammar IndentProbe {
    token TOP {
        :my $indent;
        <?before $<sp>=[ ' '* ] { $indent = ~$<sp> }>
        $indent 'x'
    }
}
ok IndentProbe.parse('x').defined,     'lookahead-computed indent, empty';
ok IndentProbe.parse('   x').defined,  'lookahead-computed indent, three spaces';

# 5. The lexical also reaches a subrule ARGUMENT expression.
grammar ArgFromMy {
    token TOP {
        :my $indent;
        <?before $<sp>=[ ' '* ] { $indent = ~$<sp> }>
        $indent <tail($indent)>
    }
    token tail(Str $ind) { 'a' [ "\n" $ind 'a' ]* }
}
ok ArgFromMy.parse("  a\n  a").defined, ':my lexical passed as a subrule argument';

# 6. A `:my` lexical belongs to ITS regex. A subrule is a different regex, so it
#    resolves the same name against its own enclosing scope, not the caller's.
{
    my $shared = 'q';
    grammar NoLeakIntoSubrule {
        token TOP { :my $shared = 'zz'; <inner> }
        token inner { $shared }
    }
    # Pre-existing gap (also fails before this change): the caller's `:my` keeps
    # the subrule's own pre-substitution from resolving the outer lexical, so the
    # subrule interpolates nothing at all. The important half — that the caller's
    # VALUE does not leak in — holds either way.
    todo 'caller :my suppresses the subrule\'s own outer-lexical substitution';
    ok NoLeakIntoSubrule.parse('q').defined,     'a subrule sees the outer lexical, not the caller regex\'s :my';
    nok NoLeakIntoSubrule.parse('zz').defined,   'the caller regex\'s :my value does not leak into the subrule';
}

# 7. Running blocks inline must not disturb the two things that already worked:
#    a plain block's side effect on an outer variable, and `make`.
my @log;
grammar SideEffect {
    token TOP { 'q' { @log.push('hit') } }
}
SideEffect.parse('q');
is @log.join(','), 'hit', 'a plain block still has its outer side effect, exactly once';

grammar Made {
    token TOP { <n> { make $<n>.made * 2 } }
    token n { (\d+) { make +$0 } }
}
is Made.parse('21').made, 42, 'make still reduces bottom-up over the match tree';

# 8. The deferral split is on what a block NEEDS from the reduce walk. A `:my $*x`
#    dynamic variable is one binding per match, installed and read back around
#    each node's reduce step, so a block that writes one must stay deferred.
grammar PerMatchDynvar {
    token TOP { <part>+ % ',' }
    token part { :my $*V = 'decl'; \w+ [ <?before ','> { $*V = 'set' } ]? { make $*V } }
}
is PerMatchDynvar.parse('a,b')<part>.map(*.ast).join('|'), 'set|decl',
    'a block writing a per-match dynamic variable still reduces per match';

# 9. ...while a block writing a plain OUTER lexical still reaches the caller's slots.
my $seen;
'123' ~~ / (\d) { $seen = $/.Str } \d+ /;
is $seen, '1', 'an inline block still writes back to a caller lexical';
