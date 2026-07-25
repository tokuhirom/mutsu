use Test;

plan 6;

# A rule's `:my $*FOO` is one binding PER MATCH of that rule, not one shared by
# the whole parse. mutsu established a single parse-wide slot, so the last code
# block to write it won for every action that read it.

{
    grammar G {
        token TOP { <part>+ % '/' }
        token part {
            :my $*FINAL;
            \w+ {}
            [<?before '/'? $> { $*FINAL = True }]?
        }
    }
    class A {
        method TOP($/) { make $<part>.map(*.ast).join('|') }
        method part($/) { make ($*FINAL ?? "FIN" !! "mid") ~ ":$/" }
    }
    is G.parse('a/b/c', :actions(A)).ast, 'mid:a|mid:b|FIN:c',
        'only the segment whose lookahead fired sees $*FINAL set';
    is G.parse('solo', :actions(A)).ast, 'FIN:solo',
        'a single segment is still the final one';
}

{
    # The declared initializer, not a sibling's leftover, is what a match starts
    # from — verified without `make` in the loop.
    grammar G2 {
        token TOP { <part>+ % ',' }
        token part { :my $*V = 'decl'; \w+ [ <?before ','> { $*V = 'set' } ]? }
    }
    my @seen;
    class A2 { method part($/) { @seen.push($*V) } }
    G2.parse('a,b', :actions(A2));
    is @seen, ['set', 'decl'],
        'each match of the declaring rule carries its own value into its action';
}

{
    # An inline `{ … }` block in the same rule reads its own match's binding too.
    grammar G3 {
        token TOP { <part>+ % ',' }
        token part { :my $*V = 'decl'; \w+ [ <?before ','> { $*V = 'set' } ]? { make $*V } }
    }
    is G3.parse('a,b').<part>.map(*.ast).join('|'), 'set|decl',
        'an inline block sees its own match binding';
}

{
    # A dynamic variable declared in an OUTER rule still accumulates across the
    # inner matches — the per-match binding must not reset a parent's.
    grammar G4 {
        token TOP { :my %*PLAYED = (); <card>+ % ' ' }
        token card { \w+ }
    }
    class A4 {
        method card($/) { %*PLAYED{~$/}++ }
        method TOP($/) { make %*PLAYED.keys.sort.join(',') }
    }
    is G4.parse('a b c', :actions(A4)).ast, 'a,b,c',
        'an outer rule declaration still accumulates across inner matches';
}

{
    # A rule that declares nothing is unaffected.
    grammar G5 {
        token TOP { :my $*T = 'top'; <part>+ % ',' }
        token part { \w+ }
    }
    my @seen;
    class A5 { method part($/) { @seen.push($*T) } }
    G5.parse('a,b', :actions(A5));
    is @seen, ['top', 'top'],
        'a non-declaring rule reads the enclosing declaration';
}
