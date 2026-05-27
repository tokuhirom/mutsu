use Test;

plan 4;

# Test: grammar action methods fire inline during parsing,
# allowing dynamic variable changes to affect subsequent tokens.
{
    my $*X = 'aa';

    grammar G {
        regex TOP { <first> <second> }
        token first { $*X }
        token second { $*X }
    }

    class A {
        method first($/) {
            $*X = 'bb';
        }
        method second($/) { }
    }

    my $actions = A.new;
    my $m = G.parse('aabb', :$actions);
    ok $m.defined, 'grammar parse with inline action dynvar change succeeds';
    is ~$m, 'aabb', 'full match text is correct';
    is ~$m<first>, 'aa', 'first subrule matched aa';
    is ~$m<second>, 'bb', 'second subrule matched bb (after dynvar change)';
}
