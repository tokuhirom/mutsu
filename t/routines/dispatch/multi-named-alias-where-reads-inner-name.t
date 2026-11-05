use Test;

plan 5;

# `:op(@case) where @case[0] eq 'case'`: the `where` reads the renamed inner
# variable, which must hold the argument while candidates are matched.
multi ec(:op(@) (Str $op where 'like'|'not-like', $l, $r, $e)) { "like" }
multi ec(:@and) { "and" }
multi ec(:op(@case) where @case[0] eq 'case') { "case:" ~ @case.elems }
multi ec(:op(@) (Str $key, $l, $r)) { "bin" }

my @warnings;
{
    CONTROL { when CX::Warn { @warnings.push: .message; .resume } }
    is ec(|\(:op['case', 'L', ('no' => 42), 13])), 'case:4', 'the where sees the inner variable';
    is ec(|\(:op['like', 'L', 'x', 'y'])), 'like', 'an earlier candidate still wins';
    is ec(|\(:op['=', 'L', 'x'])), 'bin', 'a later candidate after a failed where';
}
is @warnings.elems, 0, 'no uninitialized-value warning while matching';

sub plain(:v(@x) where @x.elems > 1) { 'many' }
is plain(:v[1, 2]), 'many', 'a single-candidate sub works the same';
