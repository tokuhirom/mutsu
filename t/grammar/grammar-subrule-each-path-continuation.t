use Test;

# A subrule call hands the caller's continuation every path the callee takes
# to an end, even when two paths end at the same position: Rakudo runs what
# follows the call once per path (#10489). Both engines (the tree walk and the
# compiled VM) used to keep only the first path to each end.

plan 4;

{
    my @log;
    grammar D {
        regex TOP { <d> { @log.push('t') } 'x' }
        regex d { a || a || ab }
    }
    nok D.parse('ab'), 'the parse fails';
    is @log.elems, 3, 'the continuation ran once per path, a repeated end included';
}

{
    my @seen;
    grammar E {
        regex TOP { <d> { @seen.push($<d><w> ?? ~$<d><w> !! '-') } 'x' }
        regex d { $<w>=[a] || $<w>=[a] <?> || ab }
    }
    E.parse('ab');
    is @seen.join(','), 'a,a,-', 'each path to the same end reaches the continuation';
}

{
    grammar F {
        regex TOP { <d> 'b' }
        regex d { a || a }
    }
    is ~F.parse('ab')<d>, 'a', 'a successful parse is unchanged';
}
