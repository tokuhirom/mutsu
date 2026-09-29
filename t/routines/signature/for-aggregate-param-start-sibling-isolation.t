use Test;

# ADR-0023 follow-up: `@`/`%` for-loop parameters are fresh per-iteration
# bindings, so sibling `start` blocks must each see their own iteration's
# container rather than the last one bound.

plan 3;

my $l = Lock.new;

{
    my @out; my @ps;
    for [1,2],[3,4] -> @c {
        @ps.push: start { sleep 0.02; $l.protect({ @out.push: @c.sum }) }
    }
    await @ps;
    is-deeply @out.sort.List, (3, 7), '@ loop parameter is per-iteration in start';
}

{
    my @out; my @ps;
    for {a=>1},{b=>2} -> %h {
        @ps.push: start { sleep 0.02; $l.protect({ @out.push: %h.keys.join }) }
    }
    await @ps;
    is-deeply @out.sort.List, <a b>, '% loop parameter is per-iteration in start';
}

{
    my @out; my @ps;
    for [1,2],[3,4],[5,6],[7,8] -> @a, @b {
        @ps.push: start { sleep 0.02; $l.protect({ @out.push: (@a.sum, @b.sum).join(",") }) }
    }
    await @ps;
    is-deeply @out.sort.List, <11,15 3,7>, 'multi @ loop parameters are per-iteration in start';
}
