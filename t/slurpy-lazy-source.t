use Test;

# A single lazy/infinite source bound to a `*@` slurpy stays lazy (ANALYSIS §F
# lazy-seq ④ / lazy-arrays.md L4). Previously `flatten_into_slurpy` dropped a
# `Value::LazyList` (a `.map` pipeline) in as ONE element, so `@x[0]` returned the
# whole Seq gist instead of the first element.

plan 6;

# A lazy .map pipeline through a slurpy: elements reify on index, stays lazy.
{
    sub f(*@x) { @x[^3].join(',') }
    is f((1..Inf).map(* * 2)), '2,4,6', 'lazy .map pipeline flattens through *@ slurpy';
}
{
    sub g(*@x) { @x.is-lazy }
    ok g((1..Inf).map(* * 2)), '*@ slurpy of a lazy .map pipeline is lazy';
}

# A bare infinite range through a slurpy: stays lazy, reifies on index.
{
    sub h(*@x) { @x[0] }
    is h(1..Inf), 1, '*@ slurpy of an infinite range reifies element 0';
}
{
    sub i(*@x) { @x.is-lazy }
    ok i(1..Inf), '*@ slurpy of an infinite range is lazy';
}

# An infinite source returned from a call, then slurped, indexes correctly.
{
    sub src { (1..Inf).map(* * 2) }
    sub j(*@x) { @x[^3].join(',') }
    is j(src()), '2,4,6', 'slurpy of a returned lazy Seq indexes correctly';
}

# The lazy slurpy gists as a placeholder rather than hanging.
{
    sub k(*@x) { @x.gist }
    is k(1..Inf), '[...]', 'lazy slurpy gist is a placeholder (no hang)';
}
