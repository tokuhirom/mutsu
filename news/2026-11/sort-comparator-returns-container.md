# `sort` reads a container-returning comparator's value

Since a Block returns the container of its last statement (#17094), a comparator such as
`-> $a, $b { my $r = $a cmp $b; $r }` hands back `$r`'s cell. `sort_result_to_ordering`
only matched `Int`/`Bool`/`Order`, so the cell fell to `Equal` and nothing was sorted:
Template::Jinja2's `sort(attribute=...)` filter returned its input order (t/13, t/14,
t/15, t/17, t/18). It now reads the value inside the cell first. All of Template::Jinja2's
20 test files give `raku`'s `ok`/`not ok` lines.
