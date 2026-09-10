use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# lastcall empties the deferral chain, so a following nextsame finds nothing, the
# call yields Nil, and neither the wider candidate nor post-nextsame code runs.

plan 2;

my @ev;
class D {
    multi method m(Int $x) { @ev.push("Int"); lastcall; nextsame; @ev.push("after") }
    multi method m(Any $x) { @ev.push("Any"); "d-any" }
}
my $r = D.new.m(1);
ok !$r.defined, "nextsame after lastcall returns Nil";
is @ev.join("|"), "Int", "lastcall truncates the chain; nothing after runs";
