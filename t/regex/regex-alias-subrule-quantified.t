use Test;

plan 6;

# `<alias=subrule>` captures under BOTH names, so a quantified alias makes the
# subrule's own name a LIST too — YAMLish reads its `%TAG` directives back as
# `@<tag-directive>».ast.list`, which needs an empty list (not a bare Match)
# when the alternative that names it never ran.
grammar G {
    token TOP { [ '%' [ <version=vd> | <tags=td> ] \n ]+ }
    token vd { 'YAML' ' ' $<version>=[ \d+ '.' \d+ ] }
    token td { 'TAG' ' ' $<h>=[ '!' \w* '!' ] }
}

my $both = G.parse("%YAML 1.1\n%TAG !yaml!\n");
ok $both, 'both directive kinds parse';
is $both<vd>.elems, 1, 'the aliased subrule name is a list under a quantifier';
is $both<td>.elems, 1, 'for the other alternative too';
is ~$both<version>[0]<version>, '1.1', 'and the alias still resolves';

my $only-version = G.parse("%YAML 1.1\n");
is $only-version<vd>.elems, 1, 'the alternative that ran captures one element';
is $only-version<td>.elems, 0, 'the one that never ran captures an empty list';
