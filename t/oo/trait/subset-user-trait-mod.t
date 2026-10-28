use Test;

# #15250 (found via Red `is sub-model`): `subset S is foo of T where ...` calls a
# user `trait_mod:<is>` with the subset's type object, and a `$subset.HOW does
# Role` it performs persists on the subset's metaobject.

plan 10;

my @seen;
role Extra { method all(\model, |c) { "all-called(" ~ model.^refinee.^name ~ ")" } }

multi trait_mod:<is>($subset where { .HOW ~~ Metamodel::SubsetHOW }, Bool :$extra) {
    @seen.push("extra:" ~ $subset.^name);
    $subset.HOW does Extra;
    $subset
}
multi trait_mod:<is>($subset where { .HOW ~~ Metamodel::SubsetHOW }, :$tag!) {
    @seen.push("tag:" ~ $tag.join("+") ~ ":" ~ $subset.^name);
}

subset Adm is extra of Int where * > 3;
is @seen.grep(*.starts-with("extra:")).join(","), "extra:Adm", "the trait ran once";
like Adm.HOW.^name, /'SubsetHOW+{Extra}'/, "the role is mixed into the subset's HOW";
is Adm.^all, "all-called(Int)", "a method from the mixed-in role is callable";
ok 5 ~~ Adm, "the subset still checks its predicate";
nok 2 ~~ Adm, "... and rejects";

subset A is tag("x") of Int where * > 0;
subset B of Int is tag(<p q>) where * > 0;
{ my subset C is tag(3) of Int; }
subset D is tag("e") is export of Int;
is-deeply @seen.grep(*.starts-with("tag:")).sort.List,
    ('tag:e:D', 'tag:x:A', 'tag:p+q:B', 'tag:3:C').sort.List,
    "arguments reach the trait; traits may follow `of`; `my subset` and `is export` work";
ok 5 ~~ A && 5 ~~ B, "subsets with traits still type-check";

throws-like 'subset Z is nosuch of Int;', X::Inheritance::UnknownParent,
    "an unknown trait is still an error";

# `.AST` keeps the traits.
my $ast = Q[subset Q is tag("x") of Int].AST;
like $ast.raku, /'Trait::Is'/, ".AST has a Trait::Is";
like $ast.raku, /'Trait::Of'/, "... and the Trait::Of base";
