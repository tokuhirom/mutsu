use Test;

# The `$.n` on the left of a statement-level compound assignment carries the
# range Rakudo gives it (ADR-10723 Stage 2, #15780), like a lexical or `$!n`
# does. Ranges measured on rakudo 2026.09.

plan 3;

sub range-of(Str $src, Str $class) {
    my @r;
    sub walk($node) {
        if $node.^name eq "RakuAST::$class" {
            with $node.origin { @r.push: "{.from}..{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(',')
}

is range-of('class A { has $.n is rw; method m { $.n += 100 } }', 'Var::Attribute::Public'),
    '36..39', 'the public attribute of `$.n += 100`';
is range-of('class B { has $.s is rw; method m { $.s ~= "z" } }', 'Var::Attribute::Public'),
    '36..39', 'the same for `~=`';
is range-of('class C { has $!n; method m { $!n += 1 } }', 'Var::Attribute'),
    '30..33', 'a private attribute keeps its range';
