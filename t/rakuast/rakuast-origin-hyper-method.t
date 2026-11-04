use Test;

# `@a>>.name` and `@a».name`: the `MetaPostfix::Hyper` runs from the `>>` to
# the end and holds the `Call::Method`, which starts at its name, with its
# `Name` and `ArgList` (ADR-10723 Stage 2, #15780). Ranges measured on
# rakudo 2026.09.

plan 4;

sub ranges(Str $src, *@classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq @classes.any {
            with $node.origin { @r.push: "$name {.from}-{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.sort.join(', ')
}

my @classes = 'MetaPostfix::Hyper', 'Call::Method', 'Name', 'ArgList';

is ranges('my @a = (1,2); say @a>>.name;', |@classes),
    'ArgList 19-28, ArgList 28-28, Call::Method 24-28, MetaPostfix::Hyper 21-28, Name 15-18, Name 24-28',
    'a hyper method call';
is ranges("my @a = (1,2); say @a\x[BB].name;", |@classes),
    'ArgList 19-27, ArgList 27-27, Call::Method 23-27, MetaPostfix::Hyper 21-27, Name 15-18, Name 23-27',
    'with the guillemet spelling';
is ranges('my @a = (1,2); my @b = @a>>.abs;', 'MetaPostfix::Hyper', 'Call::Method'),
    'Call::Method 28-31, MetaPostfix::Hyper 25-31',
    'in an initializer';
is ranges('my @a = (1,2); say @a>>.fmt("%d");', 'MetaPostfix::Hyper', 'Call::Method'),
    'Call::Method 24-33, MetaPostfix::Hyper 21-33',
    'with arguments';
