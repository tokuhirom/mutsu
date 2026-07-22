use Test;

plan 8;

# `=begin pod ... =end pod` yields a Pod::Block::Named named "pod", not a bare
# Pod::Block (RakupodObject T-043).
{
    my $doc = q:to/POD/;
    =begin pod
    Hello
    =end pod
    POD
    use MONKEY-SEE-NO-EVAL;
    my $pod;
    EVAL($doc ~ "\n\$pod = \$=pod[0]");
    isa-ok $pod, Pod::Block::Named, 'pod block is Pod::Block::Named';
    ok $pod ~~ Pod::Block, 'pod block is-a Pod::Block';
    is $pod.name, 'pod', 'named block name is "pod"';
}

# `=begin pod :attr<x>` carries config through to the named block.
{
    my $doc = q:to/POD/;
    =begin pod :status<draft>
    Hi
    =end pod
    POD
    use MONKEY-SEE-NO-EVAL;
    my $pod;
    EVAL($doc ~ "\n\$pod = \$=pod[0]");
    is $pod.config<status>, 'draft', 'pod block config is captured';
}

# `:key<>` (empty angle brackets) is a fatal Pod config error in Raku.
{
    use MONKEY-SEE-NO-EVAL;
    dies-ok {
        EVAL("=begin pod\n=begin code :lang<>\nx\n=end code\n=end pod\n\$=pod[0]");
    }, 'empty <> colonpair in pod config dies';

    dies-ok {
        EVAL("=begin pod :foo<>\nx\n=end pod\n\$=pod[0]");
    }, 'empty <> colonpair on the pod directive dies';
}

# A space inside the angle brackets is a valid (non-empty) value, not an error.
{
    use MONKEY-SEE-NO-EVAL;
    lives-ok {
        EVAL("=begin pod\n=begin code :lang< >\nx\n=end code\n=end pod\n\$=pod[0]");
    }, ':key< > (space) is a valid value, not empty';
}

# Named blocks other than "pod" keep their own name.
{
    my $doc = q:to/POD/;
    =begin foo
    Bar
    =end foo
    POD
    use MONKEY-SEE-NO-EVAL;
    my $pod;
    EVAL($doc ~ "\n\$pod = \$=pod[0]");
    is $pod.name, 'foo', 'other named blocks keep their name';
}
