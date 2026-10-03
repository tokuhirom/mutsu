use Test;

# Binding an element to the `given`/`with` topic that aliases that same
# element (`%h<k> := $_ with %h<k>`) is a no-op in Raku. mutsu's topic
# writeback stored the topic's shared container cell back through the
# element's own cell, which then contained itself, and the next read
# overflowed the stack. Reduced from the PURL distribution, whose
# canonicalization does `%args<name> := $_ with canonicalize(%args<name>)`.

plan 9;

{
    my %h = k => 1;
    given %h<k> { %h<k> := $_ }
    is %h<k>, 1, 'given %h<k> { %h<k> := $_ } leaves the value intact';
    %h<k> = 5;
    is %h<k>, 5, 'the element is still assignable afterwards';
}

{
    my %j = k => 1;
    %j<k> := $_ with %j<k>;
    is %j<k>, 1, 'statement-modifier with form';
}

{
    my @a = 1, 2;
    @a[0] := $_ with @a[0];
    is-deeply @a, [1, 2], 'array element form';
}

{
    my %h = k => 2;
    given %h<k> { %h<k> := $_; $_ = 9 }
    is %h<k>, 9, 'assigning the topic after the bind reaches the element';
}

{
    my %g = k => 1;
    my $r = do given %g<k> { %g<k> := $_; 7 };
    is "%g<k> $r", '1 7', 'do given expression form';
}

{
    sub canon($x) { $x }
    sub f(%args) {
        %args<name> := $_ with canon(%args<name>);
        %args<subpath>
    }
    my %a = name => 'io';
    nok f(%a).defined, 'first call: unrelated key stays absent';
    nok f(%a).defined, 'second call on the same hash: unrelated key stays absent';
    is %a<name>, 'io', 'the bound key keeps its value';
}
