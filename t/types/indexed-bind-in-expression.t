use Test;

# An indexed-element bind (`%h<k> := v`, `@a[i] := v`) must work in expression
# context, not only as a statement. Previously the expression-level `:=` handler
# only bound simple sigil variables (`$x := v`), leaving indexed lvalues to the
# statement-level handler — which is absent when the bind is the RHS of a
# declaration/assignment. So
#   my $commit = %!commits{$sha1} := Git::Commit.new: ...;   # Git::Blame::File
# failed to parse. The bind is now handled in expression context too.

plan 11;

{
    my %c;
    my $c = %c<k> := 5;
    is $c, 5, 'my $x = %h<k> := v returns the bound value';
    is %c<k>, 5, 'and the hash element is bound';
}

{
    my @a;
    my $c = @a[0] := 7;
    is $c, 7, 'my $x = @a[i] := v returns the bound value';
    is @a[0], 7, 'and the array element is bound';
}

{
    # Chained bind on an indexed lvalue with a method-call RHS (the exact
    # Git::Blame::File shape).
    my class G { has $.n }
    my %commits;
    my $key = 'sha1';
    my $commit = %commits{$key} := G.new(n => 42);
    is $commit.n, 42, 'my $x = %h<k> := Class.new(...) binds and returns';
    is %commits{$key}.n, 42, 'the hash element holds the same object';
}

{
    # Plain-variable bind in expression context still works (no regression).
    my $a;
    my $c = $a := 9;
    is $c, 9, 'plain-variable bind in expression context still works';
}

{
    # Statement-level indexed bind still works (no regression).
    my %c;
    %c<k> := 3;
    is %c<k>, 3, 'statement-level indexed bind still works';
}

# The expression-context fast path must NOT shadow the statement-level handler's
# special handling of slice binds, Whatever-index binds, and non-variable targets.
{
    my @a = 1, 2, 3;
    @a[0, 1] := 4, 5;
    is ~@a, '4 5 3', 'a slice bind (@a[0,1] := ...) still distributes';
}

throws-like { my @a = 1, 2, 3; @a[*-1] := 42 }, X::Bind::Slice,
    'a Whatever-index bind is still X::Bind::Slice';

throws-like '(1,2)[0] := 3', X::Bind,
    'a bind into a non-variable target is still X::Bind';
