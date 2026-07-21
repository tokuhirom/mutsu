use Test;

# A chained binding `$a := $b := VALUE` must work even when a middle term is an
# indexed lvalue (`%h<k>`, `%h{"k"}`). Regression: the scalar `:=` statement
# parsed its RHS with a plain expression parser that stopped at the second `:=`,
# so `$c := %h<k> := 5` failed ("Confused"). The other assignment branches all
# use the assign-aware RHS parser; the bind branch now does too.
# (Fasta binds `$current := %labels{.substr(1)} := my str @`.)

plan 6;

{
    my %h; my $c;
    $c := %h<k> := 5;
    is %h<k>, 5, "chained bind, middle angle-key lvalue: hash updated";
    is $c, 5, "chained bind, middle angle-key lvalue: scalar bound";
}

{
    my %h; my $c;
    $c := %h{"j"} := 9;
    is %h{"j"}, 9, "chained bind, middle brace-key lvalue";
}

{
    # Both terms indexed.
    my %h;
    %h<a> := %h<b> := 7;
    is (%h<a>, %h<b>).join(","), "7,7", "chained bind, both terms indexed";
}

{
    # Fasta shape: bind a scalar to a hash element bound to a fresh native array.
    my %labels; my $current;
    $current := %labels<lbl> := my str @;
    $current.push("x");
    is %labels<lbl>, ["x"], "chained bind to a fresh anonymous native array (Fasta)";
}

{
    # Plain scalar chain still works (no regression).
    my $a; my $b;
    $a := $b := 42;
    is "$a $b", "42 42", "plain chained scalar bind still works";
}
