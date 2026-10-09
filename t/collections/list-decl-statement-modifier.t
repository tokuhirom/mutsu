use Test;

plan 8;

# A declarator list carrying a statement modifier declares its variables
# unconditionally; only the assignment is gated (#12385).
sub f($c) {
    my ($w, @wb) = (1, 2, 3) if $c;
    (@wb.elems, $w.raku);
}
is-deeply f(0), (0, 'Any'), 'list declaration with false `if` still declares';
is-deeply f(1), (2, '1'), 'list declaration with true `if` assigns';

sub g($c) {
    my ($w, $z) := (1, 2) if $c;
    $z.raku;
}
is g(0), 'Any', 'bind form with false `if` still declares';
is g(1), '2', 'bind form with true `if` initializes';

sub h($c) {
    my ($w, %h) = (1, a => 2) unless $c;
    %h.raku;
}
is h(0), '{:a(2)}', '`unless` with false condition assigns';
is h(1), '{}', '`unless` with true condition only declares';

my ($p, $q) = (7, 8) if True;
is $p, 7, 'file-scope first element';
is $q, 8, 'file-scope second element';
