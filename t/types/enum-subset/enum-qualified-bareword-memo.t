use Test;

# A package-qualified enum key read as a bareword (`Order::Less`) is
# remembered per site for one registry generation (#15786: Array::Sorted::Util
# compares `cmp` results against `Order::Less`/`Order::More` in a loop). The
# memo must answer exactly what a fresh resolution would.

plan 9;

sub cmp-name($a, $b) {
    my $o := $a cmp $b;
    $o === Order::Less ?? "less" !! $o === Order::More ?? "more" !! "same"
}
is (^6).map({ cmp-name($_, 3) }).join(","), "less,less,less,same,more,more",
    'a loop over Order::Less / Order::More';

enum Color <Red Green Blue>;
sub pick($i) { ($i %% 2) ?? Color::Green !! Color::Blue }
is (^4).map(&pick).map(*.key).join(","), "Green,Blue,Green,Blue", 'a user enum';
ok pick(0) === Color::Green, 'the same value as a fresh read';
is pick(1).value, 2, 'with its value';

module M { enum Size <Small Large> }
sub size() { M::Size::Large }
is (size() xx 3).map(*.key).join(","), "Large,Large,Large", 'a package-nested enum';

sub later() { EVAL 'Shade::Dark' }
EVAL 'enum Shade <Light Dark>';
is later().key, "Dark", 'an enum declared after the reading routine';

{
    my enum Local <One Two>;
    sub local-two() { Local::Two }
    is local-two().value, 1, 'a lexical enum';
}

is (Bool::True xx 3).join(","), "True,True,True", 'Bool::True';
ok (^3).map({ Order::Same }).all === Order::Same, 'Order::Same in a closure';
