unit class Leak::Sub::Table;
# A multi-parameter `for` nested in another `for`: its parameters have no
# enclosing declaration, so their slots are the loop's own.
my @rows = 1, 2;
for @rows {
    for <a b>.kv -> $i, $var { }
}
