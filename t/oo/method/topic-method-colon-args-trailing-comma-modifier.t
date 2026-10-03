use Test;

plan 5;

# A topic method call in colon form (`.method: a, b`) takes the same argument
# list as `$obj.method: a, b`: a trailing comma before a statement modifier or
# a closing bracket is an empty slot (HTML::Component's
# `.label: :for($id), $text, unless $param.?no-label;`).
class Rec {
    has @.got;
    method add(*@a, *%n) { @!got.push: (|@a, |%n.sort.map(*.kv).flat).join('|'); self }
}

my $r = Rec.new;
given $r {
    .add: :for(1), 'x', unless False;
    .add: 'skipped', unless True;
    .add:
        'a',
        'b',
    if True;
}
is-deeply $r.got, ['x|for|1', 'a|b'], 'trailing comma before a statement modifier';

given Rec.new {
    my @l = (.add: 1, 2,);
    is-deeply .got, ['1|2'], 'trailing comma before a closing paren';
}

given 'ab' {
    ok (.contains: 'a' and .contains: 'b'), 'each argument still stops at a loose `and`';
}

given Rec.new {
    .add: 1, 2 ... 4;
    is-deeply .got, ['1|2|3|4'], 'a sequence is one argument, as in `$obj.method:`';
}

given Rec.new {
    .add: :a :b(2);
    is-deeply .got, ['a|True|b|2'], 'adjacent colonpairs without commas';
}
