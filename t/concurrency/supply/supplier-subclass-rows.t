use Test;

# An instance of a user subclass of `Supplier` is a supplier: it carries a
# supplier's state from construction, and `Supplier`'s rows answer its
# built-in methods (ADR-11276 §9.133). Answers checked against Rakudo.

plan 8;

class Tagged is Supplier {
    has $.tag = 'x';
    method shout($v) { self.emit($v.uc) }
}

{
    my $s = Tagged.new(tag => 'y');
    is $s.tag, 'y', 'a declared attribute';
    my @got;
    my $done = False;
    $s.Supply.tap({ @got.push($_) }, done => { $done = True });
    $s.shout('a');
    $s.emit('b');
    my %h = s => $s;
    %h<s>.emit('c');
    $s.done;
    is-deeply @got, ['A', 'b', 'c'], 'emit from a user method, a variable and a hash value';
    ok $done, 'done reaches the tap';
    isa-ok $s, Supplier, 'it is a Supplier';
}

{
    my $r = Tagged.new;
    my @w;
    react {
        whenever $r.Supply { @w.push($_); done if $_ == 2 }
        $r.emit(1);
        $r.emit(2);
    }
    is-deeply @w, [1, 2], 'a whenever over its Supply';
}

{
    my $q = Tagged.new;
    my $message;
    $q.Supply.tap(quit => { $message = .message });
    $q.quit('oops');
    is $message, 'oops', 'quit reaches the tap';
}

class KeepIt is Supplier::Preserving { }
{
    my $p = KeepIt.new;
    $p.emit($_) for 1..3;
    $p.done;
    is-deeply $p.Supply.list, (1, 2, 3), 'a Supplier::Preserving subclass replays';
}

ok Tagged.new.can('emit'), '.can finds the row';
