use Test;

plan 6;

# `when TYPE` reads a Proxy topic by value, as `~~` does: a pointy block
# parameter bound to an `is rw` Proxy result is `given`'s topic.
role AST { }
class Item does AST { has $.v }
sub mk() is rw {
    my $o = Item.new(:v(1));
    Proxy.new(FETCH => -> $ { $o }, STORE => -> $, $v { })
}

my &one = -> $value {
    given $value { when AST { 'AST' }; default { 'default' } }
};
is one(mk()), 'AST', 'pointy block, role type';

my &two = -> $, $value {
    given $value { when Item { 'Item' }; default { 'default' } }
};
is two(1, mk()), 'Item', 'pointy block with an anonymous first parameter, class type';

sub plain($value) { given $value { when AST { 'AST' }; default { 'default' } } }
is plain(mk()), 'AST', 'sub parameter';

my $seen;
class Holder {
    method AT-KEY($k) is rw {
        Proxy.new(
            FETCH => -> $ { Item.new(:v(2)) },
            STORE => -> $, $value {
                $seen = do given $value { when AST { 'store AST' }; default { 'store default' } };
            });
    }
}
my $h = Holder.new;
$h<a> = $h<b>;
is $seen, 'store AST', 'the value a Proxy STORE receives from another Proxy';

my $x := mk();
my $r = do given $x { when Item { 'bound Item' }; default { 'default' } };
is $r, 'bound Item', 'a Proxy-bound variable as the topic';
is (do given mk() { when Str { 'Str' }; when AST { 'AST' }; default { 'default' } }), 'AST',
    'later when clauses see the fetched topic';
