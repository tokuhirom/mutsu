use Test;

class K { }
for <a b> -> $n {
    K.^add_method: "m$n", my method () { -> { "c$n" } }
}

sub h($n) {
    my $x = K.new.ma;
    $x.()
}

is h('zz'), 'ca', 'inner closure reads the lexical captured by the method';

done-testing;
