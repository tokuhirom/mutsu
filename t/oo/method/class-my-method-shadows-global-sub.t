use Test;

plan 3;

sub mk() {
    only STORE(Mu) { 'global-sub' }
    1
}
mk();

class Holder {
    my method STORE($x) { 'class-method' }
    method pick { &STORE.(self, 1) }
}

is Holder.pick, 'class-method', '&name inside a method is the class\'s own my method';
is mk(), 1, 'the unrelated sub is still callable';

class Binder is Proxy {
    has $!value is default(Nil);
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $topic is raw --> Nil) { $!value = $topic }
}
my $b := Binder.new;
$b = 7;
is $b, 7, 'a Proxy subclass built from its own my methods stores through them';
