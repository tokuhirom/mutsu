use Test;

# The type-keyed multi-method resolution cache keys a ContainerRef argument
# (a variable a closure captured) by its contents (#9494). It must still
# pick by the current contents on every call, and an `is rw` candidate must
# still accept the captured variable.

plan 4;
class F { }
class R {
    has @.got;
    multi method push (F $f)   { @!got.push: 'F' }
    multi method push (Cool $c) { @!got.push: 'Cool' }
    multi method push (R $r)   { @!got.push: 'R' }
}
my $r = R.new;
my $v = F.new;
my sub keep () { $v }          # captures $v, boxing it into a shared cell
for ^2 {
    $r.push($v);
    $v = 42;
    $r.push($v);
    $v = R.new;
    $r.push($v);
    $v = F.new;
}
is-deeply $r.got, [<F Cool R F Cool R>], 'a captured argument dispatches on its current contents';
class W {
    multi method m (Int $x is rw) { $x = 99; 'rw' }
    multi method m (Str $x) { 'str' }
}
my $n = 1;
my sub cap () { $n }
is W.new.m($n), 'rw', 'is rw candidate still binds a captured variable';
is $n, 99, 'and writes through to it';
is W.new.m('s'), 'str';
