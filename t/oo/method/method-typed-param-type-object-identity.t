use Test;

plan 7;

# A typed readonly parameter of a METHOD is bound straight to the argument, so
# `$p =:= TypeObject` holds when the argument is that type object, as for a sub
# (#16959). Cooklang's `parts(CookBase :$split-by?)` gates on it.
enum E <a b>;
role R { }
class K {
    method en(E :$s?) { $s =:= E }
    method ro(R :$s?) { $s =:= R }
    method it(Int :$s?) { $s =:= Int }
    method pos(Int $s) { $s =:= Int }
    method un($s) { $s =:= Int }
    method cp(Int $s is copy) { $s =:= Int }
}
ok K.new.en, 'enum-typed optional named, not supplied';
ok K.new.ro, 'role-typed optional named, not supplied';
ok K.new.it, 'Int-typed optional named, not supplied';
ok K.new.pos(Int), 'Int-typed positional bound to the type object';
nok K.new.un(Int), 'untyped parameter holds a Scalar';
nok K.new.cp(Int), 'is copy parameter holds a Scalar';
nok K.new.en(s => a), 'a supplied value is not the type object';
