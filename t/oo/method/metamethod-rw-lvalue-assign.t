use Test;

# From the RedFactory distribution: `Type.^model = $model` where `^model` is a
# user-declared `is rw` metamethod inherited from a parent class.
plan 3;

class F { method ^model($f) is rw { $ }; method ^hi($f) { "hi" } }
class G is F { }
G.^model = 5;
is G.^model, 5, 'assignment to an inherited rw metamethod lvalue';

my \T = Metamodel::ClassHOW.new.new_type: :name("TF");
T.^add_parent: F;
T.^compose;
is T.^hi, 'hi', 'metamethod resolves on a minted type';
T.^model = 7;
is T.^model, 7, 'assignment through a minted type';
