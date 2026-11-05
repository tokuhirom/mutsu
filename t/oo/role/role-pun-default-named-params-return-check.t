use Test;
plan 6;

role Base[::T = Any, :$flag = 1, :$g = Int] {
    has $.value;
    multi method fs('' --> Base) { ::?CLASS }
    multi method fs(::?CLASS:U: Str $in --> Base) { self.new(value => $in) }
    method mk(--> Base) { ::?CLASS }
}

# Called on the role itself: the invocant is the pun of the default curry,
# whose name carries the named parameters' defaults.
lives-ok { Base.fs("b") }, 'a --> Role check accepts the role pun`s own instance';
is Base.fs("b").value, "b", 'and returns it';
ok Base.fs('') === Base.mk, 'a type-object return is accepted too';
ok Base.fs("c") ~~ Base, 'the instance smartmatches the bare role';

class IntT does Base[Int] {}
is IntT.fs("a").value, "a", 'a class that does the parameterization is unaffected';
is IntT.mk.^name, 'IntT', '::?CLASS names the class';
