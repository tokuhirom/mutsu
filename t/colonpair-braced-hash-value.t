use Test;

plan 2;

{
    my $pair = :h{:42foo};
    is-deeply $pair.value, {:42foo}, 'braced hash literal is preserved in colonpair value';
}

{
    my class Foo { has %.h is rw };
    my $o := Foo.new: :h{:42foo};
    is-deeply ($o.h ,= :100bar), {:42foo, :100bar}, 'comma meta-assign on rw hash accessor keeps hash semantics';
}
