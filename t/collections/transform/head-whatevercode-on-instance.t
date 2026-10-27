use Test;

# `Any.head(&callable)` is `self.list.head(&callable)`: an instance that is not
# itself a list reads the end-relative count from its own one-element list, and
# a user `multi method head(Int)` only adds candidates. (Found with Red's
# `ResultSeq`, whose `multi method head(UInt(Numeric) $num)` made
# `$rs.head(*-5)` die "none of these signatures matches".)

plan 8;

class Plain { }
is-deeply Plain.new.head(*-1).list, (), 'a plain instance: head(*-1) is empty';
is Plain.new.head(*-0).elems, 1, 'head(*-0) is the whole one-element list';
is Plain.new.head(2).elems, 1, 'head(2)';

class WithMulti {
    multi method head(Int $n) { "user $n" }
}
is WithMulti.new.head(2), 'user 2', 'the user candidate wins what it accepts';
is-deeply WithMulti.new.head(*-1).list, (), 'a call no user candidate accepts reaches Any.head';

class Rows does Sequence {
    has @.l;
    method iterator { @!l.iterator }
    multi method head(UInt(Numeric) $num) { "limit $num" }
    multi method head() { 'first' }
}
my $rows = Rows.new(:l<a b c>);
is $rows.head(2), 'limit 2', 'Sequence instance: the numeric candidate';
is $rows.head, 'first', 'Sequence instance: the nullary candidate';
is-deeply $rows.head(*-1).list, <a b>, 'Sequence instance: head(*-1) reaches the inherited head';

done-testing;
