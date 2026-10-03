use Test;

plan 3;

# An accessor `.wrap`ped with an `is rw` role method obtained through
# `.^find_method` (Staticish's singleton wrappers) hands back the attribute's
# container, so assigning through the type object stores into the instance.
role W {
    method _rw_wrapper($self: |c) is rw {
        my $new-self = $self;
        if not $new-self.defined { $new-self = $self.new }
        callwith($new-self, |c);
    }
}
class B {
    has Str $.foo is rw;
    my $i;
    method new(|c) { $i //= self.bless(|c) }
}
B.HOW does W;
B.^find_method('foo').wrap(B.HOW.^find_method('_rw_wrapper'));

lives-ok { B.foo = 'x' }, 'assignment through the wrapped accessor lives';
is B.foo, 'x', 'the type object reads the stored value';
is B.new.foo, 'x', 'it was stored in the singleton instance';
