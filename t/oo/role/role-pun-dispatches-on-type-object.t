use Test;

plan 12;

# Punning a role by *constructing* an instance runs the role's own `new` with
# no arguments. A role whose `new` takes a required parameter therefore lost
# the arguments of every *other* method called on the pun. raku puns onto the
# class's type object and never constructs at all.

role Plain {
    method new(Int $size) { self.bless }
    method other(Int $size) { "other:$size" }
    method no-args() { 'no-args' }
}

is Plain.other(3), 'other:3',
    'a role with a required-argument `new` still passes arguments to its other methods';
is Plain.no-args, 'no-args', 'and to its no-argument methods';
is Plain.other(4), 'other:4', 'and again once the role has been punned';

role Param[::T] {
    method new(Int $size) { self.bless }
    method other(Int $size) { "other:{$size}:{T.^name}" }
}

is Param[Int].other(3), 'other:3:Int',
    'the same holds for a parameterised role pun';

# A role that does not declare its own `new` keeps working.
role NoNew {
    method other(Int $size) { "plain:$size" }
}
is NoNew.other(3), 'plain:3', 'a role without a custom `new` still dispatches';

role ParentCallable {
    multi method CALL-ME(::?ROLE:U:) { 'no-arg' }
    multi method CALL-ME(::?ROLE:U: \v) { 'arg:' ~ v }
}
is ParentCallable.(), 'no-arg', 'a parent role itself puns to a callable type object';
role ChildCallable does ParentCallable { }
is ChildCallable.(), 'no-arg', 'a composed role pun matches a parent role type object';
is ChildCallable.(3), 'arg:3', 'its parent multi candidates are not duplicated';

role AttributeRole {
    has $.value = 5;
    method read-value() { $!value }
}
throws-like { AttributeRole.read-value }, Exception,
    'a role pun does not construct instance attribute storage';

# `.new` on a role still builds an instance through the role's own constructor.
role WithNew {
    has Int $.size;
    method new(Int $size) { self.bless(:$size) }
    method describe() { 'described' }
}
my $built = WithNew.new(7);
ok $built.defined, '.new on a role still constructs';
is $built.size, 7, 'and it runs the role-provided constructor';
is $built.describe, 'described', 'the instance dispatches role methods too';
