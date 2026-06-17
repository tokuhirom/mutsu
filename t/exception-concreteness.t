use Test;

plan 9;

# fail/die/throw/rethrow/resume on an Exception *type object* require a concrete
# invocant -> X::Parameter::InvalidConcreteness (not "no such method").

for <fail die throw rethrow resume> -> $meth {
    throws-like 'X::AdHoc.' ~ $meth, X::Parameter::InvalidConcreteness,
        should-be-concrete => 'True',
        param-is-invocant  => 'True',
        routine            => $meth;
}

# A concrete exception instance still throws/carries its message.
my $caught;
{ X::AdHoc.new(payload => "boom").throw; CATCH { default { $caught = .message } } }
is $caught, 'boom', 'thrown instance carries message';

# die/fail as plain functions are unaffected.
my $d;
{ die "func die"; CATCH { default { $d = .message } } }
is $d, 'func die', 'die function still works';

# A non-exception type object stays X::Method::NotFound (not InvalidConcreteness).
throws-like 'Int.throw', X::Method::NotFound, 'non-exception type object stays NotFound';

# A user-declared Exception subclass type object is also caught.
throws-like 'class MyErr is Exception {}; MyErr.throw', X::Parameter::InvalidConcreteness,
    'user Exception subclass type object';
