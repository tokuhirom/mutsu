use Test;

# An `IO::Path:D:` method called on the IO::Path *type object* dies with
# X::Parameter::InvalidConcreteness (as in Rakudo), not X::Method::NotFound
# or a generic Cool/Any method (#9570).

plan 19;

for <e d f s r w x l basename lines words> -> $m {
    throws-like { IO::Path."$m"() }, X::Parameter::InvalidConcreteness,
        "IO::Path.$m on the type object",
        message => /"Invocant of method '" $m "' must be an object instance of type" \s+ "'IO::Path'," \s+ "not a type object of type 'IO::Path'"/;
}

# `Nil.IO` returns the IO::Path type object (#9495), so `.IO.e` reaches this.
throws-like { Nil.IO.e }, X::Parameter::InvalidConcreteness, 'Nil.IO.e';

# Methods that answer on the type object still do; instances are unaffected.
is IO::Path.IO.^name, 'IO::Path', 'IO::Path.IO still answers on the type object';

# Numeric *context* (not an explicit method call) on the IO::Path type object
# dies with the same X::Parameter::InvalidConcreteness, rather than warning
# and coercing to 0 like a plain `Any`-ish type object would (#9629).
throws-like { IO::Path + 1 }, X::Parameter::InvalidConcreteness,
    'IO::Path + 1',
    message => /"Invocant of method 'Numeric' must be an object instance of type" \s+ "'IO::Path'," \s+ "not a type object of type 'IO::Path'"/;
throws-like { +IO::Path }, X::Parameter::InvalidConcreteness, '+IO::Path';
throws-like { IO::Path * 2 }, X::Parameter::InvalidConcreteness, 'IO::Path * 2';
throws-like { -IO::Path }, X::Parameter::InvalidConcreteness, '-IO::Path';

# `Str + 1` still only warns (Str has no `:D:`-only Numeric): unaffected.
{
    my $did-warn = False;
    my $result;
    CONTROL { when CX::Warn { $did-warn = True; .resume } }
    $result = Str + 1;
    ok $did-warn, 'Str + 1 still warns instead of dying';
    is $result, 1, 'Str + 1 still resumes with 1 (Str coerces to 0)';
}
