use Test;

# An `IO::Path:D:` method called on the IO::Path *type object* dies with
# X::Parameter::InvalidConcreteness (as in Rakudo), not X::Method::NotFound
# or a generic Cool/Any method (#9570).

plan 13;

for <e d f s r w x l basename lines words> -> $m {
    throws-like { IO::Path."$m"() }, X::Parameter::InvalidConcreteness,
        "IO::Path.$m on the type object",
        message => /"Invocant of method '" $m "' must be an object instance of type" \s+ "'IO::Path'," \s+ "not a type object of type 'IO::Path'"/;
}

# `Nil.IO` returns the IO::Path type object (#9495), so `.IO.e` reaches this.
throws-like { Nil.IO.e }, X::Parameter::InvalidConcreteness, 'Nil.IO.e';

# Methods that answer on the type object still do; instances are unaffected.
is IO::Path.IO.^name, 'IO::Path', 'IO::Path.IO still answers on the type object';
