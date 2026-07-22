use v6;
use Test;

plan 6;

# A tied hash whose AT-KEY throws must propagate the exception on read,
# not swallow it and return Nil (Hash::Agnostic's NYI stubs rely on this).
{
    role R does Associative {
        method AT-KEY($k) { die "stub NYI" }
        method keys() { () }
    }
    class C does R {}
    my %h is C;
    dies-ok { my $x = %h<foo> }, 'tied AT-KEY throw propagates on read';
}

# A non-Associative instance with no AT-KEY: `<foo>` subscript stays Nil
# (the error is only propagated when the class actually has an AT-KEY).
{
    class Plain { has $.x }
    my $obj = Plain.new(x => 5);
    lives-ok { my $y = $obj<foo> }, 'plain instance subscript does not die';
}

# A real tied hash whose AT-KEY returns Nil for a missing key stays Nil,
# does not error.
{
    role Backed does Associative {
        has %!h;
        method AT-KEY($k) { %!h.AT-KEY($k) }
        method keys() { %!h.keys }
        submethod BUILD() { %!h = (a => 1, b => 2) }
    }
    class MyHash does Backed {}
    my %m is MyHash;
    is %m<a>, 1, 'tied read of present key';
    is %m<z>.defined, False, 'tied read of missing key is undefined, not an error';
}

# throws-like matches a user exception's computed `.message` method, not just
# stored attributes.
{
    my class X::MyErr is Exception {
        has $.what;
        method message() { "No implementation of $.what method found." }
    }
    throws-like { X::MyErr.new(:what<AT-KEY>).throw },
        X::MyErr,
        :what<AT-KEY>,
        :message(/ "AT-KEY" /);
    pass 'throws-like message matcher reached';
}
