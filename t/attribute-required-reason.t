use v6;
use Test;

# `is required("reason")` weaves the custom reason into the
# X::Attribute::Required message; a bare `is required` uses the plain form.
# (raku-doc/doc/Type/Attribute.rakudoc)

plan 5;

class D { has $.a is required("it is a good idea") }
try D.new;
my $reasoned = $!;
is $reasoned.^name, 'X::Attribute::Required', 'reasoned: right exception type';
is $reasoned.Str,
    "The attribute '\$!a' is required because it is a good idea,\nbut you did not provide a value for it.",
    'reasoned message includes "because <reason>," and the newline';

class E { has $.a is required }
try E.new;
my $bare = $!;
is $bare.^name, 'X::Attribute::Required', 'bare: right exception type';
is $bare.Str,
    "The attribute '\$!a' is required, but you did not provide a value for it.",
    'bare message uses the plain form';

# Providing the value avoids the exception entirely.
lives-ok { D.new(a => 1) }, 'providing the required attribute lives';
