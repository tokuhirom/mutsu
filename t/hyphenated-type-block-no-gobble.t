use v6;
use Test;

# A declared hyphenated type name (class/role/etc.) followed by whitespace and a
# `{ ... }` block must NOT gobble the block as a call argument. The block belongs
# to the enclosing construct (if/when/given). Only a hyphenated bareword that is
# NOT a declared type — a forward-referenced sub — gobbles a block.
# Regression: `$test ~~ TAP::Sub-Test { ... }` used to parse `Sub-Test { ... }`
# as a Call and fail with "Confused / needs '{'".

plan 8;

class Foo-Bar { }

my $five = 5;

# smartmatch against a declared hyphenated type; block is the if-body
if $five ~~ Foo-Bar {
    flunk "5 should not smartmatch Foo-Bar";
} else {
    pass "if-block belongs to the if, not gobbled by Foo-Bar";
}

if Foo-Bar.new ~~ Foo-Bar {
    pass "Foo-Bar instance smartmatches Foo-Bar";
} else {
    flunk "Foo-Bar.new should smartmatch Foo-Bar";
}

# given/when with a declared hyphenated type; block is the when-body
my $branch = 'none';
given $five {
    when Foo-Bar { $branch = 'matched' }
    default      { $branch = 'default' }
}
is $branch, 'default', "when Foo-Bar block is the when-body";

given Foo-Bar.new {
    when Foo-Bar { $branch = 'matched' }
    default      { $branch = 'default' }
}
is $branch, 'matched', "when Foo-Bar matches an instance";

# `my $x = Type ~~ Type { ... }` inside parens still resolves the block correctly
my $r = do {
    if $five ~~ Foo-Bar { 'yes' } else { 'no' }
};
is $r, 'no', "nested if with hyphenated type resolves block";

# Qualified hyphenated type reference (short name registered under a module)
role Baz-Role { }
class Impl does Baz-Role { }
if Impl.new ~~ Baz-Role {
    pass "hyphenated role name does not gobble the block";
} else {
    flunk "Impl should do Baz-Role";
}

# A forward-referenced hyphenated sub still gobbles a block argument.
my $ran = 0;
run-it { $ran = 1 };
is $ran, 1, "forward-referenced hyphenated sub still gobbles its block arg";
sub run-it(&b) { b() }

# A declared hyphenated sub with a block argument keeps working too.
sub do-thing(&b) { b() }
my $done = 0;
do-thing { $done = 42 };
is $done, 42, "declared hyphenated sub gobbles its block arg";
