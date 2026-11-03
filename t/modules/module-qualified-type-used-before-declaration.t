use Test;
use lib 't/lib';
use PostDeclaredQualified;

# A qualified type name used before its declaration is not rakudo's
# "Illegally post-declared type": it is looked up when the code runs. The
# module below uses `X::PostDeclaredQualified::Late` in a method, declared
# further down the file (Directory 0.0.10 does this with
# X::Directory::FileExists); loading it must not fail. (Whether the class is
# found when the method runs is a separate question rakudo answers at run time.)

plan 2;

my $o = PostDeclaredQualified.new;
is $o.fine, 'fine', 'a module that names a later qualified class loads';

# A single-segment name is still a lexical type: a use before its declaration
# is an error.
throws-like { EVAL 'say Zlate.new; class Zlate { }' }, X::Undeclared::Symbols,
    'a plain name used before its declaration is refused';
