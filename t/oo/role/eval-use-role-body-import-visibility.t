use Test;

# #14481: a role body run at composition calls a routine its own module
# imported, although the composing class sits in an EVAL string that itself
# `use`s the module (Native::Packing's README sample), also when the EVAL runs
# inside a block another routine calls (Test::lives-ok, PDF::Content).
use lib 't/lib';

plan 3;

my $code = q:to/CODE/;
    use EvalRoleImportRole;
    my class LD does EvalRoleImportRole { }
    LD.new.h
    CODE

is EVAL($code), 42, 'EVAL that uses the role module and composes it';

sub call-it(&c) { c() }
is call-it({ EVAL $code }), 42, 'the same EVAL inside a block called by a routine';

lives-ok { EVAL $code }, 'and inside lives-ok';
