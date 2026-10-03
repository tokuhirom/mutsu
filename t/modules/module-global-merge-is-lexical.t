use v6;
use lib 't/lib';
use Test;
use MONKEY-SEE-NO-EVAL;

# A module's own package-scope declarations (classes, `our` subs, constants,
# packages) are merged into the lexical scope that ran its `need`/`use`, not
# into the whole program, and not transitively into an importer of a module
# that `use`d it (ADR-11136, #11136). Every expectation here was measured
# against Rakudo.

plan 17;

sub undeclared(Str $code) {
    try EVAL $code;
    so $! ~~ X::Undeclared::Symbols | X::NoSuchSymbol
        || ($! && $!.message.contains('Could not find symbol'));
}

my $obj;
my &closure;
{
    need MergeScope::Outer;
    is MergeOuterCls.v, 'outer', 'a class is visible in the block that needed its module';
    is merge-outer-our(), 'outer-our', 'so is an `our sub`';
    is MERGE-OUTER-C, 5, 'and a constant';
    is MergeOuterPkg::f(), 'pkg-f', 'and a package';
    is ::('MergeOuterCls').^name, 'MergeOuterCls', 'and an indirect lookup';
    ok undeclared('MergeInnerCls'), 'what the module itself used is not merged here';
    ok undeclared('merge-inner-our()'), 'nor its dependency\'s `our sub`';
    $obj = MergeOuterCls.new;
    &closure = { MergeOuterCls.v };
}

ok undeclared('MergeOuterCls'), 'the class is not visible after the block';
ok undeclared('merge-outer-our()'), 'nor the `our sub`';
ok undeclared('MERGE-OUTER-C'), 'nor the constant';
ok undeclared('MergeOuterPkg::f()'), 'nor the package';

is $obj.v, 'outer', 'an instance made in the block still works after it';
is $obj.mk.v, 'inner', 'and its methods still see what the module merged';
is closure(), 'outer', 'a closure made in the block keeps the merge';

sub in-sub { need MergeScope::Outer; MergeOuterCls.v }
is in-sub(), 'outer', 'a `need` in a sub body merges into that body';
ok undeclared('MergeOuterCls'), 'and not into the caller';

{
    need MergeScope::Outer;
    is MergeOuterCls.v, 'outer', 'a later block that needs the module sees it again';
}
