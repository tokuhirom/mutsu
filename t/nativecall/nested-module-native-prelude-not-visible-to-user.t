# A prelude splice is lexical to the compunits it was spliced into.
#
# mutsu injects `nativecast`/`nativesizeof`/`cglobal`/`explicitly-manage`/
# `refresh` as an `our sub` prelude into every compunit whose source mentions
# NativeCall (`inject_nativecall_subs_prelude`), and registers each under
# `GLOBAL::` rather than the host package so a method body running under ANY
# package can call it (see `NATIVECALL_SUB_PRELUDES`). That registration is
# process-global, which used to make the helper resolvable from every scope in
# the process once any module anywhere had pulled it in -- so `use`ing a module
# that itself uses the native-call module left `&nativecast` declared in the
# using scope, where rakudo leaves it undeclared (GH #7612).
#
# The registration still has to be process-global; only its VISIBILITY is now
# lexical: `prelude_declaring_units` records which compunits each splice went
# into, and routine resolution consults the compunit that is executing
# (`prelude_visible_here`).
#
# IMPORTANT: the splice gate is a source-TEXT check over this file's code
# (comments and Pod are stripped first, `CodeText::from_source`). Spelling any
# of the five helper names -- or the module's name -- in *code* here, even
# inside a string, would inject the prelude into this compunit and legitimately
# declare the names. So every name below is assembled at runtime.
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

plan 5;

my $cast = '&native' ~ 'cast';
my $sizeof = '&native' ~ 'sizeof';

# `BlockUseNestedOuter` -> `BlockUseNestedInner` -> the native-call module.
# Nothing in this file's code names it, so the helpers must not be declared
# here.
use BlockUseNestedOuter;

nok defined(::($cast)),
    "a nested module's own prelude splice is not visible to the using scope";
nok defined(::($sizeof)),
    'the same for the other helpers of that splice';

# ...while the module that actually declared it still sees its own copy. This is
# the direction GH #7580 fixed and must not regress.
is outer-probe(), 'visible',
   "the declaring module's own routines still resolve the helper";

# A block-scoped `use` of the same chain: same on both counts.
{
    use BlockUseNestedInner;
}
nok defined(::($cast)),
    'a block-scoped use of the chain does not leak the helper either';
is outer-probe(), 'visible',
   'and the chain still resolves after the block-scoped use';
