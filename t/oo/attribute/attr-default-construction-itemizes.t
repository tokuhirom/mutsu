use Test;

# #9040: a `$`-sigil attribute whose declaration carries an Array/Hash
# *default* held it un-itemized. A `$` attribute IS a Scalar container, so
# what it holds should render with the itemizing `$` prefix, exactly as the
# same value assigned through the accessor does (#9023). The construction
# path that evaluates a `has $.x = <default>` initializer and seeds the
# attribute cell is not reachable from `assign_method_lvalue_with_values`, so
# the #9023 fix did not cover it -- and there are several such construction
# sites (the ordinary `.new` fast path with no BUILD, the interpreter's
# pre-BUILD fill, the post-BUILD deferred pass, and `.bless`), each of which
# is pinned below.

plan 8;

class NoBuild { has $.w is rw = {a => 1} }
is NoBuild.new.w.raku, '${:a(1)}',
    'a Hash default is itemized through the native no-BUILD construction fast path';

class NoBuildArr { has $.w is rw = [1, 2, 3] }
is NoBuildArr.new.w.raku, '$[1, 2, 3]',
    'an Array default is itemized through the native no-BUILD construction fast path';

# A method-store default (`$!w`, read back through an `is rw` method) goes
# through the same construction-time seed, not the accessor-generated store.
class MethodAccessor { has $!w = {a => 1}; method w is rw { $!w } }
is MethodAccessor.new.w.raku, '${:a(1)}',
    'a Hash default on a `$!`-declared attribute is itemized at construction';

# A BUILD phase routes the default through the post-BUILD deferred pass
# (`apply_post_build_attr_defaults`) instead of the pre-BUILD fill.
class WithBuild { has $.w is rw = {a => 1}; submethod BUILD { } }
is WithBuild.new.w.raku, '${:a(1)}',
    'a Hash default is itemized through the post-BUILD deferred pass';

# `.bless` runs its own default-fill loop (`dispatch_new_and_constructors`),
# separate from `.new`.
class Blessed { has $.w is rw = {a => 1} }
is Blessed.bless.w.raku, '${:a(1)}',
    'a Hash default is itemized through .bless';

# `@`/`%` attributes are containers in their own right and must NOT itemize.
class Cont { has @.a = 1, 2, 3; has %.h = (e => 5); }
my $c = Cont.new;
is $c.a.raku, '[1, 2, 3]', 'an `@` attribute default stays un-itemized';
is $c.h.raku, '{:e(5)}', 'a `%` attribute default stays un-itemized';

# A scalar (non-container) default is unaffected.
class Plain { has $.n is rw = 42 }
is Plain.new.n.raku, '42', 'a plain Int default is unchanged';
