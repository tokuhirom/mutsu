use v6;
use Test;

# Three edges of the associative multi-dim subscript AS AN LVALUE, all measured
# against rakudo v2026.06.
#
#  1. Under 6.d `%h{1;2}` is a MULTISLICE: the lvalue is a one-element `List`,
#     so it is always defined and always true and `//=`/`||=` can never store —
#     and must not autovivify the path either. The same short-circuit rule
#     applies to a single subscript whose read is defined only because of an
#     `is default(...)`.
#  2. A `Whatever` key in an associative ASSIGNMENT is refused; mutsu used to
#     stringify it into a literal `"*"` key, a silent write nothing detects.
#  3. `postcircumfix:<{; }>` has only `(\SELF, @indices)` and
#     `(\SELF, @indices, :$exists!)` before 6.e, so `:delete` does not resolve.

plan 23;

# --- 1. a short-circuiting compound assignment does not store when it keeps --
{
    my %h;
    %h{1;2} //= 7;
    is-deeply %h, {}, '//= on a multislice stores nothing';
}
{
    my %h;
    %h{1;2} ||= 7;
    is-deeply %h, {}, '||= on a multislice stores nothing';
}
{
    my %o;
    %o<i>{1;2} //= 7;
    is-deeply %o, {}, 'and neither does the chain-rooted spelling';
}
{
    my %h;
    %h{1;2} = 5;
    %h{1;2} //= 7;
    is %h<1><2>, 5, '//= keeps an existing leaf';
}
{
    my %h;
    %h{1;2} += 7;
    is %h<1><2>, 8, '+= still stores (the multislice numifies to 0)';
}
{
    my @a;
    @a[1;2] //= 7;
    is @a[1][2], 7, 'the POSITIONAL multi-dim lvalue is a plain element and stores';
}

# ... including the single-subscript spelling, where an `is default` makes the
# read defined without the key existing.
{
    my %h is default(9);
    %h<a> //= 7;
    is-deeply %h, {}, '//= through an `is default` hash creates no key';
}
{
    my %h is default(9);
    %h<a> ||= 7;
    is-deeply %h, {}, 'and neither does ||=';
}
{
    my @a is default(9);
    @a[3] //= 7;
    is-deeply @a, [], 'nor an `is default` array';
}
{
    my %h;
    %h<a> //= 7;
    is %h<a>, 7, 'a genuinely undefined element still stores';
}
{
    my %h;
    %h<a> = 1;
    %h<a> &&= 7;
    is %h<a>, 7, '&&= stores when the LHS is true';
}
{
    my %h;
    %h<a> &&= 7;
    is-deeply %h, {}, 'and stores nothing when it is not';
}

# --- 2. a Whatever key in an associative assignment is refused --------------
{
    my %h;
    throws-like { %h{*} = 5 }, X::AdHoc, 'assigning through a Whatever key throws';
    is-deeply %h, {}, 'and writes nothing';
}
{
    my %h = a => 1, b => 2;
    my $err;
    { %h{*} = 5; CATCH { default { $err = .message } } }
    like $err, /'order of keys is non-deterministic'/, 'the message names the reason';
}
{
    my %h = a => 1;
    is-deeply (%h{*}).List, (1,), 'a Whatever READ is unaffected';
}
{
    my @a = 1, 2, 3;
    @a[*] = 7, 8, 9;
    is-deeply @a, [7, 8, 9], 'and so is the positional `@a[*] = ...`';
}
{
    my %h;
    %h<*> = 5;
    is %h<*>, 5, 'a literal "*" KEY is still an ordinary key';
}

# --- 3. `:delete` on an associative multi-dim subscript does not resolve ----
{
    my %h;
    %h{1;2} = 5;
    throws-like { %h{1;2}:delete }, X::Multi::NoMatch,
      'multi-dim :delete has no candidate under 6.d';
    is %h<1><2>, 5, 'and the leaf is untouched';
}
{
    my %h;
    %h{1;2} = 5;
    throws-like { %h{1;2}:delete(0) }, X::Multi::NoMatch,
      'the adverb VALUE cannot rescue it -- resolution fails first';
}
{
    my @a;
    @a[1;2] = 5;
    is @a[1;2]:delete, 5, 'the POSITIONAL multi-dim :delete is valid';
}
{
    my %h;
    %h<a> = 1;
    is %h<a>:delete, 1, 'and so is a single-subscript :delete';
}
