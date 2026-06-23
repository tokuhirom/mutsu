use Test;

# X::Str::Sprintf::Directives::Count message faithfulness: rakudo pluralizes
# BOTH the directive count and the supplied count independently, renders 0
# supplied args as "no argument was", echoes the format after a newline, and
# appends the interpolated-'$' hint when fewer args were supplied than needed.

plan 12;

throws-like 'sprintf("%s", 1, 2, 3)', X::Str::Sprintf::Directives::Count,
    'too many args',
    args-used => 1, args-have => 3,
    message => /'specify 1 argument, but 3 arguments were' \n 'supplied to format'/;

throws-like 'sprintf("%s %s %s", 1)', X::Str::Sprintf::Directives::Count,
    'too few args carries the interpolated-$ hint',
    args-used => 3, args-have => 1,
    message => /'specify 3 arguments, but 1 argument was' \n 'supplied' .* "interpolated '\$'"/;

throws-like 'sprintf("%s")', X::Str::Sprintf::Directives::Count,
    'zero args renders "no argument was"',
    args-used => 1, args-have => 0,
    message => /'but no argument was'/;

throws-like 'sprintf("abc", 1, 2)', X::Str::Sprintf::Directives::Count,
    'zero directives still pluralizes "0 arguments"',
    args-used => 0, args-have => 2,
    message => /'specify 0 arguments,'/;

# The format string is echoed verbatim.
throws-like 'sprintf("<%d|%d>", 5)', X::Str::Sprintf::Directives::Count,
    'format is echoed in the message',
    message => /"format '<\%d|\%d>'."/;

# No hint when extra args are supplied (have > used).
{
    my $e;
    try { sprintf("%s", 1, 2); CATCH { default { $e = $_ } } }
    nok $e.message.contains("interpolated"), 'no hint when too many args';
    ok  $e.message.contains("\n"),           'message wraps before "supplied"';
}

# Plain integers still format fine (no false positive).
is sprintf("%d-%d", 1, 2), '1-2', 'matching arity formats normally';
is sprintf("%s", "ok"),    'ok',  'single directive single arg';
is sprintf("%%"),          '%',   'literal percent needs no args';
is sprintf("%d %d %d", 1, 2, 3), '1 2 3', 'three directives three args';
is sprintf("no directives here"), 'no directives here', 'no directives no args';
