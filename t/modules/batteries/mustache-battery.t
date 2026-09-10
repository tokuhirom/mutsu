use Test;

# Template::Mustache is bundled (BATTERIES.md, docs/batteries/templates.md), so
# it must load and work with a plain `use` -- no `-I`, no install. This pins the
# zero-config resolution and a smoke slice of the API; the exhaustive behaviour
# check is the release-time gate that runs the full upstream suite
# (scripts/battery-testsuite.sh).

plan 10;

use Template::Mustache;

is Template::Mustache.render('Hello {{name}}!', { name => 'World' }),
    'Hello World!', 'a variable is interpolated';

is Template::Mustache.render('{{a}}', { a => '<b>' }), '&lt;b&gt;',
    '{{ }} HTML-escapes its value';

is Template::Mustache.render('{{{a}}}', { a => '<b>' }), '<b>',
    '{{{ }}} interpolates raw';

is Template::Mustache.render('{{#items}}[{{.}}]{{/items}}', { items => [1, 2, 3] }),
    '[1][2][3]', 'a section iterates a list with the implicit iterator';

is Template::Mustache.render('{{#a}}yes{{/a}}', { a => False }), '',
    'a falsy section is skipped';

is Template::Mustache.render('{{^a}}no{{/a}}', { a => False }), 'no',
    'an inverted section renders when the value is falsy';

is Template::Mustache.render('{{a.b}}', { a => { b => 'deep' } }), 'deep',
    'a dotted name walks into a nested hash';

# The section pushes its own value onto the context stack, so the dotted name
# `b.c` inside `{{#a}}` resolves against `a`'s value (whose `b` is empty) and
# must NOT fall back to the outer `b`. mutsu used to render 'ERROR' here
# because entering the section did not push the new frame -- the release-time
# battery gate caught it, but nothing in the ordinary test suite did.
is Template::Mustache.render(
        '{{#a}}{{b.c}}{{/a}}',
        { a => { b => {} }, b => { c => 'ERROR' } }),
    '', 'a dotted name resolves against the section context, not the outer one';

is Template::Mustache.render(
        '{{#a}}{{b.c}}{{/a}}',
        { a => { b => { c => 'inner' } }, b => { c => 'outer' } }),
    'inner', 'the section context wins over the enclosing one';

is Template::Mustache.render('{{>part}}!', {}, :from({ part => 'PARTIAL' })),
    'PARTIAL!', 'a partial is resolved from :from';
