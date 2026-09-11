use v6;
use Test;

# `require Foo:ver(...)` / `require Foo:ver<...>` — the distribution selectors
# refine WHICH distribution is loaded, exactly as they do on `use`. They are not
# import tags and not part of the module's name; without consuming them,
# `require CSS::Grammar:ver(v0.3.3+)` (CSS::Module::CSS3::Selectors' t/00basic.t)
# parsed as a call to an undeclared routine `CSS::Grammar:ver`.

plan 5;

lives-ok { EVAL 'require Test:ver(v0.0.1+)' }, 'parenthesized :ver selector';
lives-ok { EVAL 'require Test:ver<0.0.1+>' }, 'angle-bracket :ver selector';
lives-ok { EVAL 'require Test:ver<0.0.1+>:auth<perl>' }, 'stacked :ver / :auth selectors';
lives-ok { EVAL 'require Test:v<0.0.1+>' }, ':v is the short spelling of :ver';

# The selectors ride on the resolution, not on the name: the package `require`
# installs and returns is still the bare one.
is EVAL('(require Test:ver<0.0.1+>).^name'), 'Test',
    'the installed package keeps its bare name';
