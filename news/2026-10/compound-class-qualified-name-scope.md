# A compound class name is not a lookup scope for qualified type names

Inside a file-scope `class LLM::Chat::Template::Jinja2`, the type name
`Template::Jinja2` resolved to the class being declared: the outward type
lookup re-derived `LLM::Chat` as an enclosing scope from the compound name and
found `LLM::Chat::Template::Jinja2` before the imported GLOBAL
`Template::Jinja2`. A guard already kept compound-name segments from being
scopes for *unqualified* names; qualified names bypassed it, because real
nesting (`module NL { class Inner::Thing }`) must still find
`NL::Inner::Thing`.

The registry now records, for each compound-declared package, the package it
was really declared in, and the lookup walks those real enclosing scopes:
`Foo::Bar` declared at file scope goes straight to GLOBAL after itself, while a
class declared inside `NL` still visits `NL`. This makes LLM::Chat's Jinja2
chat-template wrapper (`has Template::Jinja2 $!env`) type-check, so its
`t/10_jinja2.rakutest` passes.
