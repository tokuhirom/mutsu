use Test;

plan 6;

# A routine whose name is a string literal (`sub ::("h")`), a lexical method
# (`my method`), and a method-level proto declared outside a package no
# longer keep a nested BEGIN of their scope (and every later nested BEGIN) in
# place: they are copied into the lifted BEGIN like a plain `my sub`
# (ADR-0134, #13096). None of the enclosing routines is called before its
# value is checked, so each value was stored by the lifted BEGIN alone.

my ($computed, $after-computed, $method, $after-method, $after-proto);

sub with-computed-name { sub ::("computed-helper") { 'c' }; BEGIN $computed = computed-helper() }
is $computed, 'c', 'a routine named by a string literal';

sub after-computed { BEGIN $after-computed = 'lifted' }
is $after-computed, 'lifted', 'a computed-name routine no longer keeps later BEGINs in place';

sub with-lexical-method { my method lm() { 'm' ~ self }; BEGIN $method = 7.&lm }
is $method, 'm7', 'a lexical method called through its code object';

sub after-method { BEGIN $after-method = 'lifted' }
is $after-method, 'lifted', 'a lexical method no longer keeps later BEGINs in place';

sub with-method-proto { my proto method mp(|) {*}; BEGIN 1 }
sub after-proto { BEGIN $after-proto = 'lifted' }
is $after-proto, 'lifted', 'a method-level proto outside a package no longer keeps later BEGINs in place';

# The in-place declarations still happen.
sub calls-lexical-method { my method lm2() { 'n' ~ self }; 3.&lm2 }
is calls-lexical-method(), 'n3', 'the lexical method is declared in its scope too';
