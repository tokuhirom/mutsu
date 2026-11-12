# Fixture for t/vm/scope/bareword-lexical-class-site-memo.t: a
# module-level `my class` read as a bareword from the module's own subs.
my class Hidden { method tag { 'hidden' } }
sub hidden-type() is export { Hidden }
sub hidden-or-capture(::Hidden $x) is export { Hidden }
