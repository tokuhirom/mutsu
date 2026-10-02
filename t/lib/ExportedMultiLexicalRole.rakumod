my role Tagged { }
my class Plain { }

sub tagged-handle() is export { $*OUT but Tagged }
sub tagged-int() is export { 5 but Tagged }
sub plain() is export { Plain.new }

multi sub describe(Tagged $h) is export { 'tagged' }
multi sub describe(Plain $p) is export { 'plain' }
multi sub describe(Str $s) is export { 'str' }
