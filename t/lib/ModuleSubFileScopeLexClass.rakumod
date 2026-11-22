unit module ModuleSubFileScopeLexClass;
my class R { method who { 'module R' } }
my role Ro { method role-who { 'module Ro' } }
sub make-r is export { R.new }
sub r-name is export { R.^name }
sub mixed is export { 42 but Ro }
