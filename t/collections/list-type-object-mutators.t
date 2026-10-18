use Test;

# The `List` type object refuses the mutators Rakudo defines on `List` with
# X::Immutable, as an instance does (ADR-11276 §9.177).

plan 7;

throws-like { List.push(1) }, X::Immutable, :method<push>, :typename<List>, 'List.push refuses the type object';
throws-like { List.append(1) }, X::Immutable, :method<append>, 'List.append refuses the type object';
throws-like { List.unshift(1) }, X::Immutable, :method<unshift>, 'List.unshift refuses the type object';
throws-like { List.prepend(1) }, X::Immutable, :method<prepend>, 'List.prepend refuses the type object';
throws-like { List.pop }, X::Immutable, 'List.pop refuses the type object';
throws-like { List.shift }, X::Immutable, 'List.shift refuses the type object';
throws-like { (1, 2).push(3) }, X::Immutable, 'an instance still refuses';
