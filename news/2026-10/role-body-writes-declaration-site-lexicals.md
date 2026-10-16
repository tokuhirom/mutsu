# A role body writes its declaration site's variables, wherever it is composed

```raku
my $seen = 'decl';
role Peek { $seen ~= '+'; method p { 'p' } }
sub composer($n) { my $seen = 'composer'; my $o = $n but Peek; $seen }
say composer(5);   # composer
say $seen;         # decl+
```

A role's body runs when the role is composed, which can happen inside a
routine (`$n but R`). The variables the body names are now always those of
the place the role was declared: a write such as `$count++` reaches the
declaring block at once, and a same-named lexical of the composing routine is
no longer read or overwritten by mistake (ADR-12529 §3, slice 34).
