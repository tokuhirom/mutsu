use Test;

# A `::T` type capture and a nominal invocant type are two independent facts
# about one parameter (`method m(::T Foo:D: $x --> T)`, #7984). mutsu used to
# keep both in a single `ParamDef::type_constraint` string, so the combination
# could not be expressed at all: the signature did not parse, and the two
# spellings that did parse mis-bound `T`.

plan 16;

class Box {
    has $.v = 1;
    method same(::T Box:D: $x --> T) { $x }
    method copy(::T Box:D: --> T)    { self }
    method capture-only(::T: $x --> T) { $x }
    method no-args(::T: --> T)       { self }
    method plain(::T $x --> T)       { $x }
}

# 1-2. A type capture followed by a nominal invocant type parses, captures the
# invocant's type, and enforces the nominal constraint.
my $b = Box.new;
is $b.same($b).WHAT.^name, 'Box', '::T Box:D: captures the invocant type for --> T';
is $b.copy.WHAT.^name, 'Box', '::T Box:D: with no further params returns T';

# 3-4. The capture must still be usable as a type when the invocant carries no
# nominal type. `T` was bound for the body but the `--> T` return check resolved
# against the literal name `T`, because the capture-bound marker collided with
# the synthetic parameter name the bare capture carries.
is $b.capture-only($b).WHAT.^name, 'Box', '::T: captures the invocant type for --> T';
is $b.no-args.WHAT.^name, 'Box', '::T: with no params returns T';

# 5. A non-invocant capture on a METHOD (not just a sub) binds too; it used to
# be type-checked against the literal constraint `::T` and always failed.
is $b.plain(42).WHAT.^name, 'Int', '::T $x on a method captures the argument type';

# 6. The nominal half is genuinely enforced, not just parsed past.
class Other {
    method f(::T Box:D: $x --> T) { $x }
}
dies-ok { Other.f(1) }, 'the nominal invocant type of `::T Box:D:` is enforced';

# 7-8. A role body that does not parse is a parse error, not a silently empty
# role. `role R { method m(::T R:D: $x) {...} }` used to compile to an empty
# role body, so the call site died with "No such method" and no diagnostic ever
# named the signature that failed to parse.
role Mergeable {
    method merge(::T Mergeable:D: $other --> T) { self }
}
class Doc does Mergeable {}
my $d = Doc.new;
is $d.merge(Doc.new).WHAT.^name, 'Doc', 'a role method with `::T R:D:` is composed, not dropped';
ok Doc.^can('merge'), 'the role method really reached the class';

# 9. A role body with a genuine syntax error is now reported, rather than
# yielding a role with no methods at all.
throws-like 'role Broken { method m( } }; 1', Exception,
    'an unparsable role body is a parse error';

# 10-11. Subs keep working, and so does a capture shared across parameters.
# (`try` rather than `dies-ok`: a capture-constrained parameter's binding
# failure does not reach `dies-ok`'s handler — a separate, pre-existing
# divergence, #8064.)
sub pick(::T $x, T $y --> T) { $y }
is pick(1, 2).WHAT.^name, 'Int', 'a capture declared by one param constrains the next';
my $bad = try { pick(1, 'two') };
ok $!.defined, 'a later param is checked against the captured type';

# 12-13. Role type parameters are unaffected.
role Holder[::E] { method elem { E } }
is Holder[Str].new.elem.^name, 'Str', 'role type parameters still bind their capture';
role Defaulted[::E = Int] { method elem { E } }
is Defaulted.new.elem.^name, 'Int', 'a defaulted role type parameter still binds';

# 14-16. The capture survives into signature introspection: `.type_captures`
# names it, `.type` stays Any (the capture is not a nominal constraint), and
# `.gist` renders it before the — here empty — nominal type slot, two spaces and
# all, exactly as rakudo does.
sub captured(::T $x) { $x }
is &captured.signature.params[0].type_captures.join(','), 'T', 'Parameter.type_captures names the capture';
is &captured.signature.params[0].type.^name, 'Any', 'a capture-only parameter has no nominal type';
is &captured.signature.gist, '(::T  $x)', 'the gist renders the capture ahead of the type slot';
