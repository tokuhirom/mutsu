# A renamed mixin type names its instances' `.gist` and `.raku` too

```raku
class Box {
    method ^parameterize(Mu \c, Mu \t) {
        my $w := c.^mixin(BoxOf[t]);
        $w.^set_name("Box[{t.^name}]");
        $w
    }
}
say Box[Int].new.raku;    # Box[Int].new   (was Box+{BoxOf[Int]}.new)
```

An instance's type is the type object it was built from, so a `.^set_name` on
that type object names the instance as well (#11805). The type object, and
the `.^name` of an instance, already answered the new name; the `.gist`,
`.raku` and `.perl` of an instance still rendered the synthesized
`Base+{Role,...}` composition.

The rename lives on the composition-keyed shared node (ADR-0060). Three places
asked which name a role-mixed value reports and answered it separately: the
`.^name` fast path, `HOW.name`, and the retargeting step that rewrites the
leading type name of an instance's `.gist`/`.raku`. The last one read only the
synthesized name. They now share `Interpreter::mixin_instance_type_name`.
