# A grammar's named capture is dispatched as a method call on the Match

`Template::Mojo` 0.2.2 runs **5/5** of its upstream test files under raku and
**0/5** under mutsu, every one dying with:

```
No such method 'characters' for invocant of type 'Match'
```

`.characters` is not a `Match` method in raku either — verified, the message is
byte-identical there. So mutsu is not *missing* a method: it is turning a
**named capture access into a method call**. The module has

```raku
grammar Template::Mojo::Grammar {
    token expression {
        || <perlline>
        || <perlcapture-begin>
        || <perlcapture-end>
        || <perlexpr>
        || <characters>          # lib/Template/Mojo.rakumod:11
    }
    …
    token characters { \n | [ <!before '<%' || \n > . ]+ \n? }   # :38
}
```

and the actions read `$<characters>.Str` (`:67`). Somewhere the subrule
`<characters>` is not being found in the grammar, and the fallback resolves the
name against `Match`'s methods instead.

## Repro

```sh
curl -sSL 'https://raw.githubusercontent.com/raku/REA/main/archive/T/Template%3A%3AMojo/Template%3A%3AMojo%3Aver%3C0.2.2%3E%3Aauth%3Czef%3Araku-community-modules%3E.tar.gz' | tar xz
cd Template::Mojo*/ && mutsu -I lib t/00-basic.rakutest     # dies at once; raku passes 17/17
```

## Not yet reduced

A hand-written small grammar with a `||` alternation, a forward-referenced token
and a `<!before … || …>` assertion did **not** reproduce it, so the trigger is
narrower than "named capture in an action". Candidates worth bisecting in the
real grammar: the `proto regex tag { <...> }` + `tag:sym<…>` candidates, the
`$*LEFT`/`$*RIGHT` dynamic variables used inside the regexes, or the
`<?{ $<tag>.made<type> ~~ none(…) }>` code assertion.

Part of the template-battery survey — see
`todo/deep/template-engines-blocked-on-mutsu.md` and
`docs/batteries/templates.md`.
