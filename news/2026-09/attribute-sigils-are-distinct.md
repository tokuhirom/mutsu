Attribute declarations that share a bare name but use different sigils are now kept as distinct class attributes. This fixes `X::Comp::Trait::Duplicate` failures in modules such as `Prettier::Table`.
