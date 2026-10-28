use InvocantWhereTag;

role InvocantWhereCol::Meths is export {
    multi method kind(\SELF where { .returns ~~ InvocantWhereTag }:) { "tag" }
    multi method kind(\SELF where { .returns !~~ InvocantWhereTag }:) { "other" }
}

sub tag-type() is export { InvocantWhereTag }
