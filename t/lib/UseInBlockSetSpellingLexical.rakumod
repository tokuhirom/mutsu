unit module UseInBlockSetSpellingLexical;

# User candidates of the Unicode set operator spellings and of the set
# relations, for
# t/modules/import-export/use-in-block-binds-set-operators-lexically.t
class By is export { has $.n }
multi sub infix:<∪>(By $a, By $b) is export { By.new(n => $a.n + $b.n) }
multi sub infix:<⊆>(By $a, By $b) is export { 'custom-subset' }
multi sub infix:«(<=)»(By $a, By $b) is export { 'custom-ascii-subset' }
