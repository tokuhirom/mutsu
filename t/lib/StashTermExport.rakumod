# Installs package terms through the stash from inside `sub EXPORT`, the way
# Logic::Ternary builds its True/Unknown/False values.
sub EXPORT(--> Map()) {
    class StashTerm { ... }
    class StashTerm does Enumeration {
        method new(Str:D $val) { self.bless: key => $val, value => $val.chars }
        method defined { self.key ne 'Maybe' }
    }
    StashTerm::{'Yes'}   = StashTerm.new: 'Yes';
    StashTerm::{'Maybe'} = StashTerm.new: 'Maybe';
    ('Yes' => StashTerm::{'Yes'}, 'Maybe' => StashTerm::{'Maybe'})
}
