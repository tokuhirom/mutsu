sub EXPORT {
    class QualTermHook::Grid {
        has $.r;
        multi method new(Int:D $r, *%a) { self.bless(:$r) }
    }
    Map.new: 'QualTermHook::Grid' => QualTermHook::Grid;
}
