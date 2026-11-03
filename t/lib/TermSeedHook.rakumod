use v6.d;

sub EXPORT {
    use TermSeedHub;
    use TermSeedHub::Leaf;

    Map.new:
        'TermSeedHub' => TermSeedHub,
        'TermSeedHub::Leaf' => TermSeedHub::Leaf;
}
