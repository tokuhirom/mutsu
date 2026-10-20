grammar Holder {
    use Pfx::Sub::Gram;
    also is Pfx::Sub::Gram;
    # `use Pfx` here loads a CLASS whose name is the head of the parent's name.
    method m(|c) { use Pfx; Pfx.hi }
}
