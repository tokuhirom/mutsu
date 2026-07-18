module EnumHolderWrap {
    # Loading the enum-declaring class inside another module's package block
    # rolls back the bare enum-member env keys on block exit (only qualified
    # keys survive). The class's own methods must still resolve bare members.
    use EnumHolder;

    our sub present() { True }
}
