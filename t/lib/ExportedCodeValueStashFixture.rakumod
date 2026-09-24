module ExportedCodeValueStashFixture {
    our sub original is export(:all) { 'from module stash' }
    our constant &alias is export(:all) = &original;
}
