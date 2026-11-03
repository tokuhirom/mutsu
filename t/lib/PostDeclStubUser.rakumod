use PostDeclStub::Source;
class PostDeclHolder does PostDeclStub::Source { has PostDeclTree $.tree; }
role PostDeclTree { method x { 42 } }
