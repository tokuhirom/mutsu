# `role MergeOuterBase::Kid` nests under the class it `use`s.
use MergeOuterBase;
role MergeOuterBase::Kid is MergeOuterBase { }
