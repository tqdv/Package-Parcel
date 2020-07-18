# Package::\<Parcel WellKnown>

I wrote **Package::WellKnown** to familiarise myself with Raku's [pseudo-packages](https://docs.raku.org/language/packages). It provides function aliases for `MY::` and `OUR::` and variants that remove _well-known_ symbols like `$_`, `$!`, `$/` so you can focus on variables \*you\* introduced. It also does that for my-scoped and our-scoped packages. 

**Package::Parcel** is a tool to dump package trees. It can create a tree of all classes, modules, roles, etc… available in CORE, OUR and MY and compare it to a snapshot of another tree. This can be used to check what symbols are introduced by `use`ing a module, and if they ended up in the right places.

## Sneakpeek

```raku
use Package::Parcel :tree;

my class Anima::Birb {};
my ($a, @b, %c);
 
get-mytree.pretty-print(:noself, :symbols<all>);
```
outputs
```
MY module → MY (!UNIT_MARKER $! $/ $=finish $=pod $?PACKAGE $_ $a $¢ %c &dump-tree &get-coretree &get-mytree &get-ourtree &get-sometree ::?PACKAGE @b)
├──╴Anima package
│   └──╴Birb class
├──╴EXPORT package
├──╴GLOBALish package → GLOBAL
└──╴Package package
```

See pod in each file for usage documentation.

## License

This software is copyright (c) 2020 by Tilwa Qendov.\
This is free software, licensed under the [Artistic License 2.0](LICENSE).
