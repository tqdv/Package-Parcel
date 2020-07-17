# Package::\<Parcel WellKnown>

I wrote **Package::WellKnow** to familiarise myself with Raku's [pseudo-packages](https://docs.raku.org/language/packages). It provides function aliases for `MY::` and `OUR::` and variants that remove _well-known_ symbols like `$_`, `$!`, `$/` so you can focus on variables \*you\* introduced. It also does that for my-scoped and our-scoped packages. 

**Package::Parcel** is a tool to dump package trees. It can create a tree of all classes, modules, roles, etc… available in CORE, OUR and MY and compare it to a snapshot of another tree. This can be used to check what symbols are introduced by `use`ing a module, and if they ended up in the right places.

See pod in each file for usage documentation.

## License

This software is copyright (c) 2020 by Tilwa Qendov.\
This is free software, licensed under the [Artistic License 2.0](LICENSE).