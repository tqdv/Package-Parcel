unit module Package::WellKnown;

=begin pod

=head2 Usage
  use Package::WellKnown;

  get-locals; # the sorted list of keys in MY::
  get-packaged; # the sorted list of keys in OUR::

  get-ours; # the sorted list of symbols in the current package excluding classes, modules, etc…
  get-mine; # the sorted list of symbols defined in this immediate scope with my. This excludes our-declared symbols.

  get-mypacks; # the sorted list of packages declared in this immediate scope with my
  get-ourpacks; # the sorted list of packages declared in this package

=end pod
# Inside unit scope, includes CORE::<$! $/ $=pod> cf. (UNIT::.keys (&) CORE::.keys)
our $UNIT-SYMBOLS = set <!UNIT_MARKER $! $/ $=finish $=pod $¢ EXPORT GLOBALish>;
our $PKG-SYMBOLS = set
	|<$?PACKAGE ::?PACKAGE>, # Inside a package
	|<$?MODULE ::?MODULE>, # Inside a module
	|<$?CLASS ::?CLASS>, # Inside a class
	|<$?CONCRETIZATION $?ROLE ::?ROLE>, # Inside a role, in addition to class
;
# Inside any lexical scope
our $MY-SYMBOLS = set <$_>;
# Immediately inside a bare block eg. `{ ... }` and not `do { ... }`. It is MY-scoped
our $BLOCK-SYMBOLS = set <$*DISPATCHER>;

#| Tests if package::{$symbol} is almost surely a package
#| Exceptions are if you .^rename a variable to match its identifier
sub isas-package(\package, Str $symbol) is export(:util) {
	$symbol eq package::{$symbol}.^shortname
}

=head2 Equivalent to pseudo-packages

#| Get lexicals in scope aka MY::
sub get-locals is export {
	CALLER::.keys.sort
}

#| Get our-scoped symbols aka OUR::
sub get-packaged is export {
	CALLER::OUR::.keys.sort;
}

=head2 Excluding packages and well-known symbols
Excluded well-known symbols even if they are redeclared

#| Get our-scoped symbols that aren't packages
sub get-ours is export {
	my \p = CALLER::OUR;
	p::.keys.grep({ ! isas-package p, $_ }).sort;
}

#| Get my-scoped symbols declared in this scope excluding outer scopes, packages and our-scoped symbols
sub get-mine is export {
	my \m = CALLER::MY;
	my \p = CALLER::OUR;

	my $keys = m::.keys
		∖ $PKG-SYMBOLS ∖ $MY-SYMBOLS ∖ $BLOCK-SYMBOLS # Remove predefined symbols...
		∖ p::.keys; # ... our-scoped symbols
	if m::<!UNIT_MARKER>:exists { $keys = $keys ∖ $UNIT-SYMBOLS }

	# Filter out packages
	$keys = $keys.keys.grep: { ! isas-package m, $_ };

	return $keys.sort
}

=head2 Packages

#| Get packages defined in this scope including imported packages but excluding outer scopes and our-scoped packages
sub get-mypacks is export {
	my \me = CLIENT::MY;
	my \p = CLIENT::OUR;

	my $keys = me::.keys.grep({ isas-package(me, $_) && $_ ∉ p::.keys});
	if me::<!UNIT_MARKER>:exists {
		$keys = ($keys ∖ $UNIT-SYMBOLS ∖ p::.keys).keys; # Remove our-scoped packages
	}
	return $keys.sort;
}

#| Get packages defined in this package
sub get-ourpacks is export {
	my \p = CLIENT::OUR;

	my $keys = p::.keys.grep({ isas-package(p, $_) });
	return $keys.sort;
}

=begin pod

=head2 Caveats

We can't differentiate imported symbols from symbols defined in the current scope

=head2 Notes

We use CLIENT::MY instead of CLIENT in some places, because isas-package fails with this otherwise:
  Cannot access '$_' through CLIENT, because it is not declared as dynamic

=end pod