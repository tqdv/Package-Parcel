use Tilwa::WithVal;
use Tilwa::MergeList;

unit module Parcel;

our @UNIT-SYMBOLS = <!UNIT_MARKER $! $/ $=finish $=pod $¢ EXPORT GLOBALish>; # Inside unit scope, includes CORE::<$! $/ $=pod> (CHECKME)
our @PKG-SYMBOLS = |<$?PACKAGE ::?PACKAGE>, # Inside a package
	|<$?MODULE ::?MODULE>, # Inside a module
	|<$?CLASS ::?CLASS>, # Inside a class
	|<$?CONCRETIZATION $?ROLE ::?ROLE>, # Inside a role, in addition to class
;
our @MY-SYMBOLS = <$_>; # Inside any lexical scope
our @BLOCK-SYMBOLS = <$*DISPATCHER>; # Immediately inside a bare block eg. `{ ... }` and not `do { ... }`. It is MY-scoped

#| Tests if package::{$symbol} is almost surely a package
#| Exceptions are if you name a sigiless variable the same as its class
#| or if you .^rename a variable to match it's identifier
sub isas-package(\package, Str $symbol) is export {
	$symbol eq package::{$symbol}.^shortname
}

=head2 Equivalents

#| Get lexicals in scope aka MY::
sub get-locals is export {
	CALLER::.keys.sort
}

#| Get our-scoped symbols aka OUR::
sub get-packaged is export {
	# We use LEXICAL because we want to follow lexical scopes and not call scopes
	my \p = CALLER::OUR;
	p::.keys.sort;
}

=head2 Specifics

# The get-ours and get-mine exclude packages and only return variable identifiers

#| Get our-scoped symbols that aren't packages
sub get-ours is export {
	my \p = CALLER::OUR;
	p::.keys.grep({ ! isas-package p, $_ }).sort;
}

#| Get lexical symbols declared in this scope excluding packages
#| Ignores symbols that are always there even if they are redeclared because we can't detect that
sub get-mine is export {
	my \m = CALLER::MY;

	# Remove predefined symbols
	my $keys = m::.keys ∖ @PKG-SYMBOLS ∖ @MY-SYMBOLS ∖ @BLOCK-SYMBOLS;
	if m::<!UNIT_MARKER>:exists { $keys = $keys ∖ @UNIT-SYMBOLS }

	# Filter out packages
	$keys = $keys.keys.grep: { ! isas-package m, $_ };
	
	return $keys.sort
}

#| Get unit-scoped symbols that aren't predefined
sub get-units is export {
	my \p = CALLER::UNIT;
	(p::.keys ∖ @UNIT-SYMBOLS ∖ @PKG-SYMBOLS ∖ @MY-SYMBOLS).keys.sort
}

#| Get packages defined in scope. Includes my-scoped packages
sub get-packs is export {
	my \m = CALLER::MY;
	my \p = CALLER::OUR;

	my $keys = m::.keys.grep({ isas-package m, $_ }) ∪ p::.keys.grep({ isas-package p, $_ });
	return $keys.keys.sort
}

#| Get packages defined in this lexical scope. This may include imported packages
sub get-mypacks is export {
	my \m = CALLER::MY;
	my \p = CALLER::OUR;

	my $keys = m::.keys.grep({ isas-package(m, $_) && ! grep $_, p::.keys});
	if m::<!UNIT_MARKER>:exists { $keys .= grep: { ! grep $_, @UNIT-SYMBOLS } }
	return $keys.sort;
}

#| Get packages defined in this package
sub get-ourpacks is export {
	my \m = CALLER::OUR;

	my $keys = m::.keys.grep({ isas-package(m, $_) });
	return $keys.sort;
}

=head2 Trees

enum Ptype <Package Module Class Role Enum Variant Subset CurriedRole Native NQPClass NativeRef Metamodel? NQPParametricRole>;

sub get-ptype-str (Str $HOW --> Ptype) {
	given $HOW {
		when /'Metamodel::PackageHOW'/ { Ptype::Package }
		when /'Metamodel::ModuleHOW'/ { Ptype::Module }
		when /'Metamodel::ClassHOW'/ { Ptype::Class }
		when /'Metamodel::ParametricRoleGroupHOW'/ { Ptype::Role }
		when /'Metamodel::EnumHOW'/ { Ptype::Enum }
		when /'Metamodel::SubsetHOW'/ { Ptype::Subset }
		when /'Metamodel::CurriedRoleHOW'/ { Ptype::CurriedRole }
		when /'Metamodel::NativeHOW'/ { Ptype::Native }
		when /'Metamodel::NativeRefHOW'/ { Ptype::NativeRef }
		when /'NQPClassHOW'/ { Ptype::NQPClass }
		when /'NQPParametricRoleHOW'/ { Ptype::NQPParametricRole }
		default { .note }
	}
}

sub get-ptype (Mu \p --> Ptype) {
	get-ptype-str p.HOW.^name
}

class Pkg {
	has Str $.name; # short name
	has Ptype $.type; # package type
	has Str $.longname; # with all parents
	has Str $.refname; # fully qualified name, for loops and aliases

	method gist {
		"({$!type.lc} {$!name}{ " → $!refname" if $!refname })"
	}
}

class Tree {
	has Pkg $.node is required;
	has Tree @.children; # sorted by name
	method gist {
		"{$!node.gist}"
		~ (' ⸬ {' ~ @!children».gist.join(', ') ~ '}' if @!children)
	}
}

#| Light class to store left and right
class TreeDiff {
	has Tree $.left;
	has Tree $.right;
}

sub diff-tree (Tree $a, Tree $b --> TreeDiff) is export {
	# assumes that we start from the same root
	unless $a.node eqv $b.node {
		return TreeDiff.new: left => $a, right => $b
	}

	my @left;
	my @right;

	merge-lists $a.children, $b.children,
		{ @left.push($_) },
		{ with diff-tree $^a, $^b -> $d {
			with $d.left -> $_ { @left.push($_) }
			with $d.right -> $_ { @right.push($_) }
		} },
		{ @right.push($_) },
		sub cmp ($a, $b) {
			if $a.node eqv $b.node { return Same }
			given $a.node.name cmp $b.node.name {
				when Less { return Less }
				when More { return More }
				when Same { return Less }
			}
		};

	if @left.elems == 0 && @right.elems == 0 { return TreeDiff }

	return TreeDiff.new: |(:@left if @left), |(:@right if @right)
}

multi sub pretty-tree (Tree:D $t, Bool :$novariant, Bool :$noself, Bool :$longname) is export {
	sub f (Tree $tree, @has-next) {
		sub print-junctions {
			my $last = @has-next.elems - 1;
			for @has-next.kv -> $i, $draw {
				if $i == $last {
					print ($draw ?? '├' !! '└') ~ '──╴'
				} else {
					print $draw ?? '│   ' !! '    '
				}
			}
		}

		print-junctions;
		with $tree.node {
			print "{$longname ?? .longname !! .name} {with .type -> $t { $t.lc }}" ~  (" → {.refname}" if .refname)
		}

		print "\n";

		my @children = $tree.children;
		# Remove variants if we're not an enum
		if $novariant && $tree.node.type != Ptype::Enum {
			@children .= grep: { $_.node.type != Ptype::Variant }
		}
		# Remove self if needed
		if $noself {
			@children .= grep: { $_.node.longname ne $?PACKAGE.^name }
		}
		my $last = @children.elems - 1;
		for @children.kv -> $k, $v {
			f $v, (|@has-next, $k != $last)
		}
	}

	f $t, ()
}

multi sub pretty-tree (TreeDiff:D $d) {
	say "Left tree:";
	with $d.left -> $l { pretty-tree $l } else { say "(Tree)" }
	say "Right tree:";
	with $d.right -> $l { pretty-tree $l } else { say "(Tree)" }
}

=begin pod

Stashes named after enum variants refer to the enum stash:
  enum A <B C>;
  is A::, B::;
  is A::. C::;
This leads to infinite loop if not handled


=end pod

sub subtree (\p, Str $name, Str $longname --> Tree) {
	#say "=== processing $name";

	my $fullname = p.^name;
	if $fullname ~~ /LoweredAwayLexical/ { return Tree }
	my $type = get-ptype(p);
	my $is-enum = $type == Ptype::Enum;
	my $has-keys = so p::; # calling .keys on a null object dies unconditionally

	#say "$longname isa $type";
	#say "keys in $longname: " ~ p::.keys.sort;

	my @children = do if $has-keys { gather do {
		if !$is-enum {
			for p::.keys.grep(* ~~ /^<:L>/).sort -> $key {
				my $n-longname = ($longname ~ '::' if $longname) ~ $key;
				my \r = p::{$key}; # not q because that's reserved

				unless r.WHAT ~~ Any {
					#note "GOT {r.^name}";
					# Higher than Any
					# Otherwise it fails to bind to the p parameter in the call to subtree
					my $refname = r.^name if $n-longname ne r.^name;
					if r.^name ~~ /Metamodel/ {
						take Tree.new: node => Pkg.new(
							:name($key), :type(Ptype::<Metamodel?>), |with-val :$refname, :longname($n-longname));
					} else {
						my $type = get-ptype-str(r.HOW.^name);
						take Tree.new: node => Pkg.new(:name($key), :$type, |with-val :$refname, :longname($n-longname));
						CATCH {
							$*ERR.say: "processing $n-longname failed of { r.WHAT.^name } ofof { r.HOW.^name }"
						}
					}
					next;
				}

				my $v = subtree r, $key, $n-longname;
				take $v with $v;
				CATCH {
					$*ERR.say: "processing $n-longname";
					.note;
					next;
				}
			}

		} else {
			my $is-enum-root = $name eq p.^shortname; # Is this a variant of the enum name ?
			if $is-enum-root {
				for p::.keys.sort -> $variant {
					take Tree.new: node => Pkg.new(:name($variant), :type(Ptype::Variant), :refname(p.^name), :$longname)
				}
			} else {
				$type = Ptype::Variant;
			}
		}
	}}
	my $refname = $longname ne $fullname ?? $fullname !! Str;

	Tree.new: node => Pkg.new(:$name, :$type, |with-val :$refname, :$longname), :@children,
}

sub get-coretree (--> Tree) is export {
	subtree CORE, '', ''
}

sub get-mytree (--> Tree) is export {
	my \p = CALLER::MY;
	subtree p, 'MY', ''
}

=begin pod

=head2 Caveats

We can't differentiate imported symbols from symbols defined in the current scope

=head2 Notes

We use CALLER::MY instead of CALLER in some places, because isas-package fails with this otherwise:
  Cannot access '$_' through CALLER, because it is not declared as dynamic

=end pod