unit module Package::Parcel;
# Because package tracking → parcel tracking

use Tilwa::WithVal;
use Tilwa::MergeList;

=head2 Trees

enum Ptype <Package Module Class Role Enum Variant Subset CurriedRole Native NQPClass NativeRef Metamodely NQPParametricRole>;

sub get-ptype-str (Str $HOW --> Ptype) is export(:func) {
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
		default { .note } # Throws because of the type check
	}
}

sub get-ptype (\p --> Ptype) is export(:func) {
	get-ptype-str p.HOW.^name
}

class Pkg {
	has Str $.name; # eg. WellKnown
	has Ptype $.type; # package type
	has Str $.longname; # eg. Package::WellKnown
	has Str $.refname; # fully qualified name for variants eg. Ptype::Class → Ptype
	# ^ and maybe imports and aliases ? eg. M::A → Module::A ?

	method gist {
		"("
		~ $!type.lc
		~ " " ~ $!name
		~ (" → $!refname" if $!refname)
		~ ")"
	}
}

class Tree {
	has Pkg $.node is required;
	has Tree @.children; # sorted by name
	has Str @.symbols; # sorted by name

	multi method gist (::?CLASS:D:) {
		"("
		~ "{$!node.gist}"
		~ (', [' ~ @!children».gist.join(', ') ~ ']' if @!children)
		~ ")"
	}

	multi method gist (::?CLASS:U:) {
		'(' ~ self.^name ~ ')'
	}

	method read (IO() $file --> Tree) {
		use MONKEY-SEE-NO-EVAL;
		EVAL $file.slurp
	}

	method dump (::?CLASS:D: IO() $file) {
		$file.spurt(self.raku)
	}

	#|[ Pretty prints our tree with box drawing lines.
	Flags:
	- :novariant, doesn't print variants when they're not inside their enum.
	  eg. enum E <A B> will print E::A and E::B, but not ::A and ::B
	- :noself, don't print the current package nor its children eg. Parcel::
	- :longname, use longnames (eg. Package::WellKnown) instead of the shortname (eg. WellKnown) in the tree
	- :symbols<$>, set to 'all' to print all symbols, set to 'leaf' to print symbols only in leaf nodes
	]
	multi method pretty-print (::?CLASS:D: Bool :$novariant, Bool :$noself, Bool :$longname, Str :$symbols) {
		my $PACKAGE = $?PACKAGE.^name.split('::').[0..*-2].join('::'); # Because we're inside class Tree
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
				print
					($longname ?? .longname !! .name || .longname || .refname)
						~ (" {.lc}" with .type)
					~  (" → {.refname}" if .refname)
			}
			if ($symbols eq 'all') || ($symbols eq 'leaf' && !$tree.children) {
				print " (" ~ $tree.symbols ~ ")"
			}

			print "\n";

			my @children = $tree.children;
			# Remove variants if we're not an enum
			if $novariant && $tree.node.type != Ptype::Enum {
				@children .= grep: { $_.node.type != Ptype::Variant }
			}
			# Remove self if needed
			if $noself {
				@children .= grep: { $_.node.longname ne $PACKAGE }
			}
			my $last = @children.elems - 1;
			for @children.kv -> $k, $v {
				f $v, (|@has-next, $k != $last)
			}
		}

		f self, ()
	}
	multi method pretty-print (::?CLASS:U: |) {
		say "No tree";
	}
}

#| Light class to store left and right
class TreeDiff {
	has Tree $.left;
	has Tree $.right;

	method gist {
		"left => {$!left.gist} right => {$!right.gist}"
	}

	multi method pretty-print (::?CLASS:D:) {
		say "Left tree:";
		$!left.pretty-print(:symbols<leaf>);
		say "Right tree:";
		$!right.pretty-print(:symbols<leaf>);
	}
	multi method pretty-print (::?CLASS:U:) {
		say "No differences";
	}
}

#| Returns a DiffTree of the differences between the two trees
multi sub diff-tree (Tree:D $a, Tree:D $b --> TreeDiff) is export {
	# assumes that we start from the same root
	unless $a.node eqv $b.node {
		return TreeDiff.new: left => $a, right => $b
	}

	my (@left, @right);

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

	my (@lsym, @rsym);
	merge-lists $a.symbols, $b.symbols,
		{ @lsym.push($_) },
		{},
		{ @rsym.push($_) };

	unless @left || @right || @lsym || @rsym { return TreeDiff }

	return TreeDiff.new:
		left  => Tree.new(node => $a.node, children => @left,  symbols => @lsym),
		right => Tree.new(node => $b.node, children => @right, symbols => @rsym)
}

multi sub diff-tree (Tree:D $a, IO() $bfile --> TreeDiff) { diff-tree $a, Tree.read($bfile) }
multi sub diff-tree (IO() $afile, Tree:D $b --> TreeDiff) { diff-tree Tree.read($afile), $b }
multi sub diff-tree (IO() $afile, IO() $bfile --> TreeDiff) { diff-tree Tree.read($afile), Tree.read($bfile) }

multi sub infix:<(^)> (Tree:D $a, Tree:D $b) is export { diff-tree $a, $b }
multi sub infix:<⊖> (Tree:D $a, Tree:D $b) is export { diff-tree $a, $b }

multi sub pretty-tree (Tree $t, |c) is export { $t.pretty-print: |c }
multi sub pretty-tree (TreeDiff $d) { $d.pretty-print }

sub pretty-tree-diff (|c) is export { pretty-tree diff-tree(|c) }

=head2 Dumping trees

#|[ Builds the package tree starting at p, with displayname $name and name $longname.
The last one is used to check if the package is aliased, so set it correctly
Example: subtree X::Attribute, 'Attribute', 'X::Attribute'
]
sub subtree (\p, Str $name, Str $longname --> Tree) is export(:func) {
	my $fullname = p.^name;
	if $fullname ~~ /LoweredAwayLexical/ { return Tree }
	my $type = get-ptype(p);
	my @symbols; # symbols in the current package that aren't packages

	#| When the current package is an enum
	sub take-enum {
		if $name eq p.^shortname { # This is the actual enum
			for p::.keys.sort -> $variant {
				take Tree.new: node => Pkg.new(:name($variant), :type(Ptype::Variant), :refname(p.^name), :$longname)
			}
		} else { # This is a variant
			$type = Ptype::Variant;
		}
	}

	#| When the current package is not an enum and we explore its children
	sub take-normal (Str $key) {
		my $n-longname = ($longname ~ '::' if $longname) ~ $key;
		my \r = p::{$key}; # not q because that's reserved

		unless r.WHAT ~~ Any {
			# The current package is higher than Any so binding to parameter p in the call to subtree fails
			my $refname = r.^name if $n-longname ne r.^name;
			# Guess the type
			if r.^name ~~ /Metamodel/ {
				take Tree.new: node => Pkg.new(
					:name($key), :type(Ptype::Metamodely), |with-val :$refname, :longname($n-longname));
			} else {
				my $type = get-ptype-str(r.HOW.^name);
				take Tree.new: node => Pkg.new(:name($key), :$type, |with-val :$refname, :longname($n-longname));

				CATCH { # Unknown package type
					$*ERR.say: "processing $n-longname failed of { r.WHAT.^name } ofof { r.HOW.^name }"
				}
			}
			return;
		}

		my $v = subtree r, $key, $n-longname;
		take $v with $v;
	}

	my $has-keys = so p::; # calling .keys on a null object dies unconditionally
	my @children = do if $has-keys { gather do {
		if $type == Ptype::Enum {
			take-enum
		} else {
			my @packs; # divide packages and symbols
			for p::.keys.sort -> $k { $k ~~ /^<:L>/ ?? @packs.push($k) !! @symbols.push($k) }
			for @packs -> $key {
				take-normal $key
			}
		}
	}}
	my $refname = $longname ne $fullname ?? $fullname !! Str;

	Tree.new: node => Pkg.new(:$name, :$type, |with-val :$refname, :$longname), :@children, :@symbols
}

sub get-coretree (--> Tree) is export { subtree CORE, '', '' }

sub get-ourtree (--> Tree) is export { subtree CALLER::OUR, 'OUR', '' }

sub get-mytree (--> Tree) is export { subtree CALLER::MY, 'MY', '' }

our @PACKS = <MY OUR CORE GLOBAL PROCESS COMPILING CALLER CALLERS DYNAMIC OUTER OUTERS LEXICAL UNIT SETTING PARENT CLIENT>;

sub get-sometree (\p --> Tree) is export(:tree) {
	my $name = p.^name;
	my $shortname = $name.split('::')[*-1];
	$name ~~ s/ '::'? @PACKS [ '::' @PACKS <wb> ]? '::'? //;

	subtree(p, $shortname, $name)
}

multi sub dump-tree (\p, IO() $file --> Bool) is export(:tree) { $file.spurt: get-sometree(p).raku }
multi sub dump-tree (\p, IO() :$to! --> Bool) { dump-tree(p, $to) }

=begin pod

=head2 Notes

Stashes named after enum variants refer to the enum stash:
  enum A <B C>;
  is A::, B::;
  is A::. C::;
This leads to infinite loop if not handled

=end pod