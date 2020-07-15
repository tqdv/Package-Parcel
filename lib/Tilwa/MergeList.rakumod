unit module Tilwa::MergeList;

#|[ Inspired by the boilerplate for merge sort.
The merge algorithm becomes:
  my @merged;
  merge-lists @a, @b, -> $x { @merged.push($x) }
The symmetric difference becomes:
  my (@left, @right);
  merge-lists @a, @b, { @left.push($_) }, {}, { @right.push($_) }
]
sub merge-lists (Iterable $a, Iterable $b, &left, &both = -> $x, $ { &left($x) } , &right = &left, &cmp = &infix:<cmp>) is export {
	my $A = $a.iterator;
	my $B = $b.iterator;

	my $l := $A.pull-one;
	my $r := $B.pull-one;
	until $l =:= IterationEnd || $r =:= IterationEnd {
		given &cmp($l, $r) {
			when Less { &left($l); $l := $A.pull-one; }
			when Same { &both($l, $r); $l := $A.pull-one; $r := $B.pull-one }
			when More { &right($r); $r := $B.pull-one; }
		}
	}
	until $l =:= IterationEnd {
		&left($l);
		$l := $A.pull-one;
	}
	until $r =:= IterationEnd {
		&right($r);
		$r := $B.pull-one;
	}
}