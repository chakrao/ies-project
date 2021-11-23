#! /usr/bin/perl -w

#   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

# This file is part of GNU CC.

# GNU CC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# GNU CC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU CC; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston MA 02111-1307, USA.

# This does trivial (and I mean _trivial_) conversion of Texinfo
# markup to Perl POD format.  It's intended to be used to extract
# something suitable for a manpage from a Texinfo document.

$output = 0;
$skipping = 0;
%sects = ();
$section = "";
@icstack = ();
@endwstack = ();
@skstack = ();
@instack = ();
$shift = "";
%defs = ();
$fnno = 1;
$inf = "";
$ibase = "";

while ($_ = shift) {
    if (/^-D(.*)$/) {
	if ($1 ne "") {
	    $flag = $1;
	} else {
	    $flag = shift;
	}
	$value = "";
	($flag, $value) = ($flag =~ /^([^=]+)(?:=(.+))?/);
	die "no flag specified for -D\n"
	    unless $flag ne "";
	die "flags may only contain letters, digits, hyphens, dashes and underscores\n"
	    unless $flag =~ /^[a-zA-Z0-9_-]+$/;
	$defs{$flag} = $value;
    } elsif (/^-/) {
	usage();
    } else {
	$in = $_, next unless defined $in;
	$out = $_, next unless defined $out;
	usage();
    }
}

if (defined $in) {
    $inf = gensym();
    open($inf, "<$in") or die "opening \"$in\": $!\n";
    $ibase = $1 if $in =~ m|^(.+)/[^/]+$|;
} else {
    $inf = \*STDIN;
}

if (defined $out) {
    open(STDOUT, ">$out") or die "opening \"$out\": $!\n";
}

while(defined $inf) {
while(<$inf>) {
    # Certain commands are discarded without further processing.
    /^\@(?:
	 [a-z]+index		# @*index: useful only in complete manual
	 |need			# @need: useful only in printed manual
	 |(?:end\s+)?group	# @group .. @end group: ditto
	 |page			# @page: ditto
	 |node			# @node: useful only in .info file
	 |(?:end\s+)?ifnottex   # @ifnottex .. @end ifnottex: use contents
	)\b/x and next;

    chomp;

    # Look for filename and title markers.
    /^\@setfilename\s+([^.]+)/ and $fn = $1, next;
    /^\@settitle\s+([^.]+)/ and $tl = postprocess($1), next;

    # Identify a man title but keep only the one we are interested in.
    /^\@c\s+man\s+title\s+([A-Za-z0-9-]+)\s+(.+)/ and do {
	if (exists $defs{$1}) {
	    $fn = $1;
	    $tl = postprocess($2);
	}
	next;
    };

    # Look for blocks surrounded by @c man begin SECTION ... @c man end.
    # This really oughta be @ifman ... @end ifman and the like, but such
    # would require rev'ing all other Texinfo translators.
    /^\@c\s+man\s+begin\s+([A-Z]+)\s+([A-Za-z0-9-]+)/ and do {
	$output = 1 if exists $defs{$2};
        $sect = $1;
	next;
    };
    /^\@c\s+man\s+begin\s+([A-Z]+)/ and $sect = $1, $output = 1, next;
    /^\@c\s+man\s+end/ and do {
	$sects{$sect} = "" unless exists $sects{$sect};
	$sects{$sect} .= postprocess($section);
	$section = "";
	$output = 0;
	next;
    };

    # handle variables
    /^\@set\s+([a-zA-Z0-9_-]+)\s*(.*)$/ and do {
	$defs{$1} = $2;
	next;
    };
    /^\@clear\s+([a-zA-Z0-9_-]+)/ and do {
	delete $defs{$1};
	next;
    };

    next unless $output;

    # Discard comments.  (Can't do it above, because then we'd never see
    # @c man lines.)
    /^\@c\b/ and next;

    # End-block handler goes up here because it needs to operate even
    # if we are skipping.
    /^\@end\s+([a-z]+)/ and do {
	# Ignore @end foo, where foo is not an operation which may
	# cause us to skip, if we are presently skipping.
	my $ended = $1;
	next if $skipping && $ended !~ /^(?:ifset|ifclear|ignore|menu|iftex)$/;

	die "\@end $ended without \@$ended at line $.\n" unless defined $endw;
	die "\@$endw ended by \@end $ended at line $.\n" unless $ended eq $endw;

	$endw = pop @endwstack;

	if ($ended =~ /^(?:ifset|ifclear|ignore|menu|iftex)$/) {
	    $skipping = pop @skstack;
	    next;
	} elsif ($ended =~ /^(?:example|smallexample|display)$/) {
	    $shift = "";
	    $_ = "";	# need a paragraph break
	} elsif ($ended =~ /^(?:itemize|enumerate|[fv]?table)$/) {
	    $_ = "\n=back\n";
	    $ic = pop @icstack;
	} else {
	    die "unknown command \@end $ended at line $.\n";
	}
    };

    # We must handle commands which can cause skipping even while we
    # are skipping, otherwise we will not process nested conditionals
    # correctly.
    /^\@ifset\s+([a-zA-Z0-9_-]+)/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = "ifset";
	$skipping = 1 unless exists $defs{$1};
	next;
    };

    /^\@ifclear\s+([a-zA-Z0-9_-]+)/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = "ifclear";
	$skipping = 1 if exists $defs{$1};
	next;
    };

    /^\@(ignore|menu|iftex)\b/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = $1;
	$skipping = 1;
	next;
    };

    next if $skipping;

    # Character entities.  First the ones that can be replaced by raw text
   