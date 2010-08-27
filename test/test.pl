#!/usr/bin/perl -w

use Cwd 'abs_path';
use File::Basename;

chdir(dirname(abs_path($0)));

$ip6addrval = "../dist/build/ip6addrval/ip6addrval";
$ip6addrgen = "../dist/build/ip6addrgen/ip6addrgen";

if ((-e $ip6addrval) && (-e $ip6addrgen)) {

	print "'ip6addrval < a set of inputs'\n";

	@testList1 = `cat ./inputs`;
	$fcpt = 0;
	foreach (@testList1) {
		@test = split(/!/);
		$result = `$ip6addrval $test[0]`;
		chomp($result);
		if ($test[1] == "0") {print "Bad ";} else {print "Good";}	
		print " input >> '$test[0]' >> ";
		@s=split(/\//,$test[0]);
		if (($result eq "" && $test[1] == "0")||($result eq $s[0] && $test[1] == "1")) {
			print "PASSED\n";
		} else {
			print "FAILED !\n";
			$fcpt++;
		} 
		$cpt++;
	}

	print "30 tests, $cpt passed.\n\n";

	print "'ip6addrgen -p 1234:abcd -n 5 | ip6addrval'\n";
	@testList2 = `$ip6addrgen -p 1234:abcd -n 5 | $ip6addrval`;
	foreach (@testList2) { print $_; $cpt++;$tcpt++}
	print "5 random addresses generated >> $tcpt outputted.\n\n";

	$tcpt = 0;
	print "\n'ip6addrgen -p ff00 -n 5 | ip6addrval'\n";
	@testList2 = `$ip6addrgen -p ff00 -n 5 | $ip6addrval`;
	foreach (@testList2) { print $_; $cpt++;$tcpt++}
	print "5 random addresses generated >> $tcpt outputted.\n\n";
	
	$tcpt = 0;
	print "\n'ip6addrgen -p a:b:c:d:e:f000 -n 5 | ip6addrval'\n";
	@testList2 = `$ip6addrgen -p a:b:c:d:e:f000 -n 5 | $ip6addrval`;
	foreach (@testList2) { print $_; $cpt++;$tcpt++}
	print "5 random addresses generated >> $tcpt outputted.\n";

	print "\nTests: $cpt\nFailed tests: ".(45 - $cpt)."\n\n";
} else {
	print "Run  a 'make build' before testing...\n"
}
