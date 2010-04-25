#!/usr/bin/perl -w

use Cwd 'abs_path';
use File::Basename;

chdir(dirname(abs_path($0)));
$ip6addr = "../dist/build/ip6addr/ip6addr";

if (-e $ip6addr) {
	@testList = `cat ./inputs`;
	$fcpt = 0;
	print "\n";
	foreach (@testList) {
		@test = split(/!/);
		$result = `$ip6addr $test[0]`;
		chomp($result);
		if ($test[1] == "0") {print "Bad";} else {print "Good";}	
		print " input \"$test[0]\" - ";
		if (($result eq "" && $test[1] == "0")||($result eq $test[0] && $test[1] == "1")) {
			print "PASSED\n";
		} else {
			print "FAILED !\n";
			$fcpt++;
		} 
		$cpt++;
	}
	print "\nTests: $cpt\nFailed tests: $fcpt\n\n";
} else {
	print "Run  a 'make build' before testing...\n"
}
