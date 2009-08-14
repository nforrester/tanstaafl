#!/usr/bin/perl

$line_count = 0;
$skip = 0;

print ";;;; This file is ugly, because it was generated automatically, by vsop87-raw-parser.pl\n";

while($line = <>){
	if($line =~ /#if 0/){
		$skip = 1;
	}elsif($line =~ /#endif/){
		$skip = 0;
	}elsif($skip == 0){
		if($line =~ /static VSOPTerm .*_.\d\[\] = {/){
			$line =~ /\d/;
			if($& == 0){
				if($line_count != 0){
					print "))\n";
				}
				$line =~ /static VSOPTerm /;
				$line = $';
				$line =~ /\d\[\] = {/;
				$line = $`;
				$body = $line;
				$series_set = chop $body;
				chop $body;
				chomp $series_set;
				if($series_set eq 'L'){
					$series_set = 'long';
				}
				if($series_set eq 'B'){
					$series_set = 'lat';
				}
				if($series_set eq 'R'){
					$series_set = 'rad';
				}
				if($series_set eq 'X' || $series_set eq 'Y' || $series_set eq 'Z'){
					$body =~ s/sun/sol/g;
				}
				print "(defvar *vsop-series-set-$body-$series_set* '(\n";
			}
			print "(\n";
		}elsif($line =~ /};/){
			print ")\n";
		}elsif($line =~ /\/\//){
			print ";$'";
		}elsif($line =~ /{ -?\d+(\.\d*)?(e-?\d*)?, -?\d+(\.\d*)?(e-?\d*)?, -?\d+(\.\d*)?(e-?\d*)? }/){
			$line =~ s/e/d/g;
			$line =~ /-?\d+(\.\d*)?(d-?\d*)?(,| )/;
			$a = $&;
			$line = $';
			$a =~ s/(,| )//g;
			if($a =~ /d/){
			}else{
				$a .= "d0";
			}
			$line =~ /-?\d+(\.\d*)?(d-?\d*)?/;
			$b = $&;
			$line = $';
			if($b =~ /d/){
			}else{
				$b .= "d0";
			}
			$line =~ /-?\d+(\.\d*)?(d-?\d*)?/;
			$c = $&;
			$line = $';
			if($c =~ /d/){
			}else{
				$c .= "d0";
			}
			print "($a $b $c)\n";
		}elsif($line eq "\n"){
		}else{
			die "unrecognized at $line_count: $line";
		}
		$line_count++;
	}
}
print "))\n";
