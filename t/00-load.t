#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'Algorithm::DiffMatchPatch' ) || print "Bail out!
";
}

diag( "Testing Algorithm::DiffMatchPatch $Algorithm::DiffMatchPatch::VERSION, Perl $], $^X" );
