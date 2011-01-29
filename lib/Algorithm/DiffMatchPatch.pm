package Algorithm::DiffMatchPatch;

use warnings;
use strict;

=head1 NAME

Algorithm::DiffMatchPatch - The great new Algorithm::DiffMatchPatch!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Algorithm::DiffMatchPatch;

    my $foo = Algorithm::DiffMatchPatch->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 SUBROUTINES/METHODS

=head2 function1

=cut

sub function1 {
}

=head2 function2

=cut

sub function2 {
}


sub new {
    my ($class) = @_;
    
    my $self = {};
    bless $self, $class;
    $self->_init();
    return $self;
}

sub _init() {
    my $self = shift;
    
      # Defaults.
  # Redefine these in your program to override the defaults.

  # Number of seconds to map a diff before giving up (0 for infinity).
  $self->{diff_timeout} = 1.0;
  # Cost of an empty edit operation in terms of edit characters.
  self->{diff_edit_cost} = 4;
  # At what point is no match declared (0.0 = perfection, 1.0 = very loose).
  self->{match_threshold} = 0.5;
  # How far to search for a match (0 = exact location, 1000+ = broad match).
  # A match this many characters away from the expected location will add
  # 1.0 to the score (0.0 is a perfect match).
  self->{match_distance} = 1000;
  # When deleting a large block of text (over ~64 characters), how close does
  # the contents have to match the expected contents. (0.0 = perfection,
  # 1.0 = very loose).  Note that Match_Threshold controls how closely the
  # end points of a delete need to match.
  self->{patch_delete_threshold} = 0.5;
  # Chunk size for context length.
  self->{patch_margin} = 4;

  # The number of bits in an int.
  self->{match_max_bits} = 32;
  
  return;

}

sub ceil {
    my ($self, $n) = @_;
    return int($n + .99);
}
#int(($text1_len + $text2_len)/2.0 + .99);

sub diff_main {
    my ($self, $text1, $text2, $checklines, $deadline) = @_;
    
    # Set a deadline by which time the diff must be complete.
    unless(defined $deadline){
        if($self->{diff_timeout} <= 0){
            $deadline = 2 ** 52;
        }else{
            $deadline = time() + $self->{diff_timeout};
        }
    }
    
    # Check for null inputs.
    if (!defined $text1 or !defined $text2){
        die "Null inputs. (diff_main)";
    }
    
    # Check for equality (speedup)
    if ($text1 eq $text2){
        return [[ DIFF_EQUAL, $text1 ]] if $text1;
        return [];
    }
    
      // Trim off common prefix (speedup).

    my $common_length = $self->diff_common_prefix($text1, $text2);
    my $common_prefix = substr $text1, $common_length;
    $text1 = substr $text1, $common_length;
    $text2 = substr $text2, $common_length;
    
      // Trim off common suffix (speedup).

    $common_length = $self->diff_common_suffix($text1,$text2);
    my $common_suffix = substr $text1, length($text1) - $common_length;
    $text1 = substr $text1, 0, length($text1) - $common_length;
    $text2 = substr $text2, 0, length($text2) - $common_length;
    
    # Compute the diff on the middle block.
    my $diffs = $self->diff_compute(text1, text2, checklines, deadline);

    # Restore the prefix and suffix.
    unshift @$diffs, [ DIFF_EQUAL, common_prefix ] if $common_prefix;
    push @$diffs, [ DIFF_EQUAL, common_suffix ] if $common_suffix;
    
    $self->diff_cleanup_merge($diffs);
    
    return $diffs;
}


sub diff_compute {
    my ($self, $text1, $text2, $checklines, $deadline) = @_;
	my ($long_text, $short_text);
	return [[ DIFF_INSERT, $text2 ]] unless $text1;
	return [[ DIFF_DELETE, $text1 ]] unless $text2;
	
	if( length($text1) > length($text2) ){
		($long_text, $short_text) = ($text1, $text2);
	}else{		
		($short_text, $long_text) = ($text1, $text2);
	}
	
	my $i = index $long_text, $short_text;
	if($i != -1){
		# shorter text is inside longer text (speedup)
		$diffs = [[DIFF_INSERT, substr($long_text, 0, $i) ],
				[DIFF_EQUAL, $short_text],
				[DIFF_INSERT, substr($longtext, length($short_text) + $i)]];
		# swap insertions for deletions if diff is reversed.
		if(length($text1) > length($text2)) {
			$diffs->[0][0] = DIFF_DELETE;
			$diffs->[2][0] = DIFF_DELETE;
		}
		return $diffs;
    }

    if(length($short_text) == 1) {
        # single character string
        # After the previous speedup, the character can't be an equality
        return [[ DIFF_DELETE, $text1, [DIFF_INSERT, $text2]];
    }

    $long_text = $short_text = undef;

    # check to see if the problem can be split in two
    my $hm = $self->diff_half_match($text1,$text2);
    if($hm){
        my ($text1_a, $text1_b) = ($hm->[0], $hm->[1]);
        my ($text2_a, $text2_b) = ($hm->[2], $hm->[3]);
        my $mid_common = $hm->[4];

        # send off both pairs for separate processing

        my $diffs_a = $self->diff_main($text1_a, $text2_a, $checklines, $deadline);
        my $diffs_b = $self->diff_main($text1_b, $text2_b, $checklines, $deadline);

        push @$diffs_a, ([[DIFF_EQUAL, $mid_common]]), @$diffs_b;
        return $diffs_a;
   }

   if($checklines and length($text1) > 100 and length($text2) > 100) {
       return $self->diff_line_mode($text1, $text2, $deadline);
   }

   return $self->diff_bisect($text1, $text2, $deadline);
}

sub diff_line_mode {
    my ($self, $text1, $text2, $deadline) = @_;

    ($text1, $text2, $line_array) = $self->diff_lines_to_chars($text1, $text2);

    my $diffs = $self->diff_main($text1, $text2, 0, $deadline);

    # Convert the diff back to original text.
    $self->diff_chars_to_lines($diffs, $line_array);
    # Eliminate freak matches (e.g. blank lines)
    $self->diff_cleanup_semantic($diffs);

    # Rediff any replacement blocks, this time character-by-character.
    # Add a dummy entry at the end.
    push @$diffs, [ DIFF_EQUAL, '' ];
    my $pointer = 0;
    my $count_delete = 0;
    my $count_insert = 0;
    my $text_delete = '';
    my $text_insert = '';
    while($pointer < scalar @$diffs){
        if($diffs->[$pointer][0] == DIFF_INSERT){
            $count_insert++;
            $text_insert += $diffs->[$pointer][1];
        }elsif($diffs->[$pointer][0] == DIFF_DELETE){
            $count_delete++;
            $text_delete += $diffs->[$pointer][1];
        }elsif($diffs->[$pointer][0] == DIFF_EQUAL){
            if($count_delete >= 1 and $count_insert >= 1){
                my $a = $self->diff_main($text_delete, $text_insert, 0, $deadline);
                splice @$diffs, ($pointer - $count_delete - $count_insert), $count_insert + $count_delete;
                $pointer = $pointer - $count_delete - $count_insert;
                for(my $j = $#$a; $j >= 0; $j--){
                    splice @$diffs, $pointer, 0, $a->[$j];
                }
                $pointer += scalar @$a;
           }
           $count_insert = 0;
           $count_delete = 0;
           $text_delete = '';
           $text_insert = '';
        }
        $pointer++;
    }
    pop @$diffs;

    return $diffs;
}

sub diff_bisect {
    my ($self, $text1, $text2, $deadline) = @_;

    my $text1_len = length($text1);
    my $text2_len = length($text2);
    my $max_d = $self->ceil(($text1_len + $text2_len)/2.0);
    my $v_offset = $max_d;
    my $v_length = 2 * $max_d;
    my @v1 = ((-1) x $v_length);
    $v1[$v_offset + 1] = 0;
    my @v2 = @v1;
    my $delta = $text1_len - $text2_len;

    my $front = (($delta % 2) != 0);

    my $k1start = 0;
    my $k1end = 0;
    my $k2start = 0;
    my $k2end = 0;

    foreach my $d (0 .. $max_d - 1){
        last if time() > $deadline;

        for(my $k1 = -$d + $k1start; $k1 < $d - $k1end; $k1 += 2){
            my $k1_offset = $v_offset + $k1;
            my $x1;
            if($k1 == -$d or $k1 != $d and $v1[$k1_offset - 1] < $v1[$k1_offset + 1]){
                $x1 = $v1[$k1_offset + 1];
            }else{
                $x1 = $v1[$k1_offset - 1] + 1;
            }

            my $y1 = $x1 - $k1;

            while( $x1 < $text1_len and $y1 < $text2_len and
                substr($text1, $x1, 1) eq substr($text2, $y1, 1)){
                $x1++;
                $y1++;
            }
            $v1[$k1_offset] = $x1;
            if($x1 > $text1_len){
                $k1end += 2;
            }elsif($y1 > $text2_len){
                $k1start += 2;
            }elsif($front){
                my $k2_offset = $v_offset + $delta - $k1;
                if($k2_offset >= 0 and $k2_offset < $v_length and $v2[$k2_offset] != -1){
                    my $x2 = $text1_len - $v2[$k2_offset];
                    if($x1 >= $x2){
                        return $self->diff_bisect_split($text1,$text2,$x1,$y1,$deadline);
                    }
                }
            }
        }

        for(my $k2 = -$d + $k2start; $k2 <= $d - $k2end; $k2 += 2){
            my $k2_offset = $v_offset + $k2;
            my $x2;
            if($k2 == -$d or $k2 != $d and $v2[$k2_offset - 1] < $v2[$k2_offset + 1]){
                $x2 = $v2[$k2_offset + 1];
            }else {
                $x2 = $v2[$k2_offset - 1] + 1;
            }

            my $y2 = $x2 - $k2;
            while($x2 < $text1_len and $y2 < $text2_len and 
                substr($text1, $text1_len - $x2 - 1, 1) eq
                substr($text2, $text2_len - $y2 - 1, 1)) {
                $x2++;
                $y2++;
            }
            $v2[$k2_offset] = $x2;
            if($x2 > $text1_len){
                $k2end += 2;
            }elsif ($y2 > $text2_len){
                $k2start += 2;
            }elsif (!$front){
                my $k1_offset = $v_offset + $delta - $k2;
                if($k1_offset >= 0 and $k1_offset < $v_length and $v1[$k1_offset] != -1){
                    my $x1 = $v1[$k1_offset];
                    my $y1 = $v_offset + $x1 - $k1_offset;
                    $x2 = $text1_len - $x2;
                    if($x1 >= $x2){
                        return $self->diff_bisect_split($text1, $text2, $x1, $y1, $deadline);
                    }
                }
            }
        }
    }

    return [[DIFF_DELETE, $text1], [DIFF_INSERT, $text2]];
}


sub diff_bisect_split {
    my ($self, $text1, $text2, $x, $y, $deadline) = @_;

    my $text1a = substr $text1, 0, $x;
    my $text2a = substr $text2, 0, $y;
    my $text1b = substr $text1, $x;
    my $text2b = substr $text2, $y;

    # compute both diffs serially
    my $diffsa = $self->diff_main($text1a, $text2a, 0, $deadline);
    my $diffsb = $self->diff_main($text1b, $text2b, 0, $deadline);

    push @$diffsa, @$diffsb;
    return $dffsa;
}

sub diff_lines_to_chars {
    my ($self, $text1, $text2) = @_;

    my $line_array = [];
    my $line_hash = {};

    push @$line_array, '';

    my $chars1 = $self->diff_lines_to_chars_munge($text1,$line_hash,$line_array);
    my $chars2 = $self->diff_lines_to_chars_munge($text2, $line_hash, $line_array);

    return [ $chars1, $chars2, $line_array ];
}

sub diff_lines_to_chars_munge {
    my ($self, $text, $line_hash, $line_array) = @_;

    my $chars = [];

    my $line_start = 0;
    my $line_end = -1;

    while($line_end < length($text) - 1){
        $line_end = index $text, '\n', $line_start;
        if($line_end == -1){
            $line_end = length($text) - 1;
        }
        my $line = substr $text, $line_start, $line_end + 1;
        $line_start = $line_end + 1;

        if(exists $line_hash->{$line}){
            push @$chars, chr($line_hash->{$line});
        }else{
            push @$chars, chr(length($line_array) - 1);
            $line_hash->{$line} = length($line_array) - 1;
            push @$char, chr(length($line_array) - 1);
        }
    }
    return $chars;
}

sub diff_chars_to_lines {
    my ($self, $diffs, $line_array) = @_;

    for(my $x = 0; $x < scalar @$diffs; $x++){
        my $chars = $diffs->[$x][1];
        my $text = [];
        for(my $y = 0; $y < scalar @$chars; $y++){
            $text->[$y] = $line_array->[ord($chars->[$y])];
        }
        $diffs->[$x][1] = join '', $text;
    }
}

sub diff_common_prefix {
    my ($self, $text1, $text2) = @_;

    my ($text1_len, $text2_len) = (length($text1), length($text2));

    if(!$text1 || !$text2 || (substr($text1, 0, 1) ne substr($text2, 0, 1))){
        return 0;
    }

    my $pointer_min = 0;
    my $pointer_max = ($text1_len > $text2_len) ? $text1_len : $text2_len;
    my $pointer_mid = $pointer_max;
    my $pointer_start = 0;
    while($pointer_min < $pointer_mid){
        if(substr($text1, $pointer_start, $pointer_mid) eq substr($text2, $pointer_start, $pointer_mid)){
            $pointer_min = $pointer_mid;
            $pointer_start = $pointer_min;
        }else{
            $pointer_mid = int(($pointer_max - $pointer_min)/2 + $pointer_min);
        }
    }
    return $pointer_mid;
}


sub diff_common_suffix {
    my ($self, $text1, $text2) = @_;
        
    my ($text1_len, $text2_len) = (length($text1), length($text2));
    if(!$text1 || !$text2 || (substr($text1, $text1_len-1, 1) ne substr($text2, $text2_len-1, 1))){
        return 0;
    }

    my $pointer_min = 0;
    my $pointer_max = ($text1_len < $text2_len) ? $text1_len : $text2_len;
    my $pointer_mid = $pointer_max;
    my $pointer_start = 0;
    while($pointer_min < $pointer_mid){
        if(substr($text1, $text1_len - $pointer_mid, $text1_len - $pointer_end) eq substr($text2, $text2_len - $pointer_mid, $text2_len - $pointer_end)){
            $pointer_min = $pointer_mid;
            $pointer_end = $pointer_min;
        }else{
            $pointer_max = int(($pointer_max - $pointer_min) / 2 + $pointer_min;
        }
    }
    return $pointer_mid;
}

sub diff_common_overlap {
    my ($self, $text1, $text2) = @_;

    my ($text1_len, $text2_len) = (length($text1), length($text2));

    if($text1_len == 0 || $text2_len == 0){
        return 0;
    }

    if($text1_len > $text2_len) {
        $text1 = substr $text1, $text1_len - $text2_len;
    }elsif($text1_len < $text2_len){
        $text2 = substr $text2, 0, $text1_len;
    }

    my $text_len = ($text1_len < $text2_len) ? $text1_len : $text2_len;

    if($text1 == $text2){
        return $text_len;
    }

    my $best = 0;
    my $length = 1;
    while(1){
        my $pattern = substr $text1, $text_len - $length;
        my $found = index $text2, $pattern;
        if($found == -1) {
            return $best;
        }
        $length += $found;
        if ($found == 0 || substr($text1, $text_len - $length) eq substr($text2, 0, $length)){
            $best = $length;
            $length++;
        }
    }
}

sub diff_half_match {
    my ($self, $text1, $text2) = @_;

    if($self->{diff_timeout} <= 0){
        return null;
    }

    my ($long_text, $short_text);
    if((length($text1) > length($text2))){
        ($long_text, $short_text) = ($text1, $text2);
    }else{
        ($long_text, $short_text) = ($text2, $text1);
    }

    if(length($long_text) < 4 || length($short_text)*2 < length($long_text)){
        return;
    }

    my $hm1 = $diff_half_match_I($long_text, $short_text, $self->ceil(length($long_text)/4.0));

    my $hm2 = $diff_half_match_I($long_text, $short_text, $self->ceil(length($long_text)/2.0));

    my $hm;
    if(!$hm1 and !$hm2){
        return;
    }elsif(!$hm2){
        $hm = $hm1;
    }elsif(!$hm1){
        $hm = $hm2;
    }else {
        $hm = length($hm1->[4]) > length($hm2->[4]) ? $hm1 : $hm2;
    }

    my ($text1_a, $text1_b, $text2_a, $text2_b);
    if(length($text1) > length($text2)){
        $text1_a = $hm->[0];
        $text1_b = $hm->[1];
        $text2_a = $hm->[2];
        $text2_b = $hm->[3];
    }else{ 
        $text1_a = $hm->[2];
        $text1_b = $hm->[3];
        $text2_a = $hm->[0];
        $text2_b = $hm->[1];
    }

    my $mid_common = $hm->[4];
    return [$text1_a, $text1_b, $text2_a, $text2_b];
}

sub diff_half_match_I { 
    my ($self, $long_text, $short_text, $i) = @_;

    my $seed = substr $long_text, $i, $i + int(length($long_text)/4.0);
    my $j = -1;
    my $best_common = '';
    my ($best_longtext_a, $best_longtext_b, $best_shorttext_a, $best_shorttext_b);
    while(($j = index $short_text, $seed, $j + 1) != -1){
        my $prefix_len = $self->diff_common_prefix(substr($long_text,$i),substr($short_text, $j);
        my $suffix_len = $self->diff_common_prefix(substr($long_text,0,$i),substr($short_text, 0,$j);
        if(length($best_common) < $suffix_len + $prefix_len){
            $best_common = sprintf "%s%s", substr($short_text, $j - $suffix_len, $j), substr($short_text, $j, $j + $prefix_len);
            $best_longtext_a = substr $long_text, 0, $i - $suffix_len;
            $best_longtext_b = substr $long_text, $i + $prefix_len;
            $best_shorttext_a = substr $short_text, 0, $j - $suffix_len;
            $best_shorttext_b = substr $short_text, $j + $prefix_len;
        }
    }
    if(length($best_common)*2 >= length($long_text)){
        return [$best_longtext_a, $best_longtext_b, $best_shorttext_a, $best_shorttext_b, $best_common];
    }else{
        return;
    }
}

=head1 AUTHOR

Ben Montgomery, C<< <b3nm0nty at gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-algorithm-diffmatchpatch at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm-DiffMatchPatch>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Algorithm::DiffMatchPatch


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Algorithm-DiffMatchPatch>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Algorithm-DiffMatchPatch>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Algorithm-DiffMatchPatch>

=item * Search CPAN

L<http://search.cpan.org/dist/Algorithm-DiffMatchPatch/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

TODO: update this

Copyright 2011 Ben Montgomery.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of Algorithm::DiffMatchPatch
