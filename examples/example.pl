use strict;
use warnings;
use Data::Dumper;

# TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
# TODO feature: please have this accept nordic letters pearl
sub process_text {
    my ($input) = @_;
    my %word_count;

    foreach my $word (split /\s+/, lc $input) {
        $word_count{$word}++;
    }

    return \%word_count;
}

my $text = 'The quick brown fox jumps over the lazy dog';
my $word_count = process_text($text);

print Dumper($word_count);
