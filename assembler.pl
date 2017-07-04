#!/usr/bin/env perl

use strict;
use warnings;
use v5.12;

# Some useful regexes
my $symbol_re = qr/[a-zA-Z_.$:][a-zA-Z0-9_.$:]*/;
my $jump_re = qr/J(?:GT|EQ|GE|LT|NE|LE|MP)/;
my $dest_re = qr/[AMD]{1,3}/;
my $cmd_re = qr/[-AMD01!+*|]+/;
my $debug = 1;

# Run stuff here
my $parse = Parser->new();
while ($parse->hasMoreCommands) {
    my $command = $parse->advance();    # Get next command
    next unless $command; # Some lines are comments

    my $type = $parse->commandType($command);   # Get its type

    if ($type eq 'A_COMMAND') {
        say 0
        . Code::num_to_bin_str($parse->symbol($command), 15);
    }
    elsif ($type eq 'L_COMMAND') {
        say "L - " . $parse->symbol($command)
    }
    elsif ($type eq 'C_COMMAND') {
        say 111
        . Code::comp( $parse->comp($command) )
        . Code::dest( $parse->dest($command) )
        . Code::jump( $parse->jump($command) )
        ;
    }
    else {
        die "How the hell did you get here?!?!?"
    }
}

package Parser;
sub new {
    my $filename = $ARGV[0];
    die "No filename supplied" unless $filename;
    die "No such file" unless -e $filename;
    open(my $file, '<', $filename);
    my $content = bless [];
    @$content = <$file>;
    close $file;
    return $content;
}

sub hasMoreCommands {
    my $self = shift;
    return scalar @$self
}

sub advance {
    my $self = shift;
    my $next = shift @$self;
    $next =~ s#//.*##;  # Remove comments
    $next =~ s/^\s+//;  # Remove leading whitespace
    $next =~ s/\s+$//;  # Remove trailing whitespace
    return $next;
}

sub commandType {
    my ($self, $cmd) = @_;
    if ($cmd =~ /^\@$symbol_re/ || $cmd =~ /^\@\d+$/) {
        # An A-command can be either a symbol or a number prefix with @
        return 'A_COMMAND'
    }
    elsif ($cmd =~ /\($symbol_re\)/) {
        # An L-command is a symbol in parens
        return 'L_COMMAND'
    }
    elsif ($cmd =~ /^(?:$dest_re=)?$cmd_re(?:;$jump_re)?/) {
        # A C-command is dest=comp;jump but either dest or jump can be missing
        return 'C_COMMAND'
    }
    else {
        # Just because something doesn't make it here doesn't mean it's valid
        # But if it gets here, it definitely isn't!
        die "'$cmd' is an unrecognised command"
    }
}

sub symbol {
    my ($self, $cmd) = @_;

    if ($cmd =~ /^\@($symbol_re)$/) {
        return $1
    }
    elsif ($cmd =~ /^\@(\d+)$/) {
        return $1
    }
    elsif ($cmd =~ /^\(($symbol_re)\)$/) {
        return $1
    }
}

sub dest {
    my ($self, $cmd) = @_;

    if ($cmd =~ /($dest_re)=.+/) {
        return $1
    }
    return ''
}

sub comp {
    my ($self, $cmd) = @_;

    $cmd =~ /^(?:$dest_re=)?($cmd_re)(?:;$jump_re)?/;
    return $1
}

sub jump {
    my ($self, $cmd) = @_;

    if ($cmd =~ /;($jump_re)$/) {
        return $1
    }
    return ''
}

package Code;

sub num_to_bin_str {
    my ($num, $size) = @_;
    my $str = '';
    while ($size--) {
        if ($num % 2) {
            $str = "1$str";
        }
        else {
            $str = "0$str";
        }
        $num = $num >> 1 if $num;
    }
    return $str;
}

sub dest {
    my $code = shift;
    return '000' unless $code;
    my $val = 0;
    $val += 1 if $code =~ 'M';
    $val += 2 if $code =~ 'D';
    $val += 4 if $code =~ 'A';
    return num_to_bin_str($val, 3);
}

sub comp {
    my $code = shift;
    my %mnemonics = (
        '0'   => '0101010',
        '1'   => '0111111',
        '-1'  => '0001010',
        'D'   => '0001100',
        'A'   => '0110000',
        '!D'  => '0001101',
        '!A'  => '0110001',
        '-D'  => '0001111',
        '-A'  => '0110011',
        'D+1' => '0011111',
        'A+1' => '0110111',
        'D-1' => '0001110',
        'A-1' => '0110010',
        'D+A' => '0000010',
        'D-A' => '0010011',
        'A-D' => '0000111',
        'D&A' => '0000000',
        'D|A' => '0010101',
        'M'   => '1110000',
        '!M'  => '1110001',
        '-M'  => '1110011',
        'M+1' => '1110111',
        'M-1' => '1110010',
        'D+M' => '1000010',
        'D-M' => '1010011',
        'M-D' => '1000111',
        'D&M' => '1000000',
        'D|M' => '1010101',
    );
    return $mnemonics{$code};
}

sub jump {
    my $code = shift;
    return '000' unless $code;
    my %jumps = (
        JGT => 1,
        JEQ => 2,
        JGE => 3,
        JLT => 4,
        JNE => 5,
        JLE => 6,
        JMP => 7
    );
    return num_to_bin_str($jumps{$code}, 3);
}
