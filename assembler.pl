#!/usr/bin/env perl

use strict;
use warnings;
use v5.12;

# Some useful regexes
my $symbol_re = qr/[a-zA-Z_.$:][a-zA-Z0-9_.$:]*/;
my $jump_re = qr/J(?:GT|EQ|GE|LT|NE|LE|MP)/;
my $dest_re = qr/[AMD]{1,3}/;
my $cmd_re = qr/[-AMD01!+*|]+/;

# Run stuff here
my $parse = Parser->new();
while ($parse->hasMoreCommands) {
    my $command = $parse->advance();    # Get next command
    next unless $command; # Some lines are comments

    say "\t" . $command;
    my $type = $parse->commandType($command);   # Get its type

    if ($type eq 'A_COMMAND') {
        say $parse->symbol($command)
    }
    elsif ($type eq 'L_COMMAND') {
        say $parse->symbol($command)
    }
    elsif ($type eq 'C_COMMAND') {
        say $parse->dest($command)
        . " - Dest " . $parse->comp($command)
        . " - Comp " . $parse->jump($command)
        . " - Jump"
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

    if ($cmd =~ /^@($symbol_re)$/) {
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
