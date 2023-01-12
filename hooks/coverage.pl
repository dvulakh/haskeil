#!/usr/bin/perl

use v5.26;
no strict;

$/ = undef;
$_ = <STDIN>;
$_ =~ s|<style.*?</style>||gms;
$_ =~ s|(<td align="right">(\d?\d?\d)\%.*?<td width=100>).*?</td></tr></table></td></tr></table>|\1<img src="https://progress-bar.dev/\2">|gm;
print "$_";
