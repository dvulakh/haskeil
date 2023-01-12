#!/usr/bin/perl

use v5.26;
no strict;

$/ = undef;
$_ = <STDIN>;
$_ =~ s|<style.*?</style>||gms;
$_ =~ s|href="(.*?)"|href="https://htmlpreview.github.io/?https://github.com/dvulakh/haskeil/blob/main/coverage/\1"|mg;
$_ =~ s|(<td align="right">(\d?\d?\d)\%.*?<td width=100>).*?</td></tr></table></td></tr></table>|\1<img src="https://progress-bar.dev/\2">|gm;
print "$_";
