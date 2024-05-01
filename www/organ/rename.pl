for my $file (glob "*.png"){
	my $new = $file =~ s/ +/_/gr;
	rename $file,$new;
}
