my $inputfile="symplur_amia2014_raw.txt";
open(INPUT_FILE, "<$inputfile");
open(OUT_FILE, ">symplur_amia2014_clean.txt");
while(<INPUT_FILE>){
 if($.%2!=1){$_=~s/\n/\t/;}
 $_=~s/ReplyRetweetFavorite//;
 $_=~s/\t\t/\t/;
 if($.==1){next}
 print OUT_FILE $_;
}
close(INPUT_FILE);
close(OUT_FILE);

