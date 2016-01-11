	subroutine dumplevels

c routine to (so far) dump 1/3 octave band levels created by SUBF7AD (called
c during ADLOUD)			BMS July 25 1990

c In case we want to save the 1/3 octave band levels - the only reason
c we could be here so far!
	logical ldump
	integer dumpunit
	common /dump/ ldump,dumpunit

c L is the aray of 1/3 octave band levels (1 to 40 are used for now)
c SPL will be the loudness value after going through PL.FOR
       real l(43),spl
       common /crux/ spl,l

c It would be nice to have this file in a format that Dave's METRIC, etc could
c read.  The problem is that I have 1 spectrum per signal, not N 1/2 spectra,
c and I have multiple signals in one file - but judicious editing of the file
c I do produce should be possible.

c	print *,dumpunit
c	print *,L
	write (dumpunit,*) (l(ljk),ljk=1,43)

	return
	end
