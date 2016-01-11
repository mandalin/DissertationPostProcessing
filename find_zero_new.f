	subroutine find_zero(numb,srate,arry)
c
c If we have a signal that's very complicated (so it's hard to define the front and rear
c portions) and it's got lots of "nominal" points (ninfl is large) then the new_find_zero 
c algorithm is screwed up, so go back to old_find_zero.  In fact, I don't know why I keep
c new_find_zero, except that I had a reason for creating it once. But that was 12 years
c ago, and I know it doesn't work sometimes, so reject it - BMS Feb 20 2003
c
c
c For use with both Full-frequency and High Pass filtered signals (eg measured 
c with non-DC mike).  
c This process assumes we have a signal that crosses zero - ie one that has negligible
c D.C. shift so if there was a noticeable offset, it has been extracted already
c		created from FLIP.PRO on GRAF
c                                                BMS Feb 20 1991
c
c  Change (correct?) calculation for TIMENOMZERO - BMS Aug. 2 1991
c
c Use a different algorithm for finding the two peaks, based on a
c knowledge of the duration of the wave-form, the APRESS and TIME arrays
c and the filter delay (if any)

c Increase noinfl (again) - the possible size of the input file - July 16 2002

        real arry(*)
c
c pity idelay_used is in this common block, but so it goes
        logical linv_used
        character*80 inv_name, orig_name
        character*8 ntime
        character*9 ndate
        common /footer/ linv_used, inv_name, orig_name, srate_inv_used,
     $             idelay_used, ntime, ndate
c
c pass in common the NINFL time and pressure pairs used to define this wave
        parameter (noinflp = 10000)
        real*4 press(noinflp), atime(noinflp)
        common /boomcom/ press,atime,ninfl,srate_boom
c
        common /flippts/ ifilter,indexmiddle,izerobetween,
     $       indexminmiddle,indexnomzero,indexpeak,indexotherpeak,
     $       indexstart,indexfirstknee,indexend,indexlastknee,
     $       tfirstknee,pfirstknee,tlastknee,plastknee,
     $	     tfirstpeaknom,pfirstpeaknom,tlastpeaknom,plastpeaknom,
     $	     tstartnom,tendnom
c
	real maxafter,minafter,minsig,maxsig,minbefore,maxbefore,
     $                  minmiddle
c
c
	if (ninfl .gt. 0 .and. ninfl .lt. 1) then
	   call new_find_zero(numb,srate,arry)
	else
	   call old_find_zero(numb,srate,arry)
	endif

	return
	end
