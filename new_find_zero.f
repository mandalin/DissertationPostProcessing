	subroutine new_find_zero(numb,srate,arry)
c
c made from Old find_zero but with an algorithm based on the nominal waveform
c to define the two peaks                       Aug 20 1991
c
c Handle the case that 1st peak = 2nd peak and neither is zero - BMS Mar 16 1992
c Handle finding timenomzero for case where front peak is -ve and back one
c    is +ve (which I thought incorrectly I already handled!) - BMS June 9 1994

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

c initialize
	indexmiddle = 0
	izerobetween = 0
	indexminmiddle = 0
	indexnomzero = 0
	indexminmiddle = 0
	indexpeak = 0
	indexotherpeak = 0
	indexstart = 0
	indexfirstknee = 0
	indexend = 0
	indexlastknee = 0

c What does nominal waveform tell us about this wave?  Look at press,atime
c to find nominal zero crossing, and tau (first rise time) and taumx (time to
c maximum)

 10     timenomzero=0.0
        tfirstknee = 0.0
        pfirstknee = 0.0
        tlastknee = 0.0
        plastknee = 0.0
        tfirstpeaknom = 0.0
        pfirstpeaknom = 0.0
        tlastpeaknom = 0.0
        plastpeaknom = 0.0
        tstartnom = 0.0
        tendnom = 0.0
	iz = 0
        if (ninfl .gt. 2) then
	  do i=ninfl-1,1,-1
	    if ((press(i) .eq. 0.0) .and. (press(i+1) .eq. 0.0)) then
c pass on by
	      continue
	    else if (((press(i) .ge. 0.0) .and. (press(i+1) .lt. 0.0)) 
     $  .or. ((press(i) .le. 0.0) .and. (press(i+1) .gt. 0.0))) then
	      if (press(i) .eq. 0.0) then
	         if (press(i-1) .eq. 0.0) then
	            ztime= (atime(i) + atime(i-1))/2.
	         else
	            ztime = atime(i)
	         endif
	      else
	         ztime = atime(i) + (atime(i+1)-atime(i)) * 
     $                         ( press(i) / (press(i)-press(i+1)) )
	      endif
	      print *,'zero crossing time = ',ztime
c	      go to 11
	      iz = iz+1
c what if there is (are?) more than one? Keep looking.
	      if (iz .eq. 1) then
	        timenomzero=ztime
	        iz = 2
	      endif
	    endif
	  enddo
c what index in the (real) time array is closest to this nominal time?
 11       indexnomzero = indextime(timenomzero,srate,numb,idelay_used)

          if (indexnomzero .le. 0 .or. indexnomzero .ge. numb) then
                 print *,' New_find_zero bombed'
                 stop
          endif

          i = 1
          do while ( (i .lt. ninfl) .and. (press(i) .eq. 0.0) )
            i = i+1
          enddo
          if (i .gt. 1) then
c NOMINAL start time is atime(i-1)
             tstartnom = atime(i-1)
          else
             tstartnom = 0.0
          endif
          indexstart = indextime(tstartnom,srate,numb,idelay_used)
          pfirstknee = press(i)
          tfirstknee = atime(i)
          indexfirstknee = indextime(tfirstknee,srate,numb,idelay_used)

c NOMINAL time/actual index of last segment
          i = ninfl
          do while ( (i .gt. 1) .and. (press(i) .eq. 0.0) )
            i = i-1
          enddo
          if (i .lt. ninfl) then
c end time is atime(i+1)
             tendnom = atime(i+1)
          else
c something is totally screwy
             tendnom = atime(ninfl)
          endif
          indexend = indextime(tendnom,srate,numb,idelay_used)
          plastknee = press(i)
          tlastknee = atime(i)
          indexlastknee = indextime(tlastknee,srate,numb,idelay_used)
      
c time/press. of nominal peaks (remember, this is an N-wave, so it goes + and -)
          maxnomsig=bmax(press,ninfl,indexnommax)
          minnomsig=bmin(press,ninfl,indexnommin)
          if (indexnommax .le. indexnommin) then
c MAX occurs before MIN which is the conventional way
             tfirstpeaknom = atime(indexnommax)
             pfirstpeaknom = press(indexnommax)
             tlastpeaknom = atime(indexnommin)
             plastpeaknom = press(indexnommin)
          else
             tfirstpeaknom = atime(indexnommin)
             pfirstpeaknom = press(indexnommin)
             tlastpeaknom = atime(indexnommax)
             plastpeaknom = press(indexnommax)
          endif
     
        endif
c
c (1) Find absolute maximum of first part of signal.
c     Use arry only up to indexnomzero

 1	maxsig=bmax(arry,indexnomzero,indexmax)
	minsig=bmin(arry,indexnomzero,indexmin)
	if (abs(minsig) .gt. abs(maxsig)) then 
	   firstpeak = minsig
	   indexfirstpeak = indexmin
	   ifirst = -1
	else
	   firstpeak = maxsig
	   indexfirstpeak = indexmax
	   ifirst = 1
	endif
c
c (2) Find absolute maximum of first part of signal.
c     Use arry from indexnomzero to numb
        iuse = numb-indexnomzero + 1

 2	maxsig=bmax(arry(indexnomzero),iuse,indexmax)
	minsig=bmin(arry(indexnomzero),iuse,indexmin)
	if (abs(minsig) .gt. abs(maxsig)) then 
	   secondpeak = minsig
c remember we're offset by indexnomzero-1 samples
	   indexsecondpeak = indexmin + indexnomzero-1
	   isecond = -1
	else
	   secondpeak = maxsig
	   indexsecondpeak = indexmax + indexnomzero-1
	   isecond = 1
	endif
c
c find the bigger of the first and second peaks
c 
	iwhere = 1
 5	if (abs(firstpeak) .gt. abs(secondpeak)) then
	   peaksig = firstpeak
	   indexpeak = indexfirstpeak
	   otherpeak = secondpeak
	   indexotherpeak = indexsecondpeak
	   iwhere = ifirst
	else if (abs(firstpeak) .lt. abs(secondpeak)) then
	   peaksig = secondpeak
	   indexpeak = indexsecondpeak
	   otherpeak = firstpeak
	   indexotherpeak = indexfirstpeak
	   iwhere = isecond
c else they are equal - can be OK if they're not zero; make first the main one
	else if (firstpeak .ne. 0.0) then
	   peaksig = firstpeak
	   indexpeak = indexfirstpeak
	   otherpeak = secondpeak
	   indexotherpeak = indexsecondpeak
	   iwhere = ifirst
	else
c does this mean anything?
	   print *,'This signal has one or no peak.  Sorry'
c other is not there ... panic --- return
	   iwhere=0
	endif
c
c (6) Use the sign of this other peak and the main peak
c
	if (ifirst .eq. isecond) then
	   if (ifirst .eq. -1) then
c (7a) If both peaks are negative, invert the signal (well, why not?)
 6	      do i=1,numb
	         arry(i) = -arry(i)
	      enddo
	      peaksig = -peaksig
	      otherpeak = -otherpeak
	   endif
c (7) this is H.P. filtered
	   ifilter = 1
c this is NOT filtered - this is a complete N-wave
	else
	   ifilter = 0
	endif
c
c so now our signal is the right way up and the peaks are both maxima
c or one up, one down (one max., one min.)
c
c If there was only one clear peak, give up now
	if (iwhere .ne. 0) then
c (8) Find the midpoint between the two peaks for both kinds of waves
 8	   indexmiddle=(indexpeak+indexotherpeak)/2
c
 9	   if (ifilter .eq. 0) then

c (9) Find the zero-crossing between the two peaks for the complete N-wave

	         istart=indexfirstpeak
	         istep = 1
	         iend=indexsecondpeak-istep

c stop at the point before (in time) the zero crossing, if you can't stop 
c right on it
	         do i=istart,iend,istep
	            if ( (arry(i) .ge. 0.0) .and.
     $                            (arry(i+ifirst) .lt. 0.0) ) then
	               if (arry(i) .eq. 0.0) then
	                  izerobetween = i
	               else if (iwhere .eq. -1) then
	                  izerobetween = i
	               else
	                  izerobetween = i-1
	               endif
	               go to 900
	            endif
	         enddo
 900	         continue
	   else
c (9) Find the minimum between the two peaks for the filtered N-wave
	      if (indexpeak .gt. indexotherpeak) then
        	 minmiddle = bmin(arry(indexotherpeak),
     $                (indexpeak-indexotherpeak),indexminmiddle)
	         indexminmiddle=indexminmiddle+indexotherpeak
	      else
	         minmiddle = bmin(arry(indexpeak),
     $                (indexotherpeak-indexpeak),indexminmiddle)
	         indexminmiddle=indexminmiddle+indexpeak
	      endif
	print *,'indexpeak, indexotherpeak  ', indexpeak, indexotherpeak
	print *,'indexminmiddle  ',indexminmiddle
	   endif
c
	else
	   print *,'There was only one peak to this wave'
	endif
c
c
        return
        end
