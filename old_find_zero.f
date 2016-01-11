c new version of bmin (at the end of this source code) to handlke a whole
c succession of equal values at the minimum (not likely in the booth, but does
c happen with our latest idealized booms)
c
	subroutine old_find_zero(numb,srate,arry)
c
c For use with both Full-frequency and High Pass filtered signals (eg measured 
c with non-DC mike).  
c This process assumes we have a signal that crosses zero - ie one that has negligible
c D.C. shift so if there was a noticeable offset, it has been extracted already
c		created from FLIP.PRO on GRAF
c                                                BMS Feb 20 1991
c
c  Change (correct?) calculation for TIMENOMZERO - BMS Aug. 2 1991
c Handle the case that 1st peak = 2nd peak and neither is zero - BMS Mar 16 1992
c For a U-wave (aka H.P. filtered), define IZEROBETWEEN as the first zero 
c crossing after the first peak (of the inverted wave, if neccessary) - Mar 18 1992

c Increase noinfl (again) - the possible size of the input file - July 16 2002
c
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

c (1) To handle D.C. problem (more important for full N-waves), check the 
c     first 100 points for mean value and subtract it from the whole signal.
c (Done in the calling routine)
c
c (2) Find absolute maximum of signal.
c
 2	maxsig=bmax(arry,numb,indexmax)
	minsig=bmin(arry,numb,indexmin)
	if (abs(minsig) .gt. abs(maxsig)) then 
	   peaksig = minsig
	   indexpeak = indexmin
	   iwho = -1
	else
	   peaksig = maxsig
	   indexpeak = indexmax
	   iwho = 1
	endif
c
c (3) find the zero crossing before this peak
c (4) find the maximum and minimum before this peak and
c     the bigger in absolute value of this maximim and minimum
c
c (3) look at the signal before the peak (which may be positive or negative)
 31	i=indexpeak-1
	if (iwho .eq. 1) then
	   do while ((i .gt. 1) .and. (arry(i-1) .gt. 0.0))
	      i=i-1
	   enddo
	else
	   do while ((i .gt. 1) .and. (arry(i-1) .lt. 0.0))
	      i=i-1
	   enddo
	endif
	izerobefore=i
c (4) find max and min before the main peak
 41	maxbefore=bmax(arry,izerobefore,indexmaxbefore)
	minbefore=bmin(arry,izerobefore,indexminbefore)
c     find the bigger 
	if (abs(minbefore) .gt. abs(maxbefore)) then
	   peakbefore = minbefore
	   indexpeakbefore = indexminbefore
	else
	   peakbefore = maxbefore
	   indexpeakbefore = indexmaxbefore
	endif
c
c (3) find the zero crossing after the main peak
c (4) find the maximum and minimum before this peak and
c     the bigger in absolute value of this maximim and minimum
c
c (3) look at the signal after the peak
 32	i=indexpeak+1
	if (iwho .eq. 1) then
	   do while ((i .lt. numb-1) .and. (arry(i+1) .gt. 0.0))
	      i=i+1
	   enddo
	else
	   do while ((i .lt. numb-1) .and. (arry(i+1) .lt. 0.0)) 
	      i=i+1
	   enddo
	endif
	izeroafter=i
c (4) find max and min after the main peak
 42	maxafter=bmax(arry(izeroafter),(numb+1-izeroafter),indexmaxafter)
	indexmaxafter=indexmaxafter+izeroafter-1
	minafter=bmin(arry(izeroafter),(numb+1-izeroafter),indexminafter)
	indexminafter=indexminafter+izeroafter-1
c     find the bigger 
	if (abs(minafter) .gt. abs(maxafter)) then
	   peakafter = minafter
	   indexpeakafter = indexminafter
	else
	   peakafter = maxafter
	   indexpeakafter = indexmaxafter
	endif
c
c (5) find the bigger of the peak before and the peak after
c				greater than or equal to 
 5	if (abs(peakbefore) .ge. abs(peakafter)) then
	   otherpeak = peakbefore
	   indexotherpeak = indexpeakbefore
c other peak is before main
	   iwhere=1
	else if (abs(peakbefore) .lt. abs(peakafter)) then
	   otherpeak = peakafter
	   indexotherpeak = indexpeakafter
c other peak is after main
	   iwhere=-1
c else they are equal - can be OK if they're not zero; make first the main one
c this code is ridiculous - must be a remnant from the distant past? (BMS, July 9 2004)
c firstpeak doesn't exist that I can find now
	else if (firstpeak .ne. 0.0) then
	   peaksig = firstpeak
	   indexpeak = indexfirstpeak
	   otherpeak = secondpeak
	   indexotherpeak = indexsecondpeak
	   iwhere = -1
	else
c does this mean anything?   Never seems to happen?
	   print *,'This signal has only one peak.  Sorry'
c other is not there so panic --- return
	   iwhere=0
	endif
c
c (6) find the sign of this other peak and the main peak
c
	if ((peaksig .lt. 0.0) .and. (otherpeak .lt. 0.0)) then
c (7a) If both peaks are negative, invert the signal (well, why not?)
 6	   do i=1,numb
	      arry(i) = -arry(i)
	   enddo
	   peaksig = -peaksig
	   otherpeak = -otherpeak
c	   minsig = -minsig
	   iwho = -iwho
c (7) this is H.P. filtered - or a U-wave
	   ifilter = 1
	else
c (7) this too is H.P. filtered or a U-wave
	   if ((peaksig .gt. 0.0) .and. (otherpeak .gt. 0.0)) then
	      ifilter = 1
c (7) this is NOT filtered - this is a complete N-wave
	   else
	      ifilter = 0
	   endif
	endif
c
c so now our signal is the right way up and the peaks are maxima
c or one up, one down (one max., one min.)
c
c If there was only one clear peak, give up now
	if (iwhere .ne. 0) then
c (8) Find the midpoint between the two peaks for both kinds of waves
 8	   indexmiddle=(indexpeak+indexotherpeak)/2
c
 9	   if (ifilter .eq. 0) then
c (9) Find the zero-crossing between the two peaks for the complete N-wave
c (start from the higher one and move down the trace)
	         if (iwho .eq. 1) then
	            i=indexpeak
	            iend=indexotherpeak
	         else
	            i=indexotherpeak
	            iend=indexpeak
	         endif
	print *,'indexpeak, indexotherpeak  ',indexpeak, indexotherpeak
	         istep = -iwhere*iwho
c shoot, I cannot get one expression that handles all cases!
	         if (istep .gt. 0) then
	            imore = iend
	            iless = i
	         else
	            iless = iend
	            imore = i
	         endif
	         do while ((i .le. imore) .and. (i .ge. iless) .and. 
     $                  (arry(i) .gt. 0.0))
	            i=i+istep
	         enddo
	         if ( (arry(i) .eq. 0.0) .or. (i .eq. 1) .or.
     $        ( (i .gt. 1) .and. (arry(i-1) .lt. 0.0) ) ) then
	            izerobetween = i
	         else
	            izerobetween = i-1
	         endif
	print *,'izerobetween  ',izerobetween
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
c (9b) Find the first zero crossing after the first of the two peaks for the 
c      filtered N-wave (well, why not!)
	      if (indexpeak .le. indexotherpeak) then
	         i = indexpeak
	      else
	         i = indexotherpeak
	      endif
	      do while (arry(i) .gt. 0.0)
	         i = i+1
	      enddo
	      izerobetween = i
	   endif
c
	else
	   print *,'There was only one peak to this wave'
	endif
c
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
        if (ninfl .gt. 2) then
	  do i=2,ninfl
	    if ((press(i-1) .gt. 0.0) .and. (press(i) .le. 0.0)) then
	      if (press(i) .eq. 0.0) then
	         timenomzero=atime(i)
	      else
	         timenomzero = atime(i-1) + (atime(i)-atime(i-1)) * 
     $                         ( press(i-1) / (press(i-1)-press(i)) )
c     $                         ( press(i) / (press(i-1)-press(i)) )
	      endif
	      go to 11
	    endif
	  enddo
c what index in the (real) time array is closest to this nominal time?
 11       indexnomzero = indextime(timenomzero,srate,numb,idelay_used)

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
        return
        end


        integer function indextime(timesec,srate,numb,idelay_used)

c to convert from some nominal time to the nearest array point (remember
c the delay time)

c in order to get from index in signal array to time, remember
c	   time(i) = (1000./srate)*(i-1)

	if (timesec .lt. 0.0) then
	   indextime = 1
	else
c convert to msecs from secs.
	   timemsec = timesec*1000.
	   factort = 1000./srate
c what index in the time array is closest to this time?
	   i=1
	   timei = factort*i
	   do while ((i .lt. numb) .and. (timei .lt. timemsec))
	      i=i+1
	      timei = factort*i
	   enddo
	   indextime = i + idelay_used
	endif

	return
	end


	real function bmax(array,nmax,indexmax)

	real array(*)

	bmax = array(1)
	indexmax = 1
	do i=2,nmax
	   if (array(i) .gt. bmax) then
	      bmax = array(i)
	      indexmax = i
	   endif
	enddo

	return
	end

c	real function old_bmin(array,nmax,indexmin)
c
c	real array(*)
c
c	bmin = array(1)
c	indexmin = 1
c	do i=2,nmax
c	   if (array(i) .lt. bmin) then
c	      bmin = array(i)
c	      indexmin = i
c	   endif
c	enddo
c
c	return
c	end

	real function bmin(array,nmax,indexmin)

	real array(*)
	logical flag

	bmin = array(1)
	indexmin = 1
	flag = .false.
	do i=2,nmax
	   if (array(i) .lt. bmin) then
	      bmin = array(i)
	      indexmin = i
	      icontiguous = 0
	   else if (array(i) .eq. bmin) then
              icontiguous = icontiguous+1
	      flag = .true.
	   else if (flag) then
	      flag = .false.
	      indexmin = indexmin + int(icontiguous/2)
	   endif
	enddo

	return
	end

