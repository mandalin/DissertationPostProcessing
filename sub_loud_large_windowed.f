	subroutine sub_loud(outshort,outunit,signal,number,srate,ipres,
     $    name$,ref_volt,equiv_db,equiv_press,dcoffset,ierr)
c
c Update psftopa to 47.88 from 47.85 - BMS Dec 14 2005
c Also update reference pressure for psf to be consistent with psftopa
c
c put in a Hanning window - BMS Sep 19 2005
c	Pass in (in common) the length of the Hanning window taper - BMS Nov 3 2005
c Remove some of the less meaningful output in short file - BMS Nov 3 2005
c
c put array size parameters in include file - BMS Aug 4 2005

c name$ passed in as 80 characters - BMS Aug 23 2004
c Increase noinfl (again) - the possible size of the input file - July 16 2002

c add the two time-constant model (tc1,tc2 and sub_expav2) - Oct 24 1994
c add /boomcom/ common block and write out parameters of boom instead of some 
c of the pressure values we never use (tailored for MV option) - Oct 6 1993
c add multi-peak option (LMV) - Sept 7 1993
c sub_loud enlarged - increase storage size from 32768 to 65536 - April 29 1993
c             and to 131072 - July 19 1993
c (some versions of the calling routine have longer signals than the others!)
c routine to convert A/D (or D/A) numbers ("volts") into pressures in
c Pascals and to calculate loudnesses using SUBF7X offspring
c                                                BMS June 26 1990
c Unit for short_form output changed from 2 to outshort and put in the 
c calling arguments - BMS Jan 25 1991
c Split off the end into a separate routine, in preparation for the
c asymmetric boom analysis - BMS Feb 20 1991
c Add Zwicker loudness - BMS May 28 1991
c Use Find_zero to get the max and min points, rather than just finding
c the max. and min. of signal - BMS 17 Sept 1991
c Zero out the a array after filling it with the useful part of signal 
c                                          - BMS 19 Feb 1991 (1992?)
c add PNL output - bms 20 oct 1992
c3 add dBA from 1/3 ob data for now - BMS dec 2 1992
c3 but don't write it out - BMS dec 7 1992

	real signal(1)
	integer outunit,outshort
	character*80 name$

c We have room for 2**17 data points
c        parameter (exponent = 17)
c        parameter (power2 = 131072)
c        parameter (halfp2a1 = 65537)
c parameters now in include file
        include 'sigpair_size.inc'

        real a(power2),unp(power2)
        common /nwavearray/ a,unp
c
c ref. pressure in Pascals and psf
        real pref(2)
        data pref /2.0e-5,.417973e-6/
c conversion factor psf to Pa
c Dec 14 2005 Update psf to Pa value from 47.85 to 47.88; use this value to
c recalculate pref(2) later on
	parameter (psftopa=47.88)
c
c If this is an asymmetric signal we want to play games to separate out the 
c front and back ends.  Let's assume the request for an asymmetric analysis is
c in a new common block, and it's true if we want it.  lfull tells whether it's
c a fullwave or a half wave analysis and lmv tells whether we want multiple
c events analysed
	logical lasym,lfull,lmv
	common /yessym/ lasym,lfull,lmv
c
c padding parameters
        common /padding/ ifrontpad,ibackpad

c Nominal times and indeces thereof
        common /flippts/ ifilter,indexmiddle,izerobetween,
     $       indexminmiddle,indexnomzero,indexpeak,indexotherpeak,
     $       indexstart,indexfirstknee,indexend,indexlastknee,
     $       tfirstknee,pfirstknee,tlastknee,plastknee,
     $	     tfirstpeaknom,pfirstpeaknom,tlastpeaknom,plastpeaknom,
     $	     tstartnom,tendnom
c
c pass in common the NINFL time and pressure pairs used to define this wave
        parameter (noinflp = 10000)
        real*4 press(noinflp), atime(noinflp)
        common /boomcom/ press,atime,ninfl,srate_boom
c
c two time-constant model parameters
	common /twotc/ tc1, tc2

c Hanning window taper length
	common /hantaper/ taper_length
c
c READ routines had room only for 32000 points;
c SUBF7X has room for 32768 points;
c array A and array SIGNAL need not have powers of two points for FFTSC to work!
c
c update reference pressure for psf to be consistent with psftopa
        pref(2) = pref(1)/psftopa
c not but what I can't see it used anywhere!
c 
c SIGNAL is in "volts"; convert it to Pascals
c where REF_VOLT = EQUIV_PRESS (in Pascals)
	convert = equiv_press/ref_volt
c
	do index = 1,number
	   a(index) = signal(index) * convert
	enddo
c paranoia, paranoia!
	do index = number+1, power2
	   a(index) = 0.0
	enddo

c
c what's the peak pressure?  (Careful with the D.C. offset)
	dconvert = dcoffset*convert
	write (outunit,*) ' '
	write (outunit,*) ' Calibration. D.C. offset is ',
     $					dconvert,' Pascals'
	write (outunit,*) ' '
c Signal start ; for acoustic signals (picked up by the mike in the booth, this
c is = average of first 100 samples (~3msecs of data) which is a measure of the 
c D.C. offset. For numbers got out of a signal file, there is no D.C. offset,
c and the start is probably at 0, 'cos there's no time delay.
	if (ipres .gt. 0) then
	   frontend = a(1)
	   do next=2,100
	      frontend = frontend+a(next)
	   enddo
	   frontend = frontend/100.
	else
	   frontend = 0.0
	endif
	write (outunit,*) ' Average Initial value is ',frontend,' Pascals'
c
c just for jollies, let's try D.C. correcting the WHOLE signal
	if (ipres .gt. 0) then
	   do next = 1,number
	      a(next) = a(next) - frontend
	   enddo
	endif
c
c try a Hanning window (courtesy of Ken Plotkin) - for the moment do it here, 
c so the asymm booms will have the start and ends windowed too, though inverted.
	if (taper_length .gt. 0) then
	   numtaper = min(float(number)/11.,taper_length)
	   print *,numtaper
	   do k=0,numtaper
	      factr=float(k)/float(numtaper)
	      factr=0.5*(1.-cos(3.141592653*factr))
	      a(1+k)=a(1+k)*factr
	      a(number-k)=a(number-k)*factr
           enddo
	endif
c
c
c For the most asymmetrical booms, the rear end (post boom) of the signals
c that have been processed with Don's filter can rise to levels greater than
c the initial maximum.  Hence a straight max., min. search is not effective
c to find the two peaks.  We'll have to use the knowlege (if it exists) of
c the shape of the unprocessed boom, in the same way that find_zero does.
c The front end correction (weird as it is) still applies (if it ever did).

c so process the signal as it stands
c store the unprocessed wave
	nunp = number
	do i=1,power2
	   unp(i) = a(i)
	enddo
c find times of various points and the associated delta Ps. (Still use same
c for MV option - even though idelay_used is WRONG!!!*******************)
	call find_zero(number,srate,a)
c Use these findings to calculate TAU times and pressures
c including man. and min.  I have two peaks but which is max & which min?
c
	sigmax = a(indexpeak)
	sigm2 = a(indexotherpeak)
	if (sigmax .lt. sigm2) then
	   sigmin = sigmax
	   sigmax = sigm2
	else
	   sigmin = sigm2
	endif

	write (outunit,*) ' '
	write (outunit,*) '       Raw MAX. and MIN. are ',
     $			sigmax,sigmin,' Pascals'

	patopsf = 1./psftopa
	sigmax = sigmax - (frontend)
	sigmin = sigmin - (frontend)
	write (outunit,*) ' '
	write (outunit,*) ' FrontEnd Corrected MAX. and MIN. are ',
     $			sigmax,sigmin,' Pascals'
	write (outunit,*) ' FrontEnd Corrected MAX. and MIN. are ',
     $		sigmax*patopsf,sigmin*patopsf,' psf'
	if (sigmax .eq. 0.0) then
	   dbmax = 0.0
	else
	   dbmax = 20.*alog10(abs(sigmax)/pref(1))
	endif
	if (sigmin .eq. 0.0) then
	   dbmin = 0.0
	else
	   dbmin = 20.*alog10(abs(sigmin)/pref(1))
	endif
	write (outunit,*) ' FrontEnd Corrected MAX. and MIN. are ',
     $			dbmax,dbmin,' dB'
c
c
c The pressure at the nominal start time; ditto at nominal first knee; ditto
c at nominal maximum; ditto at true maximum (and the same at the other end of 
c the boom)
	if (ninfl .gt. 2) then
	   pstart = a(indexstart)
	   pfirstknee = a(indexfirstknee)
	   pend = a(indexend)
	   plastknee = a(indexlastknee)
	else
	   pstart = 0
	   pfirstknee = 0
	   pend = 0
	   plastknee = 0
	endif
	write (outunit,*) ' '
	write (outunit,*) ' FEC pressures (psf) at start,
     $ first knee,last knee and end, all nominal times'
	write (outunit,*) pstart*patopsf,pfirstknee*patopsf,
     $         plastknee*patopsf,pend*patopsf 
c
c Do loudness on the original waveform (subf7 pads the A array)
	lfull = .true.
	print *,'        Just before Calling subf7adw_main in sub_loud'
	call subf7adw_main(outunit,number,srate,dbpl,dbc,dba,
     $                                dblin,dbz0,dbz1,dbpnl,dba3)
c3     $                                dblin,dbz0,dbz1,dbpnl)
	print *,'        Just after Calling subf7adw_main in sub_loud'
c
c	print *,'Enter time constants (in secs) for this run'
c	accept *,tc1,tc2
c
	call sub_expav2l(tc1,tc2,srate,number,a,sigoutmax)
c
c write out the short form results
	if (lmv) then
	   if ((press(5)-press(4)) .ne. 0.0) then
	      write (outshort,991) ipres,name$,dbpl,dbc,dba,dblin,dbz0,
     $          dbz1,dbpnl,sigmax*patopsf,sigmin*patopsf,
c need first rt, second rt, separation between their starts and the
c ratio of their pressure differences (and I know where they are in the 
c array of inflection points (I think))
     $		(1000.*(atime(3)-atime(2))),
     $		(1000.*(atime(5)-atime(4))),
     $		(1000.*(atime(4)-atime(2))),
     $		(press(3)-press(2))/(press(5)-press(4))
	   else
	      write (outshort,991) ipres,name$,dbpl,dbc,dba,dblin,dbz0,
     $          dbz1,dbpnl,sigmax*patopsf,sigmin*patopsf
c need first rt, second rt, separation between their starts and the
c ratio of their pressure differences (and I know where they are in the 
c array of inflection points (I think))
C     $		,(1000.*(atime(3)-atime(2))),
C     $		(1000.*(atime(5)-atime(4))),
C     $		(1000.*(atime(4)-atime(2))),
C     $		0.00
	   endif
	else
	   write (outshort,99) ipres,name$,dbpl,dbc,dba,dblin,dbz0,
     $          dbz1,dbpnl,
     $		sigmax*patopsf,sigmin*patopsf
C    $        ,pstart*patopsf,
C    $		pend*patopsf,pfirstknee*patopsf,plastknee*patopsf 
C     $          ,sigoutmax
c3     $          dbz1,dbpnl,dba3,
	endif

c write out the point indexes
c	if (outshort .eq. 2)
c     $	   write (8,'(i4,9i7)') ipres,izerobetween,indexpeak,
c     $       indexotherpeak,indexstart,indexfirstknee,indexlastknee,
c     $       indexend,indexnomzero


	if (lasym .or. lmv) then
c Having passed through SUBF7ADW_MAIN, the A array has now had padding added to 
c its front and back so restore the original wave form.
	   number = nunp
	   do i=1,power2
	      a(i) = unp(i)
	   enddo
c Use the predefined center points (various ways) for asymm. or symm. booms
c and calculate dB (PL,C,A,Lin) metrics for the preocessed waveforms
	   if (lasym) then
	      call flip(number,srate,outunit,outshort,ipres,name$,
     $				convert,psftopa)
	   else
c for now
	      call find_events(number,srate,a)
	      call flip_events(number,srate,outunit,outshort,ipres,
     $                    name$,convert,psftopa)
	   endif 	
	endif

c just to check, put all of a() back in signal for calling routine to plot
	do index = 1,power2
	  signal(index)  = a(index) / convert
	enddo

 991	format (i4,2x,a20,3f7.2,4f8.2,1x,2f9.4,4(f5.1,1x),f6.2)
 99	format (i4,2x,a20,3f7.2,4f8.2,1x,2f9.4,4f6.2,e12.5)
c3 99	format (i4,2x,a20,3f7.2,4f8.2,1x,3f7.2,4f6.2)

	return
	end
