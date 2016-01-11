       subroutine subf7adw_main(outunit,n,srate,dbpl,dbc,dba,dblin,
     & dbz0,dbz1,dbpnl,dba3)
c3     $                                     dbz0,dbz1,dbpnl)
c
c put array size parameters in include file - BMS Aug 4 2005

c code for output of the array that is passed into FFT routine
c           added; commented out for now - BMS Aug 1 2005
c change the fading from end to beginning - Aug 1 2005
c corrected call to lin (only 2 arguments) - BMS July 8 2004
c add comment printout to 1/3 octave band dump - BMS Dec 19 2003
c add threshold-weighted output - BMS Feb 10 2003
c
c Add LFULL for ADL control (as opposed to other loudness program) and 
c call to 1/3 octave band integrator changed for new integration - July 11 1994
c increase variable exponent from 16 to 17 (in line with other storage)
c (though we "never" get up that large in signal size    - BMS July 5 1994
c Increase storage size from 32768 to 65536. That doubles time taken for FFT - 
c so it's excessive to do it in all cases; hence do it only if N>32768 (or so)
c                                                      - April 30 1993
c (up it to 131072 - July 19 1993)
c3 Add 1/3 ob dBA output just for now - BMS Dec 2 1992
c Add PNL output - bms 20 oct 1992
c mini changes (typo * declarations) - April 20-1992
c
c Add Zwicker's loudness method (which uses data from 25Hz 1/3 o.b. to 10kHz)
c                                         - BMS May 28 1991
c
c Force number of points in FFT to POWER2 - BMS March 12 1991

C       BUILT FROM SUBF7C PROGRAM
c 1/3 octave band integration broken out into SUBF7AD_3OB; required 
c options are written out using LDUMP
c					BMS July 26 1990
c A/D loudness version pads out to powers of two with the final value in the
c array (not zero) - called by sub_loud
c					BMS June 28 1990
c Try a primitive windowing to match the front end of the signal with the back
c   (from TRANSWIN.PRO on GRAF)         BMS Sept 15 1990
c Put number of padded points in common - BMS 13 March 1991
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Title:             Module for sonic boom loudess calculation program.
C  Name:              FFT7C
C  Source Language:   VAX FORTRAN 77
C      Non-CDC FORTRAN features -
C               do while              
C  Purpose:
C           To calculate the loudness of sonic booms, using Stevens 
C      Mk VII method, calculated from 1/3 octave band levels which in turn are 
C      calculated from a spectrum that is derived analytically using the single 
C      precision IMSL FFT routine, FFTSC. The same spectrum is C-weighted (using
C      a formula from ANSI S12.4, "Specifications for Sound Level Meters"),
C      and integrated to get an equivalent C-weighted level. Similarly it is
C      A-weighted (using a formula from R.M.Aarts - J.S.V.(1987) 115(2), 372)
C      and integrated to get an equivalent A-weighted level.
C  Files:
C      Input data file of pressure levels; file is in free format; the first 
C      record is the  number of data points. The rest is the data array of 
C      pressure levels in units of Pascals or psi (user option); the size of 
C      the array can be up to POWER2 data points. The sample rate is also 
C      user-input (Pascals or PSI), as is a scaling factor that can be used 
C      to shift (multiply) the pressure values linearly.
C  Subroutines called:
C             FFTSC (IMSL single precision FF routine)
C             PL (Stevens Mk VII loudness calculation procedure)
C             CL (C-weighting procedure)
C             AL (A-weighting procedure)
C  Restrictions:
C      Input data can be up to POWER2 data points only (at present that is 
C      8192); the number of points need not be a power of 2.
c	(If not, it gets padded out)
C      Input data must be in Pascals or Pounds per Square Inch (except for
C      pressure units that can be scaled into these units using the user-
C      supplied scale factor).
C
C  Author:    B.M.Sullivan (PRC) - Structural Acoustics Branch, ext 3561
C  Date:      Sept 8 1988
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


c The FFT routine produces 2 series, ST and CT, (sine and cosine) which are
c then combined into XT (magnitude). This represents the pressure amplitude
c data array. ( SQRT(ST**2 + CT**2) * N = amplitude, because of the form of the 
C output from FFTSC, where N is the number of samples.)

C The pressure data is integrated into 1/3 octave bands and then converted
c into dB; first, the energy within the band is calculated & 
c divided by 70msec (.07 sec) to get the units right and to account for the 
c integration time of the ear. Then subtract 3dB because this loudness is due 
c to the energy of the whole N-wave, which (usually) is heard as 2 events 
c (as the events are separated by a time greater than the integration time of 
c the ear) so we need to split the energy into two.  Hence the 3dB reduction 
c (before the Stevens Mk VII loudness calculation).

C For the C-weighted "loudness" calculation, the energy is divided by 1 sec.

c data may be printed on the screen or sent to a file
        include 'sigpair_size.inc'
       integer outunit
       
c In case we want to save the 1/3 octave band levels
       logical ldump
       integer dumpunit, nweight
       common /dump/ ldump,dumpunit, nweight

       real pref(2)
       data pref /2.0e-5,.417973e-6/

c We have room for 2**17 data points
C       parameter (exponent = 17)
C       parameter (power2 = 131072)
C       parameter (halfp2a1 = 65537)
c parameters now in include file


       real A(power2)
       common /nwavearray/ A

       real CT(power2),ST(power2)
C The results of the FFT will be put into an array of M amplitude factors, XT,
c separated in frequency by FDELTA (starting with frequency=0 Hz)
       real fdelta, XT(power2)
       integer m
       common /spec/ XT, m, fdelta
c For powers of 2, IWK should be of length J, where N=2**J
c Otherwise, IWK should be of length 6*(N/2)+150 = 3*N + 150
c Use N=POWER2 - and use powers of 2, so J=exponent
       integer iwk(exponent)
c For powers of 2, WK is not used (except in the calling sequence)
c Otherwise, WK should also be of length 6*(N/2)+150
       real wk(exponent)
       equivalence (iwk(1),wk(1))
c CWK should be of length N/2 + 1
       complex cwk(halfp2a1)

c space to stored weighted spectrum
       real wtxt(power2)

c L is the aray of 1/3 octave band levels (1 to 40 are used for now)
c SPL will be the loudness value after going through PL.FOR
       real l(43),spl, splthr
       common /crux/ spl,l, splthr

       integer start_samp,end_samp

c padding parameters
       common /padding/ ifrontpad,ibackpad

c  LFULL tells whether it's a fullwave or a half wave analysis
	logical lasym,lfull
	common /yessym/ lasym,lfull
	
	print *,'        Just after entering subf7adw_main in sub_loud'

c Initialize the index to the kind of pressure data we're expecting
C (Data all converted to Pa in root)
       ipref = 1
       pref2 = pref(ipref)*pref(ipref)

c We're going to find the square of the magnitude from the real and imaginary  
C parts returned from the FFT routine. This we divide by { (twice the square 
C of the number of samples) divided by the sample time }
c and take 10log( this number/(reference pressure)**2 ), where the reference 
c pressure value depends on the units we measured the pressure in, i.e.:-
c             TEMP1 = (ST(I)*ST(I) + CT(I)*CT(I))
c             TEMP2 = TEMP1 / (2 * n**2 * fdelta)
C Remembering sample time = 1/fdelta = number of samples / sample rate

c
 
       if (N .gt. POWER2) then
          N=POWER2
          write (outunit,*) ' There are more data points in the file than I
     1 can handle.'
          write (outunit,*) ' I shall truncate to ',N,' points.'
       endif

c
c "zero" pad front and back of signal (1/2 & 1/2) - how much each side?
c fix number of points in PADDED signal to power2/2 if N<(power2/2)-3200 
c (ie leave enough space (what's enough? 10%?) for some padding) else 
c power2 (max. possible). Hence the same for all signals mostly.
       if (N .lt. (power2/4) - 6400) then
          ibignum = power2/4
       else if (N .lt. (power2/2) - 3200) then
          ibignum = power2/2
       else
          ibignum = power2
       endif
       ipadding = ibignum - N
       ifrontpad = ipadding/2
       ibackpad = ipadding - ifrontpad

c When we've done that, we need to window it.
c let's try a linear ramp from the back of the signal to the (repeated) front
c so we have IPADDING samples during which we want to go from A(N)
c to A(1) -- so the step size per sample is:-
c	 temp = A(1)-A(N)
       step_delta = ( A(1) - A(N) ) / (ipadding+1)

c First move the signal in the array
c we're moving the signal later in the array so we better start at the back
       do i=ibignum,ifrontpad+N+1,-1
          A(i) = A(N)
       enddo
       do i=N,1,-1
          A(i+ifrontpad) = A(i)
       enddo
       do i=ifrontpad,1,-1
          A(i) = A(1)
       enddo

       start_samp = ifrontpad+N+1
       do i=start_samp,ibignum
c          A(i) = A(i-1)+step_delta
          A(i) = A(start_samp-1)+(step_delta*(i-start_samp+1))
       enddo
       end_samp = ifrontpad
       do i=end_samp,1,-1 
c          A(i) = A(i+1)-step_delta
          A(i) = A(end_samp+1)-(step_delta*(end_samp-i+1))
       enddo

c   Redefine number of samples
       N = ibignum

c what have we ended up with? Aug 1 2005
	open (unit=7,file='subf7_shape.txt',
     $ status='unknown',form='formatted',err=8000)
	write (7,*) (A(ljk),ljk=1,N)

	close(unit=7)

 80    continue
       
 90    M = N/2 + 1
       RN = FLOAT(N)
       RM = FLOAT(M)

       call FFTSC(A,N,ST,CT,IWK,WK,CWK)

c Relationship between FFT coefficient array and frequency:-
c       frequency of first coefficient = 0.0
c       frequency of last coefficient = (sample_rate)/2
c       number of coefficients = M
c therefore, frequency interval between coefficients = ((sample rate)/2.)/(M-1)
c                                                    = ((sample rate)/2.*(M-1)
c However, given the number of samples, N,
c       M = N/2 + 1
c so    2*(M-1) = 2*(N/2) = N
c Therefore, frequency interval
       FDELTA = SRATE/RN

       do 400 I=1,M
          if (ST(I) .eq. 0.0 .and. CT(I) .eq. 0.0) then
             XT(I) = 0.0
          else
             TEMP =  (ST(I)*ST(I) + CT(I)*CT(I))
             XT(I) = (TEMP)/(2. * float(N)**2 * FDELTA)
          endif
 400   continue

c Now do the 1/3 octave band integration; result is in L array in common
C	print *,fdelta,m,pref2,lfull
c	print *,ldump
	call onethirdoctaves(fdelta,xt,m,l,pref2,lfull)

c If requested, dump the linear 1/3 octave band levels
c REMEMBER - these are normalized with 70 msec and for Full wave calculations
c (LFULL = true) have the -3dB averaging
	if (ldump) then
	   if (mod(nweight,2) .eq. 1) then
c		print *,'2'
		print *,'dumping 1/3 octave levels for bands'
C 1              1-43 (1.25Hz - 2020kHz)'
	      call dumplevels
	   endif
c/ print *,'3'
	endif

c Use bands 20 Hz to 41 to calculate a level using a weighting based on the 
c   human threshold of hearing 
       call threshold(splthr)
       write (outunit,*) 'Threshold weighted level = ',splthr,' dB'


C Use all the available band levels (1.25Hz to 10kHz) to calculate loudness
c using Stevens Mk VII
       call Pl

       write (outunit,*) 'Loudness of wave  = ',SPL,' dB(PL)'
       dbpl = spl

C Use some of the available band levels (25Hz to 10kHz) to calculate loudness
c using Zwicker for Frontal Field
       call LLzd(0)
       write (outunit,*) 'Loudness of frontal wave  = ',SPL,' dB(Z)'
       dbz0 = spl

C Use some of the available band levels (25Hz to 10kHz) to calculate loudness
c using Zwicker for Diffuse Field
       call LLzd(1)
       write (outunit,*) 'Loudness of diffuse wave  = ',SPL,' dB(Z)'
       dbz1 = spl

C Use some of the available band levels (50Hz to 10kHz) to calculate noisiness
c using PNL
       call Pnl
       write (outunit,*) 'Noisiness of wave  = ',SPL,' dB(PNL)'
       dbpnl = spl

C3 Use some of the available 1/3 octave band levels (50Hz to 10kHz) to 
c3 calculate loudness using dBA
       call Weight(2)
       write (outunit,*) 'Noisiness of wave  = ',SPL,' dB(A) (1/3 ob)'
       dba3 = spl

C Use the spectrum to calculate level
c using C-weighting
       print*, 'm', m
       call cl(spll,pref2,wtxt)

	if (ldump) then
c		print *,'4'
	   if (nweight .gt. 3) then
c		print *,'5'
c Now do the C-weighted 1/3 octave band integration; 
	      call onethirdoctaves(fdelta,wtxt,m,l,pref2,lfull)
	      call dumplevels
	   endif
	endif

       write (outunit,*) 'Level of wave  = ',SPLL,' dBC'
       dbc = spll

C Use the spectrum to calculate level
c using A-weighting
       call al(spll,pref2,wtxt)

	if (ldump) then
c		print *,'6'
	   if (nweight .eq. 2 .or. nweight .eq. 3 .or. 
     $			nweight .eq. 6 .or. nweight .eq. 7) then
c Now do the A-weighted 1/3 octave band integration; 
c		print *,'7',fdelta,m
	      call onethirdoctaves(fdelta,wtxt,m,l,pref2,lfull)
c		print *,'8'
	      call dumplevels
	   endif
	endif

       write (outunit,*) 'Level of wave  = ',SPLL,' dBA'
       dba = spll

C Use all the available band levels (1.25Hz to 10kHz) to calculate level
c using no weighting
       call lin(spll,pref2)

       write (outunit,*) 'Level of wave  = ',SPLL,' dBLin'
       dblin = spll

 900   continue

       return

 9990  format (a40)

 8000  print *,'subf7_shape.txt file open error'
	 goto 80

       end
