	program adloud

c change some of the printed lines to be more comprehensible - BMS Oct 10 2006

c Add option for selecting a piece of a (Matlab) file (and not
c looking for an event within it, but using the whole thing, suitable windowed)
c						BMS July 25 2006
c Add the Hanning window taper length here to be available to all - BMS July 24 2006
c	Default to a length of 0, in which case no taper


c Lets have a release version that removes all the impossible options - BMS Oct 3 2005
c put array size parameters in include file - BMS Aug 4 2005

c i/3 o.b. dump replaces and all formats handled.  Otherwise I have to have
c	a version for them and a version for me - BMS Aug 20 2004
c Version that only does undefined format files - BMS July 20 2004
c Remove dump 1/3 octave band values (for release to SSBD) - BMS July 9 2004
c Add an adjusted file option for computer files - BMS Aug 12 2003
c initialize the Matlab A/D option - BMS Aug 21 2002
c pass max available storage array size to read_it - BMS July 25 2002
c  Mets version - remove plotting code; store results locally; no default extensions;
c           no old formats available  - BMS May 17 2002
c read_ad replaced with read_mat1 as thr new DAQ code writes out Matlab files - BMS May 21 2002
c
c add the two time-constant model (tc1,tc2 and sub_expav2) - Oct 24 1994
c add stop on error from read_ad - July 18 1994
c add option for lots of D/A files - Feb 4 1994
c Add BOOMFILE options - Dec 2 1993
c Add MV - ie loudness calculation for each of multiple events - Sep 7 1993
c (Also scream about Write_Boom error!)
c Increase max array size to 131072 &
c Pass max. array size into read_wsmr - July 17 1993
c replace reading data from file of any origin - June 4 1993
c increase max. signal length to 65536 - April 29 1993
c       However, SUB_LOUD only returns 32768 of it because of conflicts with
c                                            SUB_PLAY_STORE_INE in PLAY_BOOM
c Add METRUM option to the WSMR reading code - April 22 1993
c Put plot after subf7ad to get the full length & add HRK after the plots 
c                - BMS Feb 19 (& April 20) 1992 (for plot_only version, really)
c add optional plotting capability 	BMS July 29 1990
c add peak psf levels to short-form output     BMS August 7 1990
c Unit for short_form output changed from 2 to outshort and put in the
c calling arguments - BMS Jan 25 1991
c Add the asymmetric analysis option - BMS Feb 20 1991
c Combine old and new file formats into one program - BMS March 13 1991
c Add Zwicker's loudness - BMS May 29 1991
c Replace an option for the old PAD: files (IAGE in common) - BMS May 31 1991
c Add the reflection parameter values - Dec 11, 1991
c Add ASCII booms from WSMR option, using REGEN-type code from WRITE_BOOM - bms
c                                                              March 17 1992
c add PNL output - bms 20 oct 1992

c program to calculate loudness (dBA, dBC, PLdB) from time histories
c such as are read in form the A/D (by option 13 in PLAYBOOM) or such
c as are created by WRITE_BOOM (but there you have to be clever in 
c calibrating the "voltages", which are actually numbers)
c                                             BMS June 26 1990

c Add 1/3 octave band dump option (compatible with Dave's FIND?)
c Units used are:-
c	1 = long-form level output (.lvl) (if required) (outunit)
c	2 = short-form level output (.slv) (if required) (outshort)
c commented out 8 = points output for graf (.pts) (output if 2 is)
c	3 = signal storage file input (inunit)
c	4 = 1/3 octave band dump file output (.tho) (if required) (dumpunit)
c	7 = MYLIB hard copy file unit (if required)

C CONTAINS DATA TABLES FOR PNL CALCULATIONS
	INCLUDE 'pnl_pl.cmn'

c the READ routines read into the ncreased size (ex-standard) PLAYBOOM arrays
	include 'sigpair_big.inc'
c
c two time-constant model parameters
	common /twotc/ tc1, tc2
c
c reflection parameters
        logical lreflx
        real deltime,ratpress
        integer numbdel
        common /reflex/ lreflx,deltime,delphase,ratpress,numbdel

c But the loudness routine (from NWAVE - SIG7FX has room for 2**16 data points
c Now in include file         parameter (power2 = 131072)
        real a(power2)
        common /nwavearray/ a

	real srate

c do we plot?
	logical lplot
	common /doplot/ lplot
	logical lseq,ltype

c In case we want to save the 1/3 octave band levels
	logical ldump, lfirst
	integer dumpunit, nweight
	common /dump/ ldump, dumpunit, nweight

	integer inunit,outunit,outshort

	character*1 cans
	character*80 file$,name$,old_sfile$,old_lfile$,old_ofile$


        logical linv_used
        character*80 inv_name, orig_name
        character*8 ntime
        character*9 ndate
        common /footer/ linv_used, inv_name, orig_name, srate_inv_used, 
     & idelay_used, ntime, ndate
 
c store the results somewhere
	character*32 result$
	data result$ /'pout:'/

c If these are asymmetric signals, we want to play games to separate out the 
c front and back ends.  Let's assume the request for an asymmetric analysis is
c in a new common block, and it's true if we want it

	logical lasym,lfull,lmv
	common /yessym/ lasym,lfull,lmv

c old (pre-nov 1990) data (0), middle-aged (to Dec 1991) (2) or new (1)
	integer iage
	common /howold/ iage

c variables for Matlab
        integer ep, status
c Matlab wants everything in double precision so make a holding variable.
c parameter in include file
        double precision dptemp(max_size)
        double precision dp_size
        common /Matcom/ ep, status, dptemp, dp_size

c Hanning window taper length
	common /hantaper/ taper_length
C	real array(power2)

c initialize to taper length of 1
	taper_length = 0


c	print *,'power2',power2
c	print *,isize_sigpair

c initialze to Matlab not used
        ep = 0

c initialize the PNL data tables
	call pnldata
c  set up weighting data arrays 
	call Weightdata

	inunit = 3

 100	print *,' '
	print *,' '
	print *,' Are you analyzing '
	print *,'        (0) nothing at all'
C	print *,'        (1) an (Matlab) A/D file (that''s acoustical)?'
C	print *,'        (2) a D/A file (that''s the computer file)?'
C	print *,'        (3) a WSMR file?'
C	print *,'        (4) lots of WSMR files?'
C	print *,'        (5) a METRUM file?'
C	print *,'        (6) lots of METRUM files?'
C	print *,'        (7) a BOOMFILE file?'
C	print *,'        (8) lots of BOOMFILE files?'
	print *,'        (9) an ASCII data file?'
C	print *,'       (10) lots of D/A files 
C     $(that''s the computer file)?'
C	print *,'       (11) lots of D/A files with adjusted levels 
C     $(that''s the computer file)?'
	print *,'       (12) lots of files with similar format?'
	read *,ifile

	print *,' '
	if (ifile .eq. 0) then
		print *,'closing matlab and stopping'
           if (ep .ne. 0) call close_matlab
	   stop
c this code would only allow options 9 and 12
c	else if (ifile .ne. 9 .and. ifile .ne. 12)
c		print *,'Option not available'
c		go to 100
	else if (ifile .gt. 12 .or. ifile .lt. 0) then
		print *,'Option not available'
		go to 100
	endif

c default to new
	iage = 1

	print *,' Do you want to do symmetric analyses (0 or N)'
	print *,'                   asymmetric analyses (1 or Y)'
C	print *,'               or  multi-event analyses (2) ? (0=default)'
	read (*,'(a)') cans
	if (cans .eq. 'y' .or. cans .eq. 'Y' .or. cans .eq. '1') then
	   lasym = .true.
	   lmv = .false.
	   print *,
     $'*** Beware the WRITE_BOOM error may be still there ****'
C	else if (cans .eq. '2') then
C	   lasym = .false.
C	   lmv = .true.
C	   print *,
C     $'*** I am assuming the WRITE_BOOM error is still there ****'
C	   print *,
C     $'*** and delay used is really 512 always and forever ******'
C	   print *,
C     $'** though that was in the VAX days and may no longer be true **'
	else
	   lasym = .false.
	   lmv = .false.
	endif

c first define where the output is to go (use old file if we're already using it
	print *,' Enter short-form output file name (N for none)'
	read (*,'(a)') file$
	if (file$ .eq. 'n' .or. file$ .eq. 'N') then
	   outshort = 6
	   old_sfile$ = ' '
	else
	   outshort = 2
	   if (file$ .ne. old_sfile$) then
	      old_sfile$ = file$
	      close (outshort)
c	      open (unit=outshort,file=result$//file$,
	      open (unit=outshort,file=file$,
     $ status='new',form='formatted',err=8000)
c     $ status='new',form='formatted',defaultfile='.slv',err=8000)
c	      outpoint = 8
c	      close (outpoint)
c	      open (unit=outpoint,file=result$//file$,
c     $ status='new',form='formatted',defaultfile='.pts',err=8000)
	      if (lasym) write (2,'(a)') ' ASYMMETRIC MEASURES for
     $ full and half waves'
	      if (lmv) write (2,'(a)') ' MEASURES for multiple events'
	      write (outshort,99) '  #     signal            dB(PL)  dB(C)
     $  dB(A)  dB(Lin)  dB(Z)f  dB(Z)d  dB(PNL)   max   min (psf) '
c	      write (8,*) '  #  zerobetween ipeak iotherpeak istart ifirstknee
c     $ ilastknee iend inomzero'

	   endif
	endif
 99	format (a)

	print *,' '
	print *,' Enter long-form output file name (N for none)'
	read (*,'(a)') file$
	if (file$ .eq. 'n' .or. file$ .eq. 'N') then
	   outunit = 6
	   old_lfile$ = file$
	else if (file$ .ne. old_lfile$) then
	   old_lfile$ = file$
	   outunit = 1
	   close (outunit)
c	   open (unit=outunit,file=result$//file$,
	   open (unit=outunit,file=file$,
     $ status='new',form='formatted',err=8000)
c     $ status='new',form='formatted',defaultfile='.lvl',err=8000)
	endif

	   lplot = .false.

	lfirst = .true.
c now do we want 1/3 octave band levels?
 150	continue
	nweight = 0
c	print *,' 1/3 octave band level output has been removed'
C	print *,' Do you want :-'
C	print *,'        linear 1/3 octave band levels (1)'
C	print *,'        A-weighted 1/3 octave band levels (2)'
C	print *,'        C-weighted 1/3 octave band levels (4)'
C	print *,' (sum those numbers for multiple weightings'
C	print *,'  so 7 is all three, 3 is linear and A, etc)'
C	accept *,nweight

c I need a bitmask!
	if (nweight .eq. 7) then
	   ndump = 3
	else if (nweight .eq. 6 .or. nweight .eq. 5 
     $					.or. nweight .eq. 3) then
	   ndump = 2
	else if (nweight .eq. 1 .or. nweight .eq. 2 
     $					.or. nweight .eq. 4) then
	   ndump = 1
	else if (nweight .eq. 0) then
	   ndump = 0
	   ldump = .false.
	else
	   print *,' Illegal option; try another'
	   go to 150
	endif
c	print *,ndump,nweight
	if (ndump .gt. 0) then	
	   print *,' Enter 1/3 octave band output file name'
	   read (*,'(a)') file$
	   ldump = .true.
	   dumpunit = 4
	   if (file$ .ne. old_ofile$) then
	      old_ofile$ = file$
	      close (4)
c	      open (unit=4,file=result$//file$,
	      open (unit=4,file=file$,
     $ status='new',form='formatted')
c     $ status='new',form='formatted',defaultfile='.tho')
	   endif
	endif

	print *,' '
	print *,'Ignoring two time constant possibilities for now'
	print *,' '
	tc1 = 1
	tc2 = 1
c	print *,'Enter the time constants for this run (in secs)'
c	accept *,tc1,tc2

c now calibrate - BOOMFILE files are already in psf
c also open Matlab file
	call ad_cal(inunit,outshort,outunit,ifile,file$,ical_no,nsignals,
     $		ref_volt,equiv_db,equiv_press,dcoffset,srate,ierr)
	if (ierr .ne. 0) then
	   print *,' Error reading cal. signal'
           if (ep .ne. 0) call close_matlab
	   stop
	endif

c loop to here on option 4 (oh,oh, another use of goto!)
	index = 0
 160	continue
	if (ifile .eq. 12 .or. (ifile .ge. 3 .and. ifile .le. 9)) then
C WSMR data - these should all be separate modules but it's too late now
c read a WSMR signal, zero pad it, and pass it to subf7
	   if (ldump) then
	      write (dumpunit,*) float(ndump)
	   endif
c
 190	   print *,' '
	   slevel = 0.
	   cal_fac = 0.
	   adjust = 1.0
	   iwhich = 0
	   print *,' Enter ASCII data file name (n to quit)'
	   read (*,'(a)') name$
	   if (name$ .eq. 'n' .or. name$ .eq. 'N') go to 100
	   print *,name$
c	   index = index + 1
	   in_size = power2
 191	   call read_WSMR(in_size,ifile,inunit,name$,slevel,
     1		cal_fac,iwhich,signal,isignal,number,srate,adjust,ierr)
C	   print*, array(1), size(array)
	   if (ierr .eq. 2) then
	      print *,' '
	      print *,' ****** Overload Error reading signal *******'
              print *,' Enter a temporary signal attenuation in dB'
              read *,slevel
	      go to 191
	   else if (ierr .ne. 0) then
	      print *,' '
	      print *,' ****** Error reading signal *******'
	      print *,' Try another name'
	      go to 190
	   endif
c If we adjusted the signal in order to be able to read it into the integer
c array, we now have to take it out again (Uh?)
	   if (adjust .ne. 1.0) then
c undo the previous adjustment in level, in preparation for a new one
	      do i=1,number
	         signal(i) = signal(i) / adjust
	      enddo
	      print *,' I have just taken out an attenuation of ',
     $                      slevel,' dB'
c	adjust = 10.**((-slevel + cal_fac)/20.)
	      print *,' Calibration factor was ',cal_fac
	      print *,' '
	   endif

c Now we are in PSF,  we need to go through the motions of any other signal -
c and we need to transfer it into array A for SUBF7
c (use an index of zero to tell SUB_LOUD not to correct for any frontend D.C.)
	   call sub_loud(outshort,outunit,signal,number,srate,index,
     $        name$,ref_volt,equiv_db,equiv_press,dcoffset,ierr)

c next read a signal, zero pad it, and pass it to subf7
	else if (ifile .eq. 2 .or. ifile .eq. 10 .or. ifile .eq. 11) then
c
	   if (ldump) then
c It would be nice to have the 1/3 octave band file in a format that Dave's 
c METRIC, etc could read.  The problem is that I have 1 spectrum per signal,
c not N 1/2 spectra, and I have multiple signals in one file - but judicious
c editing of the file I do produce should be possible.  I shall here try to
c estimate how many signals will be in this file so I can fake it by using
c the nominal number of half-seconds to represent the number of signals.  
	      write (dumpunit,*) float(ndump)
	   endif
c
 200	   print *,' '
	   slevel = 0.
	   cal_fac = 0.
	   print *,' Enter D/A file name (n to quit)'
	   read (*,'(a)') name$
	   if (name$ .eq. 'n' .or. name$ .eq. 'N') go to 100
	   if (ifile .eq. 11) then
	      print *,' Enter ATTENUATION to be applied'
	      read (*,*) slevel
	   endif
	   iwhich = 0
 201	   call read_it(inunit,name$,slevel,cal_fac,iwhich,
     1		signal,isignal,number,srate,adjust,isize_sigpair,ierr)
	   if (ierr .eq. 2) then
	      print *,' '
	      print *,' ****** Overload Error reading signal *******'
              print *,' Enter a temporary signal attenuation in dB'
              read *,slevel
	      go to 201
	   else if (ierr .ne. 0) then
	      print *,' '
	      print *,' ****** Error reading signal *******'
	      print *,' Try another name'
	      go to 200
	   endif
c If we adjusted the signal in order to be able to read it into the integer
c array, we now have to take it out again (Uh?)
c BUT if we adjusted the signal because of the new option 11 then don't take it out
	   if (adjust .ne. 1.0 .and. ifile .ne. 11) then
c undo the previous adjustment in level, in preparation for a new one
	      do i=1,number
	         signal(i) = signal(i) / adjust
	      enddo
	      print *,' I have just taken out an attenuation of ',
     $                      slevel,' dB'
c	adjust = 10.**((-slevel + cal_fac)/20.)
	      print *,' Calibration factor was ',cal_fac
	      print *,' '
	   endif

c Now we have some "volts" in SIGNAL, we need to convert it to pressure,
c and correct for the D.C. offset if different from the cal. version.
c Then we need to transfer it into array A for SUBF7
c (use an index of zero to tell SUB_LOUD not to correct for any frontend D.C.)
	   index = 0
	   call sub_loud(outshort,outunit,signal,number,srate,index,
     $        name$,ref_volt,equiv_db,equiv_press,dcoffset,ierr)

	else
c (ifile = 1)
	print *,'ifile = 1'

c A lot of this code is kept until I find out what I really want to do - BMS May 21 2002
	   if (ical_no .eq. -1) then
c I thought this worked once - doesn't now, though - July 7 1992
	     iold = 0
	   else
	     iold = 1 - ical_no
	   endif
 300	   print *,' '
c	   print *,' Which A/D signal shall I use
c     $ (counting the first cal. as 0)?'
c	   print *,' (Use a negative number to go through the
c     $ whole file)'
c	   print *,' (Use 0 to quit)'
c	   accept *,inow
c	   if (inow .eq. 0) then
c	      go to 7000
c	   else if (inow .lt. 0) then
c	      istart = 1
c	      iend = nsignals
c	      istep = 1
c	   else if (inow .gt. 0 .and. inow .le. nsignals+2) then
cc (so far I allow a final b/g noise and a post cal. recording)
c	      istep = inow - iold
c	      istart = inow
c	      iend = inow
c	   else
c	      print *,' **** Number out of range **** '
c	      go to 300
c	   endif
cc
C	   if (ldump .and. lfirst) then
c To repeat what I said above,
c It would be nice to have the 1/3 octave band file in a format that Dave's 
c METRIC, etc could read.  The problem is that I have 1 spectrum per signal,
c not N 1/2 spectra, and I have multiple signals in one file - but judicious
c editing of the file I do produce should be possible.  I shall here try to
c estimate how many signals will be in this file so I can fake it by using
c the nominal number of half-seconds to represent the number of signals.  
C	      write (dumpunit,*) float((iend-istart+1)*ndump)
c If we're looping through multiple signals, I don't want to stick this in every
c time - what a mess
C	      lfirst = .false.
C	   endif
c
c	   do index = istart,iend

	      name$ = file$
		  print *,'Calling read_mat1'
		print *, srate
	      call read_mat1(isize_sigpair,name$,signal,
c   numodd,
     $    outshort,outunit,ipres,
     $    ref_volt,equiv_db,equiv_press,dcoffset,
     $    srate,ierr)

		  print *,'back from read_mat1'

	      if (ierr .eq. 99) then
c we had an error, but we should already have said so
	      endif
c	      if (ierr .eq. -1) then
c we've run out of file (or had a read error)
c	         print *,' Premature end of data ',index
c                 if (ep .ne. 0) call close_matlab
c	         stop
c	      endif
c
c		all this following is in read_mat1
c
c Now we have some "volts" in SIGNAL, we need to convert it to pressure,
c and correct for the D.C. offset if different from the cal. version.
c Then we need to transfer it into array A for SUBF7
c	      call sub_loud(outshort,outunit,signal,number,srate,
c     $          index,name$,ref_volt,equiv_db,equiv_press,dcoffset,ierr)

c	   enddo
c	   iold = iend
c	   go to 300
	endif

	if ((ifile .eq. 4) .or. (ifile .eq. 6) .or. (ifile .eq. 8)
     $         .or. (ifile .eq. 10) .or. (ifile .eq. 11) 
     $         .or. (ifile .eq. 12))  go to 160

 7000	go to 100

 8000	print *,' Couldn''t open output file'
        if (ep .ne. 0) call close_matlab
	stop

	end
