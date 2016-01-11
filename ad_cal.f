	subroutine ad_cal(inunit,outshort,outunit,ifile,file$,
     $            ical_no,signumber,
     $		ref_volt,equiv_db,equiv_press,dcoffset,
     $          sample_rate,ierr)
c
c file$ passed in as 80 characters - BMS Aug 23 2004
c corrected call to read_it - wrong number of arguments - BMS July 8 2004
c read ref_volt and equiv_db from open_mat_file - BMS Nov 18 2002
c return sample rate (read from Matlab file, if nowhere else) - BMS Aug 26 2002
c Also to open the Matlab file for reborn booth version- BMS Aug 21 2002
c program to calibrate the system for adloud
c                                             BMS June 26 1990
c change calling parameters of read_ad_header      Feb 12 1992
c Add BOOMFILE option (already in PSF) - Dec 2 1993

c	INPUT
c             inunit = logical unit for input data
c            outshort = logical unit for shortform output data
c            outunit = logical unit for longform output data
c	       ifile = 1 for A/D (one file for cal & signals)
c	               2 for D/A (separate files)
c	               3 for WSMR data

c	OUTPUT
c	       file$ = file name (for A/D file, this may be usefull)
c	   signumber = number of signals in file (not including cals.)
c	     ical_no = aquisition number in A/D file for cal_tone (iskip)
c	    ref_volt = reference voltage for calibration level
c	    equiv_db = pressure level equivalent (in dB) to ref_volt
c	 equiv_press = pressure level equivalent (in Pascals) to ref_volt
c           dcoffset = D.C. offset for cal. tone
c	        ierr = error flag (o is good)

c the READ routines read into the standard PLAYBOOM arrays
	include 'sigpair_big.inc'

	integer inunit,signumber,outunit,outshort
	logical lseq,ltype,lme

	character*80 file$,name$

c reference pressure in Pascals (which is what SUBF7X works in)
	real pzero
	data pzero /20.0e-6/

	real sample_rate

	ierr = 0

	if (ifile .eq. 7 .or. ifile .eq. 8) then
	   ref_volt = 1.002
	   equiv_db = 94.0
	else if (ifile .ne. 1) then
	   print *,' '
	   print *,' Enter the reference voltage and equivalent dB level'
	   print *,' or enter zeros to use a cal. tone file'
	   accept *,ref_volt,equiv_db
	   print *,' '
	   if (ref_volt .eq. 0.0) then
 100	      print *,' Enter file name holding the cal. tone'
	      read (*,'(a)') file$
	      slevel = 1.
	      cal_fac = 1.
	      iwhich = 0
	      call read_it(inunit,file$,slevel,cal_fac,iwhich,
     1		signal,isignal,number,srate,adjust,isize_sigpair,ierr)
	      if (ierr .ne. 0) then
	         print *,' ******* Error reading cal. signal *******'
	         print *,' Try another name'
	         go to 100
	      endif
	      call sub_ref(outunit,signal,number,srate,ref_volt,equiv_db,
     $				dcoffset,ierr)
	   endif
	else
c it was all in one file but I can't see how to handle that now.
 300	   print *,' Enter Matlab file name holding the data'
	   read (*,'(a)') file$
	   iclose = 0
	   call open_mat_file(sample_rate,ref_volt,equiv_db,file$,ierr)
	print *,sample_rate
	print *,ref_volt
	print *,equiv_db

c	   call read_ad_header(file$,inunit,outshort,outunit,
c     @				iclose,lseq,ltype,lme,signumber,ierr)
	   if (ierr .ne. 0) go to 300
	   print *,' '
c	   print *,' Enter the aquisition number for the cal. tone'
c	   print *,' or zero to enter the reference level'
c	   accept *,ical_no
c	   print *,' '
c	   if (ical_no .eq. 0) then
c	      print *,' Enter the reference voltage and equivalent dB level'
c	      accept *,ref_volt,equiv_db
	      ical_no=-1
c	   else
c
c we've only just opened the file so we skip from the start to the
c required aquisition (iskip = ical_no)
c	      call read_ad(inunit,outunit,0,ical_no,number,srate,name$,ierr)
c	      call sub_ref(outunit,signal,number,srate,ref_volt,equiv_db,
c     $				dcoffset,ierr)
c	   endif
	endif

c ref_volt is equivalent to equiv_db.
c What's that in Pascals?
c 0 dB = pzero pascals
	equiv_press = pzero * 10.**(equiv_db/20.)

	print *,' '
	print *, ref_volt,equiv_db,equiv_press

	return
	end
