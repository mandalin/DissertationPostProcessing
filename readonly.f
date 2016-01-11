	subroutine readonly(inunit,iwhich,numb,srate,signal,isize,ierr)

c read in max. size of storage array - BMS July 25 2002
CM _ MVAX version - CM
c routine to read the meat of a D/A or A/D signal 	BMS June 25 1990
c A/D comment removed             BMS Oct 12 1990
c Put inverse stuff in common & add the boom inflection point 
c definitions - BMS Oct 25 1990
c Add the inverse filter sample rate and delay value - Nov 6 1990
c Accept only 32764 samples (that's all D_AOUTPUT, etc, will use) - Aug 28 1991
c Add the reflection parameter values - Dec 11, 1991
c Increase noinfl (again) - the possible size of the input file - July 16 2002

        logical lreflx
        real deltime,ratpress
        integer numbdel
        common /reflex/ lreflx,deltime,delphase,ratpress,numbdel

	logical lseq,ltype

	real signal(1)

	integer inunit

c pass in common the NINFL time and pressure pairs used to define this wave
        parameter (noinflp = 10000)
        real*4 press(noinflp), atime(noinflp)
        common /boomcom/ press,atime,ninfl,srate_boom

        logical linv_used
        character*80 inv_name, orig_name
        character*8 ntime
        character*9 ndate
        common /footer/ linv_used, inv_name, orig_name, srate_inv_used,
     $             idelay_used, ntime, ndate
  
c	print *,'In readonly ',isize
	ierr = 0
	read (inunit,err=8000,end=8010) numb,srate
	if (numb .gt. isize) then
	   numb = isize
	   print *,' Signal has been reduced to ',numb,' in READONLY'
	endif
	read (inunit,err=8000,end=8010) (signal(i),i=1,numb)
	read (inunit,err=9000,end=9010) ntime,ndate
	read (inunit,err=9000,end=9010) linv_used
	if (linv_used) then
	   read (inunit,err=9000,end=9010) inv_name
	   read (inunit,err=9000,end=9010) orig_name
c if Inverse srate or delay is not there, don't report an error, just ignore it
	   read (inunit,err=9020,end=9020) srate_inv_used
	   read (inunit,err=9020,end=9020) idelay_used
	else
	   inv_name = ' '
	   orig_name = ' '
	   srate_inv_used = 0.0
	   idelay_used = 0
	endif
c if NINFL is not there, don't report an error, just ignore it
	read (inunit,err=9030,end=9030) ninfl
	if (ninfl .gt. 0) then
c zero out the arrays
	   do j=1,200
	      press(j) = 0.0
	      atime(j) = 0.0
	   enddo
	   read (inunit,err=9000,end=9010) srate_boom
	   if (ninfl .le. 20) then
              read (inunit,err=9000,end=9010) (press(j),j=1,20),
     $                            (atime(j),j=1,20)
	   else
	      read (inunit,err=9000,end=9010) (press(j),j=1,ninfl),
     $                            (atime(j),j=1,ninfl)
	   endif
	endif
c if LREFLX is not there, don't report an error, just ignore it
	read (inunit,err=9040,end=9040) lreflx
	if (lreflx) then
	   read (inunit,err=9000,end=9010) 
     $                       deltime,delphase,ratpress,numbdel
	else
	   deltime = 0.0
	   delphase = 0.0
	   ratpress = 0.0
	   numbdel = 0
	endif

	return

 8000	print *,' Error reading signal file'
	ierr = -1
	return

 8010	print *,' Premature end of signal file'
	ierr = -1
	return

 9000	print *,' Error reading signal file footer'
	ierr = -1
	return

 9010	print *,' Premature end of signal file footer'
	ierr = -1
	return

 9020   srate_inv_used = 0
	return

 9030   ninfl = 0
	return

 9040   lreflx = .false.
	return

	end
