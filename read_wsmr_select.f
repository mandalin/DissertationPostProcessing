	subroutine read_WSMR(isize,ifkind,iunit,file$,slevel,
     1		cal_fac,iwhich,array,isignal,numodd,srate,adjust,ierr)

c improve some of the printed lines - BMS Oct 10 2006
c if srate = 0 and no time data entered, program will probably crash.
c So warn the user

c Allow for selection of portion of a file; also allow for the selection of
c		the taper length - Nov 2 2005
c Handle if files read in is larger than storage array - Nov 4 2005
c fade up from zero if start pressure is negative as well as positive - August 1 2005
c number of pairs of data points need not be known - March 29 1994
c Sample rate need not be known but input must still be at equispaced-times
c CHECK THE SIGNAL LENGTH                                 - Jan 4 1994
c Improve code for reading in strange files  - Jan 3 1994
c Add code for reading BOOMFILE booms - Dec 2 1993
c Pass in isize - July 17 1993
c Add METRUM-type files - April 22 1993
c Extrapolate the first two points to get zero/start of boom - May 12 1992
c Different format in boom file - April 29 1992
c Zero IERR on entry - April 20, 1992
c Make zero padding at start optional - April 17 1992 (ho,hum)
c Delete zero padding at start - it doesn't always fit - March 30 1992
c
c Based on 
c        subroutine regen(array,wavesave,iwavesave,srate,numbnow,
c     $                          lcont,number)

c but we need to assign values to the usual parameter list:-
c      INPUT -  iunit = signal storage unit for reading from
c               file$ = file name
c        adjust, iwhich, slevel & cal_fac set to zero by calling routine
c
c      OUTPUT - array = storage array for odd format time history, assuming
c                       known (8K) sample rate
c               srate = sample rate for time history
c              numodd = number of points in time history 

c	parameter (isize=65536)

	real array(isize)
	integer isignal(1)
	character*(*) file$

	character*1 junk

c Hanning window taper length
	common /hantaper/ taper_length
c
	print*, 'array', array(1), size(array)
	ierr = 0

c	print *,'What type of file is this :-'
c	print *,'                      WSMR  -  1'
c	print *,'                    Metrum  -  2'
c	print *,'                  BOOMFILE  -  3'
c	print *,'                     other  -  4'
c	print *,'Enter the appropriate code'
c	accept *,ifsort
	if (ifkind .eq. 3 .or. ifkind .eq. 4) then
	   ifsort = 1
	else if (ifkind .eq. 5 .or. ifkind .eq. 6) then
	   ifsort = 2
	else if (ifkind .eq. 7 .or. ifkind .eq. 8) then
	   ifsort = 3
	else
	   ifsort = 4
	endif

	if (ifsort .eq. 1) then
	   file$ = 'dvl:'//file$
	else if (ifsort .eq. 3) then
	   file$ = 'bmf:'//file$
	endif
          
	close (unit=iunit)
C	open (unit=iunit,file=file$,status='old',form='formatted',readonly,err=8000)
	open (unit=iunit,file=file$,status='old',form='formatted')
	scale_fac = 1.0
	if (ifsort .eq. 3) then
c BOOMFILE files have 13 record of junk initially
	   do i=1,13
	     read (iunit,'(a)') junk
	   enddo
	   srate = 8000.0
	   numodd = 0
c already in psf
	else if (ifsort .eq. 1) then
c WSMR files have a record of junk initially
	   read (iunit,'(a)') junk
	   srate = 8000.0
	   numodd = -1
	else
	   print *,'Enter sample rate of input (samples PER SECOND)'
	   print *,' 0 if not known - I''ll calculate it from input
     $ if you have time data'
	   read *,srate
	   if (ifsort .eq. 2) then
	      read (iunit,*) scale_fac
	      numodd = 0
	   else
	      print *,' Enter number of samples (0 if not known, -1 if
     $ first (useful) datum in file)'
	      read *,numodd
	      print *,' Enter scale factor to convert input to psf'
	      read *,scale_fac
	      print *,'How many lines of junk initially?'
	      read *,ijunk
	      do i=1,ijunk
	        read (iunit,'(a)') junk
	      enddo
	   endif
	endif

	if (numodd .gt. isize) then
         print *,' Too many points in input'
	   print *,' Call the programmer'
	   go to 600
	else if (numodd .eq. 0) then
c number of samples not known
	   ios = 0
	   index = 1
	   if (ifsort .eq. 2) then
	      do while (ios .eq. 0)
c 1 hex number per record for Metrum files (as of April 22 1993)
	         read (iunit,'(z5)',iostat=ios) ival
	         array(index) = scale_fac*(2048.-ival)
	         index = index+1
	         if (index .gt. isize) then 
			    go to 200
	         endif
	      enddo
c I seem to get 2 or 3 numbers in xs
 200	      index = index-3
	   else if (ifsort .eq. 3) then
	      do while (ios .eq. 0)
c 2 real numbers (time and pressure pair) per record for BOOMFILE files
	         read (iunit,*,iostat=ios) rval1,rval2
	         array(index) = rval2
	         index = index+1
	         if (index .gt. isize) then 
			    go to 210
	         endif
	      enddo
 210	      continue
c I should get 1 number in xs - shouldn't I?
	   else
	      print *,'Do you have pressure values only (1) or
     $ time and pressure pairs (2)?'
	      read *,ipairs
	      if (ipairs .eq. 1 .and. srate .eq. 0) then
		     print *,'I cannot find a sample rate unless ',
     c 'you give me time data.'
	         print *,'I shall probably crash - you have been warned.'
c               stop
	      endif
	      if (ipairs .eq. 2) then
	         print *,'Are times in secs(1) or msecs(2)?'
	         read *,msec
	         if (msec .eq. 1) then
	            rsec = 1.
	         else
	            rsec = 1000.
	         endif
	         read (iunit,*,err=7000,end=6000) a,b
	         array(1) = b
	         time_start = a/rsec
	         index = 2
	         do while (ios .eq. 0)
	            read (iunit,*,iostat=ios) a,b
	            array(index) = b
	            index = index+1
	            if (index .gt. isize) then 
			       go to 220
	            endif
	         enddo
 220	         time_end = a/rsec
	         srate_calc = float(index-2)/(time_end-time_start)
	         if (srate .eq. 0.0) then
	            srate = srate_calc
	            print *,' Calculated sample rate was ',srate_calc
	         else if (srate .ne. srate_calc) then
	            print *,' Input sample rate was ',srate
	            print *,' Calculated sample rate was ',srate_calc
	         endif 
	      else
c pressures only
      print *,' How many numbers per record/repeating sequence?'
	         read *,irecnum
	         if (irecnum .le. 0) print *,' Oh, come on!  I give up'
	         index = 1
	         do while (ios .eq. 0)
 		 read (iunit,*,iostat=ios)
     $                      (array(i),i=index,index+irecnum-1)
	            index = index+irecnum
	            if (index .gt. isize) then 
			       go to 230
	            endif
	         enddo
230	         continue
	      endif
	   endif
	   print *,' I got up to ',index-1,' before the data
     $ ran out'
	   numodd = index-1
      else if (numodd .eq. -1) then
c number of samples is first datum in file
	   read (iunit,*,err=7000,end=6000) numodd
	   if (numodd .gt. isize) then
              print *,' Too many points in input'
	        numodd = isize
	   endif
	   print *,'Do you have pressure values only (1) or
     $ time and pressure pairs (2)?'
	   read *,ipairs
	   if (ipairs .eq. 1 .and. srate .eq. 0) then
		     print *,'I cannot find a sample rate unless ',
     c 'you give me time data.'
	         print *,'I shall probably crash - you have been warned.'
c               stop
	   endif
	   if (ipairs .eq. 2) then
	      print *,'Are times in secs(1) or msecs(2)?'
	      read *,msec
	      if (msec .eq. 1) then
	         rsec = 1.
	      else
	         rsec = 1000.
	      endif
	      read (iunit,*,err=7000,end=6000) a,b
	      array(1) = b
	      time_start = a/rsec
	      do i=2,numodd
	         read (iunit,*,err=7000,end=6000) a,b
	         array(i) = b
	      enddo
	      time_end = a/rsec
	      srate_calc = float(numodd-1)/(time_end-time_start)
	      if (srate .eq. 0.0) then
	         srate = srate_calc
	         print *,' Calculated sample rate was ',srate_calc
	      else if (srate .ne. srate_calc) then
	         print *,'******************************************'
	         print *,' Input sample rate was ',srate
	         print *,' Calculated sample rate was ',srate_calc
	         print *,' IS THIS RIGHT?'
	         print *,'******************************************'
	      endif 
	   else
	      read (iunit,*,err=7000,end=6000) (array(i),i=1,numodd)
           endif
      else 
c number of samples is known and resides in variable numodd
	   print *,'Do you have pressure values only (1) or
     $ time and pressure pairs (2)?'
	   read *,ipairs
	   if (ipairs .eq. 1 .and. srate .eq. 0) then
		     print *,'I cannot find a sample rate unless ',
     c 'you give me time data.'
	         print *,'I shall probably crash - you have been warned.'
c               stop
	   endif
	   if (ipairs .eq. 2) then
	      print *,'Are times in secs(1) or msecs(2)?'
	      read *,msec
	      if (msec .eq. 1) then
	         rsec = 1.
	      else
	         rsec = 1000.
	      endif
	      read (iunit,*,err=7000,end=6000) a,b
	      array(1) = b
	      time_start = a/rsec
	      do i=2,numodd
	         read (iunit,*,err=7000,end=6000) a,b
	         array(i) = b
	      enddo
	      time_end = a/rsec
	      srate_calc = float(numodd-1)/(time_end-time_start)
	      if (srate .eq. 0.0) then
	         srate = srate_calc
	         print *,' Calculated sample rate was ',srate_calc
	      else if (srate .ne. srate_calc) then
	         print *,'******************************************'
	         print *,' Input sample rate was ',srate
	         print *,' Calculated sample rate was ',srate_calc
	         print *,' IS THIS RIGHT?'
	         print *,'******************************************'
	      endif 
	   else
	      read (iunit,*,err=7000,end=6000) (array(i),i=1,numodd)
           endif
      endif

	if (ifsort .ne. 2 .and. scale_fac .ne. 1.) then
	   do i=1,numodd
	      array(i) = array(i)*scale_fac
	   enddo
	endif

cNov 2 2005 - select portion of file
	istart=1
	iend=numodd
	taper_length = srate
c	if ifsort of ifkind or something
	   istart=numodd
	   iend=0
	   do while (istart .le. 0 .or. istart .ge. numodd)
	      print *,'Enter index of begining point'
	      read *,istart
	      if (istart .le. 0 .or. istart .ge. numodd) then
	         print *,istart, numodd
		     print *,'That is not allowed'
		  endif
	   enddo
	   do while (iend .le. istart .or. iend .gt. numodd)
	      print *,'Enter index of ending point (use 0 for all)'
	      read *,iend
		  if (iend .eq. 0) iend=numodd
	      if (iend .le. istart .or. istart .gt. numodd) then
		     print *,'That is not allowed'
		  endif
	   enddo
	   do i=istart,iend
	      array(i-istart+1) = array(i)
	   enddo

	   numodd=iend-istart+1
c Select the length of taper required (in case there's not enough start time for 1 sec or 1/11 of the signal)
	   taper_length = 0
	   do while (taper_length .le. 0 .or. taper_length .gt. numodd)
	      print *,'Enter number of points for Hanning window taper'
	      read *,taper_length
	      if (taper_length .le. 0 .or. taper_length .gt. numodd) then
		     print *,'That is not allowed'
		  endif
	   enddo
	   
c 	endif

	if ((ifsort .eq. 1) .or. (ifsort .eq. 3)) then
cAug 1 2005	   if (array(1) .gt. 0.0) then
	   if (array(1) .ne. 0.0) then
	      print *,' First point of array is ',array(1)
              if (array(2) .le. array(1)) then
	         print *,' The begining is not a clean pressure increase.'
	      else
	         print *,' I shall extrapolate from the first two points
     $ to get to zero'
                 dely = array(2) - array(1)
	         nshift = int(abs(array(1)/dely)) + 1
	         print *,dely,nshift
	         if (nshift .le. 0) then
	            print *,'Barmy, I call it.'
	         else 
	            if (isize .lt. numodd+nshift) then
	               print *,nshift,' is too many points to extrapolate'
	               nshift = isize-numodd
	               print *,' I shall use ',nshift
	            endif
                  do i=numodd,1,-1
	               array(i+nshift) = array(i)
	            enddo
	            do i=nshift,2,-1
	               array(i) = array(i+1) - dely
	            enddo
	            array(1) = 0.0
	            numodd = numodd+nshift
	        endif
	      endif

	      print *,' Enter number of preceeding samples of zero value,
     $ if any'
	      read *,nzeros
c	      print *,' I shall enter 100 zeros at the start! Unwise?'
c	      nzeros = 100
c	      print *,' I shall enter NOT 100 zeros at the start.'
c	      nzeros = 0
	      if (nzeros .gt. 0) then
	         numodd = numodd+nzeros
	         do i=numodd,1,-1
	            array(i+nzeros) = array(i)
	         enddo
	         do i=1,nzeros
c	            array(i) = array(nzeros+1)
	            array(i) = 0.0
	         enddo
	      endif
	   endif
	endif
c time in seconds for now
	timeodd = float(numodd)/srate
	print *,' Length of signal in seconds is ',timeodd
	close (unit=iunit)
	return


 6000	print *,' Premature end of file'
	print *,' Try another'
	go to 600

 7000	print *,' Error in file format'
	print *,' Try another'
	go to 600

 8000	print *,' Couldn''t open that file name'
	print *,' Try another'
	go to 600

  600	ierr = 1
c what about overload?
	return

	end
