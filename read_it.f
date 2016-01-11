	subroutine read_it(iunit,file$,slevel,cal_fac,iwhich,
     1		store_area,iwhere_to_go,number,srate,
     2		adjust,isize_sigpair,ierr)

c remove error if slevel less than cal_fac - BMS Aug 12 2003
c read in max available storage size and send it to readonly - BMS July 25 2002
c Remove sampl_to_clock for Mets version - BMS May 17 2002
c
c (inverse used in common now - Oct 25 1990)
c  Protect against a att. level too much for the system to handle;
c also invoke an error handler in case that protection didn't work
c because we're working with an unnormalized signal.  (May 23 1990)
c Differentiate between clock rate and sample rate, and keep both - June 11 1990

C  THIS ROUTINE HAS NO PROTECTION AGAINST READING IN A FILE THAT IS TOO
C  LARGE FOR THE STORAGE AREAS, STORE_AREA AND IWHERE_TO_GO, SO IT CAN
C  OVERWRITE MEMORY.  THE ARRAY SIZES ARE CONTROLLED BY THE INCLUDE FILE
C  PINC:SIGPAIR.INC.  EDIT THIS FILE IF NECCESSARY, RECOMPILE AND RELINK 
C  TO INCREASE THE STORAGE AREA SIZE.

c Routine to read in a signal file -- binary time history data for
c the Sonic Boom study.  Also scales it as requested by the calling routine.
c If we ever add any header data to describe the file, we can read it here and
c pass it via a common block to the other routines.

c INPUT  - iunit = logical unit number for this read
c          file$ = file name of signal  file
c          slevel = requested ATTENUATION to be applied to this signal 
c          cal_fac = the calibration factor which is applied to all signals
c          iwhich = 0 for signal file, 1 for cal file
c      (use zero if in doubt - used to modify IFLAG passed in to OPEN_SIG)
c          isize_sigpair = maximum size of iwhere_to_go
c OUTPUT - iwhere_to_go = array in which to store the integerized 
c						scaled time history
c          store_area = array in which to store the real scaled time history
c          number = number of samples in the time history
c          srate = sample rate of the time history
c          adjust = the scaling applied, used to restore the signal in the
c                   calling routine if the same signal is to be played
c                   with a different attenuation
c          ierr = error code
c Also output the clock rate - which is what the PLAY_IT routine needs,
c rather than the sample rate;
c pass the clock rate in common to PLAY_IT
	common /clockrate/ iclock

	character*(*) file$

	real store_area(*)
	integer*2 iwhere_to_go(*)
c
	logical ladjust,logtest

c	print *,'now it''s isize_sigpair  ',isize_sigpair

	ierr = 0
c open old & leave open
	iflag = 1 + 2*iwhich
	call open_sig(file$,iunit,iflag,ierr)
	if (ierr .ne. 0) go to 8000

	iwho = 1
	call readonly(iunit,iwho,number,srate,store_area,isize_
     $ sigpair,ierr)
	close (iunit)

	if (ierr .eq. -1) then 
	   return
	endif
c
c	if (cal_fac .gt. slevel) then
c	   print *,' Signal attenuation cannot be less than system',
c     $ ' calibration factor, ',cal_fac
c	   ierr = 3
c	   return
c	endif

c ladjust returns a value of false if there is an error
	logtest =  ladjust(slevel,cal_fac,store_area,iwhere_to_go,
     2		number,adjust,ierr)

	if (.not. (logtest)) then
	   print *,' Adjusted signal too large; reduce its
     $ level by using a greater attenuation.'
	   print *,' The signal now in memory is partially adjusted
     $ and therefore messed up.'
	   print *,' Read it in from the disk file again and reapply
     $ the attenuation.'
	   ierr = 2
	   return
	endif

c If we got this far, calculate the clock rate from the sample rate
c - this is as yet about as stable as a plate of jelly (sorry, jello)
c	call sampl_to_clock(iclock,srate)
c	print *,' '
c	print *,' Sample rate was ',srate,
c    $                     '   so Clock rate is ',iclock

	return

c Could not open the file; which file depends on the context
 8000	ierr = 1
	return

	end
