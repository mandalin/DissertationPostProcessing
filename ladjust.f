	logical function ladjust(slevel,cal_fac,rarray,iarray,number,
     2		adjust,ierr)

c (Oct 10 2000) O2 version has lost the ability to:
c Function to adjust the level of a signal and indicate overflow without crashing
c If any integer errors occur in this function, LADJUST will be set
c to .false. 

c INPUT  - slevel = requested ATTENUATION to be applied to this signal 
c          cal_fac = the calibration factor which is applied to all signals

c OUTPUT - iarray = array in which to store the integerized 
c						scaled time history
c          rarray = array in which to store the real scaled time history
c          number = number of samples in the time history
c          adjust = the scaling applied, used to restore the signal in the
c                   calling routine if the same signal is to be played
c                   with a different attenuation
c          ierr = error code

	integer*2 iarray(*)
	real*4 rarray(*)

c including an error-handler to catch integer errors 
c	external handli
c
c establish the error handler
c	call LIB$ESTABLISH(handli)
c assume success
	ladjust = .true.

	ierr = 0
	adjust = 10.**((-slevel + cal_fac)/20.)
	do i=1,number
	   rarray(i) = rarray(i) * adjust
c scale to 16 bit integer, where 1.0 = 32767
	   iarray(i) = int(rarray(i) * 32767.)
	enddo

c If we got here, we didn't detect an error so we can revert to the
c normal error handling
c	call LIB$REVERT

	return

	end

	integer*4 function handli(sigargs, mechargs)

c Let's try to build a condition handler to intercept explosions of
c the calling routine (i.e. integer overflows) and to return to
c the program in such a case rather than to crash

c Copied from the example p9-27 in the VAX FORTRAN User's Manual
c (see also page 5-5)

	integer*4 sigargs(*), mechargs(5)

c	include '($SSDEF)'
c	include '($MTHDEF)'

c Assume RESIGNAL (ie initialize handl)
c	handl = SS$_RESIGNAL

c check for integer overflows and zero divides 

c	if (LIB$MATCH_COND(  SIGARGS(2),
c     $        SS$_INTOVF,
c     $	      SS$_INTDIV  )  .ne. 0) then
c	   mechargs(4) = .false.
c	   call SYS$UNWIND(,)
c	endif

	return
	end
 
