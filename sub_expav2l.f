	subroutine sub_expav2l(time_const1,time_const2,srate,isize,
     $                                  rarray,outmax)

c sub_expav2 without the output array

c Based on graf::dkb0:[sullivan.boom3]expav2c.pro - July 20 1994
c
c; 2-time-constant exponential average - Averaging pressure of A-weighted boom?!
c; one (time_const1) for rising signals, another (time_const2) for decaying.
c; Version 3 = Feb 1 1994
c; Method courtesy of B&K - 2131 Instruction Manual - but they only have one!
c;
c input variables
c    time_const1 = time constant for averaging increasing (abs.) signals
c    time_const2 = time constant for averaging decaying (abs.) signals
c    srate = sample rate of signal array
c               make sure these three are in the same units!
c    isize = number of points in input array
c    rarray = signal array
c output variables
c    outmax = maximum value
c
	real rarray(1),output(2),konstant1,konstant2,normf,normf2,
     $                                 outmax
c
c;time interval between samples is 1/srate (eg 0.000026 secs = .026 msecs)
	tinterval = 1./srate
c
c;time constant = Konstant * tinterval  (according to B&K) (eg 2 msec)
c;              = Konstant / srate
c;     Konstant = time constant * srate = time constant / tinterval
c;             ~= number of samples per Averaging Time
c
	konstant1 = time_const1/tinterval
	konstant2 = time_const2/tinterval
c;
c; apparently I need some kind of normalization factor - squared or not?
c	normf = max(abs(rarray))
	normf = abs(rarray(1))
	do i=2,isize
	   normf = max(normf,rarray(i))
	enddo
	normf2 = normf*normf
c	array = rarray**2/normf2

c;present average=previous average+("volts squared"-previous average)/Konstant 
c	output(1) = array(1)**2/konstant1
	output(1) = (rarray(1)**2/normf2)/konstant1
	outmax = output(1)
	do i=2,isize
           valnow=rarray(i)**2/normf2
	   if (valnow .gt. output(1)) then
	      output(2) = output(1) + (valnow - output(1))/konstant1
	   else
	      output(2) = output(1) + (valnow - output(1))/konstant2
	   endif
	   outmax = max(outmax,output(2))
	   output(1) = output(2)
	enddo

	outmax = outmax*normf2

	return
	end
