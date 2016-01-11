	subroutine sub_ref(outunit,signal,number,srate,ref_volt,equiv_db,
     $				dcoffset,ierr)

c routine to calculate the reference voltage and equivalent pressure level 
c (in dB) from a pure tone signal
c                                              June 26 1990
	real signal(1)
	integer outunit

	ierr = 0

c let's NOT assume we have integer numbers of cycles
c let's average 5 points to try to find the zero crossing
c (simpler than drawing a line?)
c	do istart=1,number-4
c	   do i=1,5

c I'm assuming we have steady state, so we can start at the beginning
c except I don't trust the first sample, so I'll junk that one
	rn = float(number-1)
c Then again, let's assume we know the frequency and the step rate accurately
c (accurately enough for dBA, etc)	      
	print *,' What frequency is this tone?'
	read *,cal_freq
c how many samples per cycle?
	samppcycl = srate / cal_freq
	isamppcycl = int(samppcycl)
c how many complete cycles in our signal?
	ncycles = int(rn/samppcycl)
c and that's how many samples?
	nsamp = nint(ncycles*samppcycl)

c initialize the D.C. component
	dc = 0.0
c initialize the RMS component
	tempms = 0.0
c initialize the maximum and minimum values (I'm going to find a max. and 
c a min. for each "cycle" {as close as I can judge it with a discrete series}
c and average them across the complete NCYCLES)
	sigmax = 0.0
	sigmin = 0.0
	icycle = 1
c I'm junking the first sample, remember
	nextcycl = (int(icycle*samppcycl)) + 1
c assume positive and negative numbers (so start cycle max. and min. at zero)
	cycmax = 0.
	cycmin = 0.
	do inow = 2,nsamp+1
	   tempms = tempms + signal(inow)**2
	   dc = dc + signal(inow)
	   cycmax = amax1(cycmax,signal(inow))
	   cycmin = amin1(cycmin,signal(inow))
	   if (inow .ge. nextcycl) then
	      sigmax = sigmax + cycmax
	      sigmin = sigmin + cycmin
	      icycle = icycle+1
	      nextcycl = (int(icycle*samppcycl)) + 1
	      cycmax = 0.
	      cycmin = 0.
	   endif
	enddo

	sigmax = sigmax/ncycles
	sigmin = sigmin/ncycles
	rms = sqrt(tempms/nsamp)
	dcoffset = dc/nsamp

	print *,' What rms dB level is this tone?'
	read *,dblevel

	ref_volt = rms
	equiv_db = dblevel

	write (outunit,*) ' cal. factors',sigmax,sigmin,rms,dcoffset

	return
	
	end
