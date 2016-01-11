	subroutine flip_events(numb,srate,outunit,outshort,ipres,name$,
     $				convert,psftopa)
c
c put array size parameters in include file - BMS Aug 4 2005

c name$ passed in as 80 characters - BMS Aug 23 2004
c
c Increase noinfl (again) - the possible size of the input file - July 16 2002

c  Mets code - remove plotting option - BMS May 18 2002
c
c takes the points defined by find events and does loudness calculation
c of each "event" (but doesn't flip anything - any more than FLIP does now).
c Also do "first half" and "second half" - whatever that means for multi-peak
c waves, but Jack still believes in asymmetry (possibly because he can spell it)
c
c		created from FLIP_SHORT.FOR
c                                                BMS Sept 8 1993
c
c We have room for 2**17 data points
c        parameter (exponent = 17)
c        parameter (power2 = 131072)
c        parameter (halfp2a1 = 65537)
c parameters now in include file
	include 'sigpair_size.inc'

c stuff to pass to the loudness routines
	integer outunit,outshort
	character*80 name$

c LFULL tells whether it's a fullwave or a half wave analysis
	logical lasym,lfull,lmv
	common /yessym/ lasym,lfull,lmv

c do we plot?
	logical lplot
	common /doplot/ lplot

c need to store the unprocessed array somewhere
        real a(power2),unp(power2),aplot(power2)
        common /nwavearray/ a,unp
c
c pity idelay_used is in this common block, but so it goes
        logical linv_used
        character*80 inv_name, orig_name
        character*1 cans
        character*8 ntime
        character*9 ndate
        common /footer/ linv_used, inv_name, orig_name, srate_inv_used,
     $             idelay_used, ntime, ndate
c
c pass in common the NINFL time and pressure pairs used to define this wave
        parameter (noinflp = 10000)
        real*4 press(noinflp), atime(noinflp)
        common /boomcom/ press,atime,ninfl,srate_boom
c
        common /flippts/ ifilter,indexmiddle,izerobetween,
     $       indexminmiddle,indexnomzero,indexpeak,indexotherpeak,
     $       indexstart,indexfirstknee,indexend,indexlastknee,
     $       tfirstknee,pfirstknee,tlastknee,plastknee,
     $	     tfirstpeaknom,pfirstpeaknom,tlastpeaknom,plastpeaknom,
     $	     tstartnom,tendnom
c
c
c new common block for the variables defined in FIND_EVENTS
        parameter (max_events = 50)
        common /def_events/ num_events,ievents(max_events),
     $         itype_events(max_events),
     $         istart_events(max_events),iend_events(max_events)
c
c first do "front" and "back" "halves" (expansion and contraction phases)
c but where is the cross-over?  depends on zero crossings between +ve
c and -ve peaks, and let's hope there's only one such! If there's more than one
c use the one after the last +ve peak.  So start at the back to find it?
c Find type 0 between -ve and +ve peaks (using reverse order)

c find a +ve peak starting at the back
	now_event=num_events
	do while (now_event .gt. 1 .and. 
     $                    itype_events(now_event) .le. 0)
	   now_event = now_event-1
	enddo
c now go the other way to find a zero crossing
	do while (now_event .le. num_events .and. 
     $                    itype_events(now_event) .ne. 0)
	   now_event = now_event+1
	enddo
c by now now_event should be the zero crossing point (if there was one)
	if (now_event .lt. num_events) then
	   ifilter = 0
	   izerobetween = ievents(now_event)
	   call flip(numb,srate,outunit,outshort,ipres,name$,
     $				convert,psftopa)
	else
	   print *,' Couldn''t find the ''zero crossing point'''
	endif

c now process the (unpredictable number of) events of the wave-form;
	lfull = .false.
	iamnow = 0
	do now_event=1,num_events
 
	   if (abs(itype_events(now_event)) .eq. 99) then
	      print 
     $  *,'A peak crosses zero (type ',itype_events(now_event),' )'
	      print *,'I cannot handle that yet'
	   else if (itype_events(now_event) .ne. 0) then
c we have a peak
	      now_start=istart_events(now_event)
	      now_end=iend_events(now_event)
c
c replace everything in the A array before & after the peak with the value the
c pressure waveform has there but keep the number of points the same.
	      if (now_start .le. 0) then
	         print *,'No start point'
	      else if (now_end .le. 0) then
	         print *,'No end point'
	      else if (now_end .gt. numb) then
	         print *,'Insane end point',now_end
	      else
	         do i=1,now_start-1
	            a(i) = unp(now_start)
	         enddo
	         do i=now_start,now_end
	            a(i) = unp(i)
	         enddo
	         do i=now_end+1,numb
	            a(i) = unp(now_end)
	         enddo
	         iamnow = iamnow+1
	         write (outunit,*) iamnow,' event at #',ievents(now_event)
c operate on the whole signal
	         newnumb = numb
c plot a signal in the original units (pseudo-volts; actually almost PSF - I'm
c sorry know I didn't stick to them instead of Pascals)
	         if (lplot) then
	            jstart = 1
	            isamp = newnumb
	            do index = 1,newnumb
	               aplot(index) = a(index) / convert
	            enddo
c	            call plot_it_var(aplot,newnumb,jstart,srate,isamp)
	            print *,'HRK'
	            read (*,'(a)') cans
	         endif
	         sigmax = a(1)
	         sigmin = sigmax
	         do index = 2,newnumb
	            sigmax = amax1(sigmax,a(index))
	            sigmin = amin1(sigmin,a(index))
	         enddo

	         call subf7adw_main(outunit,
     $      newnumb,srate,dbpl,dbc,dba,dblin,dbz0,dbz1,dbpln,dba3)
c3     $           newnumb,srate,dbpl,dbc,dba,dblin,dbz0,dbz1,dbpln)
c write out the short form results
	         write (outshort,99) ipres,name$,dbpl,dbc,dba,dblin,
     $           dbz0,dbz1,dbpln,sigmax/psftopa,sigmin/psftopa,newnumb

	      endif
	   endif
	enddo


 99	format (i4,2x,a20,3f7.2,4f8.2,1x,2f7.2,i8)
c3 99	format (i4,2x,a20,3f7.2,4f8.2,1x,3f7.2,i8) 

	return
	end
