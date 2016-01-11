	subroutine find_events(numb,srate,arry)
c
c I hope this will be for use with both Full-frequency and High Pass filtered 
c signals (eg measured with non-DC mike).  
c                                                BMS Sep 7 1993
c Increase noinfl (again) - the possible size of the input file - July 16 2002
c
        real arry(*)
c
c pity idelay_used is in this common block, but so it goes
        logical linv_used
        character*80 inv_name, orig_name
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
c let's have a new common block for the variables we define here
        parameter (max_events = 50)
	common /def_events/ num_events,ievents(max_events),
     $         itype_events(max_events),
     $         istart_events(max_events),iend_events(max_events)
c

c initialize
	num_events = 0
	do i=1,max_events
	   ievents(i) = 0
	   itype_events(i) = 0
	   istart_events(i) = 0
	   iend_events(i) = 0
	enddo

c the boom is defined in press and atime arrays, but the times in atime
c are offset from the time history in arry by float(idelay_used)/srate
c or, in index terms, by :-

	iuse_delay = idelay_used
c	iuse_delay = min(512,idelay_used)

c start is a zero crossing (or should be)
	num_events = 1
	ievents(num_events) = 1
c type 0 = zerocrossing
c type 1 = +ve peak (ie ends above zero)
c type -1 = -ve peak (ie starts below zero)
c (what if both? then below zero end is -99 and above is +99)
c (which doesn't make any sense!!! So treat it with care!)
	itype_events(num_events) = 0
	do i=2,ninfl-1
	   if ( press(i) .gt. press(i-1) ) then
c we have an peak-type event - herein defined as any +ve pressure change;
c WON'T WORK FOR KNEES (or maybe it will - just depends on the point of view)
	      if (press(i-1) .lt. 0.0 .and. press(i) .gt. 0.0) then
c curses! Two events?  Who cares about zero crossings really? Jack does!
c (the other way doesn't matter 'cos the only events that count are +ve going!)
	         num_events = num_events+1
	         if (num_events .gt. max_events) then
	            print *,'We have exceeded the array
     $ size for storing events'
	            go to 1000
	         endif
c index into arry of that nominal event time is
                 ievents(num_events) = 
     $                     indextime(atime(i-1),srate,numb,iuse_delay)
                 itype_events(num_events) = -99
                 num_events = num_events+1
                 ievents(num_events) = 
     $                     indextime(atime(i),srate,numb,iuse_delay)
                 itype_events(num_events) = 99
c skip the zero crossing tests - keep it one event
	         go to 910
	      else if (abs(press(i)) .gt. abs(press(i-1))) then
	         num_events = num_events+1
                 ievents(num_events) = 
     $                     indextime(atime(i),srate,numb,iuse_delay)
                 itype_events(num_events) = 1
	      else
	         num_events = num_events+1
                 ievents(num_events) = 
     $                     indextime(atime(i-1),srate,numb,iuse_delay)
                 itype_events(num_events) = -1
	      endif
	   endif

c a zero crossing may happen between events (nowhere else?)
	   if (press(i) .eq. 0.0) then
c only accept this as a zero crossing if we go from above zero to below zero
c or vice versa.  Is that an adequate definition?
	      if ((press(i+1) .gt. 0.0 .and. press(i-1) .lt. 0.0) .or.
     $	          (press(i-1) .gt. 0.0 .and. press(i+1) .lt. 0.0)) then
	         num_events = num_events+1
	         ievents(num_events) = 
     $                     indextime(atime(i),srate,numb,iuse_delay)
	         itype_events(num_events) = 0
	      endif
	   else if (press(i-1) .gt. 0.0 .and. press(i) .lt. 0.0) then
	      jend = indextime(atime(i),srate,numb,iuse_delay)
	      jstart = indextime(atime(i-1),srate,numb,iuse_delay)
c stop at the point before (in time) the zero crossing, if you can't stop 
c right on it
	      do j=jstart,jend-1
	         if ( (arry(j) .ge. 0.0) .and.
     $                            (arry(j+1) .lt. 0.0) ) then
	            num_events = num_events+1
	            ievents(num_events) = j
	            itype_events(num_events) = 0
	            go to 900
	         endif
	      enddo
 900	      continue

	   else if (press(i-1) .lt. 0.0 .and. press(i) .gt. 0.0) then
	      jend = indextime(atime(i),srate,numb,iuse_delay)
	      jstart = indextime(atime(i-1),srate,numb,iuse_delay)
c stop at the point before (in time) the zero crossing, if you can't stop 
c right on it
	      do j=jstart,jend-1
	         if ( (arry(j) .le. 0.0) .and.
     $                            (arry(j+1) .gt. 0.0) ) then
	            num_events = num_events+1
	            ievents(num_events) = j
	            itype_events(num_events) = 0
	            go to 905
	         endif
	      enddo
 905	      continue
	   endif
 910	   continue
	enddo
c may be an event at the end (but not a zero CROSSING)
	i = ninfl
	if ( press(i) .gt. press(i-1) ) then
c we have an peak-type event - herein defined as any +ve pressure change;
c WON'T WORK FOR KNEES (or maybe it will - just depends on the point of view)
	   if (abs(press(i)) .gt. abs(press(i-1))) then
	      num_events = num_events+1
              ievents(num_events) = 
     $                     indextime(atime(i),srate,numb,iuse_delay)
              itype_events(num_events) = 1
	   else
	      num_events = num_events+1
              ievents(num_events) = 
     $                     indextime(atime(i-1),srate,numb,iuse_delay)
              itype_events(num_events) = -1
	   endif
	endif
c and add the end point (which IS a zero crossing within the meaning of the act)
	num_events = num_events + 1
	ievents(num_events) = numb
	itype_events(num_events) = 0

c is that it?  No, let's define the start and end of each event while we're here
c
c       Z1     E1     E2   Z2       Z3
c        |      |      |   |        |
c        |     / \    / \  |        |
c        |    /   \  /   \ |        |
c        -----     \/     \|    -----
c                          \   /
c                           \ /
c                            |
c                            |
c                           E3
c
c  E1 goes from Z1 to min. between E1 and E2
c  E2 goes from min. between E1 and E2 to Z2
c  E3 goes from Z2 to Z3 (that's the easy one)
c
 1000	do now_event=1,num_events
c is this a peak (type <> 0)?
	   if (itype_events(now_event) .ne. 0) then 
c If there are two events between zero-crossing points, we need to 
c separate them and use the minimum (maximum) between them as the start/end
c of the event.  Otherwise we can use the zero crossing times (indeces).
	      if (itype_events(now_event-1) .eq. 0) then
	         istart_events(now_event) = ievents(now_event-1)
	      else if (itype_events(now_event-1) .eq. 
     $                        itype_events(now_event)) then
	         if (itype_events(now_event) .eq. 1) then
	            num_to_use = ievents(now_event) - ievents(now_event-1)
        	    minmiddle = bmin(arry(ievents(now_event-1)),
     $                     1+num_to_use,indexminmiddle)
	            indexminmiddle=indexminmiddle+ievents(now_event-1)-1
	         else
	            num_to_use = ievents(now_event) - ievents(now_event-1)
        	    minmiddle = bmax(arry(ievents(now_event-1)),
     $                     1+num_to_use,indexminmiddle)
	            indexminmiddle=indexminmiddle+ievents(now_event-1)-1
	         endif
	         istart_events(now_event) = indexminmiddle
	      else
c what else could there be?  Apart from 99's - then what do I do?
	         print *,'Help #1 in find_events'
	         istart_events(now_event) = 1
	      endif

	      if (itype_events(now_event+1) .eq. 0) then
	         iend_events(now_event) = ievents(now_event+1)
	      else if (itype_events(now_event+1) .eq. 
     $                        itype_events(now_event)) then
	         if (itype_events(now_event) .eq. 1) then
	            num_to_use = ievents(now_event+1) - ievents(now_event)
        	    minmiddle = bmin(arry(ievents(now_event)),
     $                     1+num_to_use,indexminmiddle)
	            indexminmiddle=indexminmiddle+ievents(now_event)-1
	         else
	            num_to_use = ievents(now_event+1) - ievents(now_event)
        	    minmiddle = bmax(arry(ievents(now_event)),
     $                     1+num_to_use,indexminmiddle)
	            indexminmiddle=indexminmiddle+ievents(now_event)-1
	         endif
	         iend_events(now_event) = indexminmiddle
	      else
c what else could there be?  Apart from 99's - then what do I do?
	         print *,'Help #2 in find_events'
	         iend_events(now_event) = numb
	      endif
	   endif
	enddo
c
	print *,num_events,' events'
	do i=1,num_events
	   print *,ievents(i),itype_events(i),
     $            istart_events(i),iend_events(i),arry(ievents(i))
	enddo

	return
	end
