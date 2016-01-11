	subroutine flip(numb,srate,outunit,outshort,ipres,name$,
     $				convert,psftopa)
c
c put array size parameters in include file - BMS Aug 4 2005

c name$ passed in as 80 characters - BMS Aug 23 2004
c
c Try to do a proper flip (and then do full wave computation, to see if Front+Back
c     is closer to the total - BMS Feb 24 2004
c I have subtracted 3 dB from the outshort values (where are the outunit ones?)
c (in subf7ad_main or whatever it's called which I don't want to mess with)
c for the dBA, dBC, dBLin values so they apply to the single peak.  The
c loudness metrics also apply to a single peak, because they aren't the
c average of the two, as is true for the total boom. - Feb 24 2004
c

c Increase noinfl (again) - the possible size of the input file - July 16 2002

c Mets version - remove plotting code - BMS May 17 2002

c add the two time-constant model (tc1,tc2 and sub_expav2) - Oct 24 1994
c use unp for the frist half as well as the second (a may have been
c affected by other things)  - Sept 17 1993
c increase storage size to 65536 from 32768 - April 29 1993
c          and again to 131072 - July 19 1993
c3 DON'T add dBA (from 1/3 octave bands) output for now - BMS 2 Dec 1992
c add PNL output - bms 20 oct 1992
c
c Differentiate between whole boom or half booms.  Whole booms have 3 db 
c subtracted from the octave band levels (so we have the average level); half
c booms are left alone NOW ('twas not so before T7).
c For now, I'm leaving out the reflected wave calculations because it takes
c too much time                              - BMS 17 Sept 1991
c
c Add Zwicker loudness - BMS May 29 1991
c
c Use length instead of itwice-1 for the first-half reflection's end point - Apr 8 1991
c (there are problems in the SECOND sample of signals measured across the board,
c perhaps because of a switching transient, and it's getting reflected into the
c end of the signal; I think the new way will reflect the first sample.)
c
c For use with both Full-frequency and High Pass filtered signals (eg measured 
c with non-DC mike).  
c This assumes we have a signal that crosses zero - ie one that has negligible
c D.C. shift so if there was a noticeable offset, it has already been extracted
c		created from FLIP.PRO on GRAF
c                                                BMS Feb 20 1991
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
	logical lasym,lfull
	common /yessym/ lasym,lfull

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
c two time-constant model parameters
	common /twotc/ tc1, tc2
c
c
c process the first half of the wave-form; let's use the zero-crossing point
c if we have it; and the nominal for the filtered signals (4145). 
c On second thoughts, use the first zero crossing point after the first
c peak if the nominal isn't available (ie it's not a boom we've created).
c
c If the signal is filtered, reflect it, but do not rotate it about the time 
c axis; if it's all there, do rotate?
	   if (ifilter .eq. 0) then
	      icross = izerobetween
	      rrotate = -1.0
	   else if (indexnomzero .gt. 0) then
	      icross = indexnomzero
	      rrotate = 1.0
	   else
	      icross = izerobetween
	      rrotate = 1.0
	   endif

c replace everything in the A array after icross with the value the
c pressure waveform has there but keep the number of points the same.
	   if (icross .le. 0) then
	      print *,'No zero crossing point'
	   else if (icross .ge. numb) then
	      print *,'Insane zero crossing point',icross
	   else
c Do the first half 
	      do i=1,icross
	         a(i) = unp(i)
	      enddo
C this is new as of Feb 24 20004;  was just
c Fill the rest with zeros? No, with the last good value
c	      lfull = .false.
c	      do i=(icross+1),numb
c	         a(i) = unp(icross)
c	      enddo
c		print *,'Trying for a proper flip'
	      if (int(numb/2) .lt. icross) then
c	         print *,'Can''t do proper flip'
c Why should that be a problem?  Just have to redefine numb
               newnumb = icross*2
            else
               newnumb = numb
            endif
c	         do i=(icross+1),numb
c	            a(i) = unp(icross)
c	         enddo
c	         lfull = .false.
c	      else

c repeat "mirror" image backwards (or backwards upside-down)
	         do i=(icross+1),icross*2
	            ijk = i-icross -1
	            a(i) = -unp(icross - ijk)
	         enddo
c  fill with first point (negative?)
	         do i=icross*2+1, newnumb
	            a(i) = -unp(1)
	         enddo
	         lfull = .true.
c	      endif
	      write (outunit,*) ' BEFORE Zero crossing point at #',
     $                      icross, unp(icross)
c operate on the whole signal
c	      newnumb = numb
	      sigmax = a(1)
	      sigmin = sigmax
	      do index = 2,newnumb
	         sigmax = amax1(sigmax,a(index))
	         sigmin = amin1(sigmin,a(index))
	      enddo

	      call subf7adw_main(outunit,
     $    newnumb,srate,dbpl,dbc,dba,dblin,dbz0,dbz1,dbpln,dba3)
c3     $           newnumb,srate,dbpl,dbc,dba,dblin,dbz0,dbz1,dbpln)

c newnumb has been changed by subf7ad_main so it's no longer = numb (if it was)
            call sub_expav2l(tc1,tc2,srate,newnumb,a,sigoutmax)

c write out the short form results
c Subtract 3 dB for the new flipped results
	      write (outshort,99) ipres,name$,dbpl,dbc-3,dba-3,dblin-3,
     $           dbz0,dbz1,dbpln,sigmax/psftopa,sigmin/psftopa,newnumb
     $          ,sigoutmax


C this is new as of Feb 24 20004;  was just
c Do the second half - fill the rest with the first good value
c	      lfull = .false.
c Feb 24  can still use this bit
	      if (unp(icross) .eq. 0.0) then
	         mark = icross
	      else
	         mark = icross+1
	      endif
c	      lfull = .false.
c	      do i=mark,power2
c	         a(i) = unp(i)
c	      enddo
c	      do i=1,(mark-1)
c	         a(i) = unp(mark)
c	      enddo
c	      newnumb = numb
c Feb 24 - let the fun begin!  As the a array is twice as long as the signal
c array, there is room to do a complete flip (even though most of it is useless)
c The signal goes from 1 to numb; the bit to flip goes from mark to numb
c Therefore the new signal will be 2*(numb-mark+1) = 2numb - 2mark + 2
	      nnumb = (numb-mark+1)
	      newnumb = 2*nnumb
c	      print *,' Flipped tail is ',newnumb,' points long'
	      lfull = .true.
	      do i=mark,numb
	         a(numb-i+1) = -unp(i)
	      enddo
	      if (newnumb .lt. numb) then
	         do i=mark,power2
	            a(i-numb+newnumb) = unp(i)
	         enddo
c new boom is shorter so need to fill in with the end of the old one
	         do i=power2-numb+newnumb+1,power2
	            a(i) = unp(power2)
	         enddo
	      else
c new boom is longer so just go to its end and ignore the rest of the old one.
	         do i=nnumb+1,power2
	            a(i) = unp(i-nnumb+mark-1)
	         enddo
	      endif
	      

	      write (outunit,*) ' AFTER Zero crossing point at #',
     $                      icross
	      sigmax = a(1)
	      sigmin = sigmax
	      do index = 2,newnumb
	         sigmax = amax1(sigmax,a(index))
	         sigmin = amin1(sigmin,a(index))
	      enddo
	      call subf7adw_main(outunit,
     $      newnumb,srate,dbpl,dbc,dba,dblin,dbz0,dbz1,dbpln,dba3)
c3     $            newnumb,srate,dbpl,dbc,dba,dblin,dbz0,dbz1,dbpln)

c newnumb has been changed by subf7ad_main so it's no longer = numb
	call sub_expav2l(tc1,tc2,srate,newnumb,a,sigoutmax)

c Subtract 3 dB for the new flipped results
	      write (outshort,99) ipres,name$,dbpl,dbc-3,dba-3,dblin-3,
     $            dbz0,dbz1,dbpln,sigmax/psftopa,sigmin/psftopa,newnumb
     $          ,sigoutmax

	   endif

c 99	   format (i4,2x,a20,3f7.2,4f8.2,1x,2f7.2,i8)
c3 99	   format (i4,2x,a20,3f7.2,4f8.2,1x,3f7.2,i8)
 99        format (i4,2x,a20,3f7.2,4f8.2,1x,2f9.4,i8,16x,e12.5)

	   return

cm 99      format (i4,2x,a20,3f7.2,4f8.2,1x,2f7.2,4f6.2,e12.5)
	   end
