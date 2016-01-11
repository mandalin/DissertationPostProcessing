       subroutine onethirdoctaves(fdelta,yt,m,l,pref2,lfull)

c called by subroutine subf7 (source name some variation on SUBF7Z)
c      extracted as a separate routine - BMS July 8 1994

c pass in fdelta = bin width of FFT
c         yt = magnitudes returned by FFT (in compatible units!)
c                                      (may or may not be weighted)
c         m = number of bins/ FFT coefficients
c         pref2 = square of the reference pressure
c         lfull = flag to show if this is a full waveform to be averaged or not
c return  l = array of 1/3 octave band levels

       real UF(0:41),sum,dblevel,bandenergy
       real fdelta,yt(1),pref2
       real l(43)
       logical lfull
       integer NBAND

c The 1/3 octave band boundaries are:-
       data ( UF(i), i=0,41 )/   1.118034,     1.414217,
     1            1.788854,     2.236068,     2.806243,     3.549648,
     1            4.472136,     5.612486,     7.099296,     8.944272,
     1           11.180340,    14.142136,    17.888544,    22.360680,
     1           28.062430,    35.496479,    44.721360,    56.124860,
     1           70.992957,    89.442719,   111.803399,   141.421356,
     1          178.885438,   223.606798,   280.624304,   354.964787,
     1          447.213595,   561.248608,   709.929574,   894.427191,
     1         1118.033989,  1414.213562,  1788.854382,  2236.067978,
     1         2806.243040,  3549.647870,  4472.135955,  5612.486080,
     1         7099.295740,  8944.271910, 11180.339887, 14142.135622/

c Now do the 1/3 octave band integration;  (differentiate carefully 
c between c bins and bands!  Bands are 1/3 octaves, bins are FFT steps.
c The first bin (yt(1)) is centered at frequency 0.0. Let us work up 
c from that, through bins at intervals of FDELTA, until we find a 
c frequency within the 1/3 octave band range, then sum up the yt's 
c until we reach a frequency c outside the range; then we move on 
c to the next 1/3 o.b.     *****  DO NOT INCLUDE D.C. (bin 1)  *****
       INOW = 2

		ijkl = 0
c		print *,'ijk',ijkl

c bin top frequency
       FREQNOW = fdelta/2. + fdelta
c		print *,freqnow
c Define the 1/3 octave band number

	do 200 NBAND=1,40
c		print *,'top'
c A and B are the boundaries of this 1/3 octave band:-
           BOTTOMFREQ = UF(NBAND-1)
           TOPFREQ = UF(NBAND)

c		print *,topfreq,bottomfreq
C SUM is a sum representing Integral(from BOTTOMFREQ to TOPFREQ) of 
C F(omega)**2.d(f), where BOTTOMFREQ and TOPFREQ are expressed in Hz

C First, is the top of this bin at least in part above the bottom 
c of the 1/3 octave band?  If not, go to the next bin.
	do 300 istep=inow,m
              if (freqnow .gt. bottomfreq) go to 310
              freqnow = freqnow + fdelta
 300       continue

c		print *,'300',freqnow,inow,m
c if we're here, we've run out of bins
           go to 500

 310       inow = istep

		ijkl = ijkl + 1
c		print *,ijkl,'   310  ',freqnow,topfreq, bottomfreq

c sum in the partial bin.
c is the top of the bin above the top of the band?  (All band in bin).
           if (freqnow .gt. topfreq) then
              sum = (topfreq - bottomfreq)*yt(istep)/fdelta
c and on to the next band, which will be partially included in this bin
           else
              sum = (freqnow - bottomfreq)*yt(istep)/fdelta
c and on to the next bin
              inow = inow+1
              freqnow = freqnow + fdelta
              do 400 istep = inow,m
c for this bin, is its top above the top of the band?
                 if (freqnow .ge. topfreq) go to 410
c if not, add all its contents into sum
                 sum = sum + yt(istep)
c and go to the next bin,
                 freqnow = freqnow + fdelta
 400          continue
c if we're here, we've run out of bins
c		print *,'400'
              go to 500

c Bin top above band top, so sum in the partial bin
 410          sum = sum +
     $           (topfreq-(freqnow-fdelta))*yt(istep)/fdelta
c on to the next band, which will be partially included in this bin
              inow = istep
           endif
c but first, work out a dB level for this band
c Now, we need to convert this integral (sum) into dB; first, calculate
c the energy within the band (=SUM) & divide by 70msec (.07 sec) 
c to get the units right and to account for the integration time 
c of the ear.
           BANDENERGY = SUM/0.07
c Subtract 3dB because this loudness is due to the energy of the
c whole N-wave, which (usually) is heard as 2 events (as the events
c are separated by a time greater than the integration time of the
c ear) so we need to split the energy into two.  Hence the 3dB
c reduction (before the loudness calculation).
           if (BANDENERGY .LE. 0.0) then
              DBLEVEL = 1.E-10
           else
              DBLEVEL = 10.0*log10(BANDENERGY/PREF2)
              if (lfull) DBLEVEL = DBLEVEL-3.
           endif
           l(NBAND) = DBLEVEL

 200   continue
c got all the bands filled
c		print *,'200'
       go to 700

 500  continue
c		print *,'500',NBAND,NNBAND
C In case we ran out of data before running out of bands (which is
c highly likely), we have to stop integrating and set this and all
c higher bands to 0.0
       do 600 NNBAND=NBAND,40
           l(NNBAND) = 0.0
 600   continue

 700   continue
       return
       end
