       subroutine cl(splc,pref2,wtxt)
C
c put array size parameters in include file - BMS Aug 4 2005

c increase storage space from 65536 to 131072 - July 19 1993
c increase storage space from 32768 to 65536 - April 29 1993
C  Calculates c-weighted level of spectrum based on
c  returned results from FFT passed through in array xt in common spec.
c  Returned value in splc.
c  Square of reference pressure passed through in pref2.
C
c                            B.M.Sullivan Sept 23 1988
c ref1 = American National Standards Institute S12.4
c          "Specifications for Sound Level Meters"

c       parameter (power2 = 131072)
c parameters now in include file
       include 'sigpair_size.inc'
       real fdelta, XT(power2)
       integer m
       common /spec/ XT, m, fdelta

       real wtxt(power2)

       parameter (f1=20.598997,f4=12194.22,const=1.00715)

       const2 = const*const

       splc = 0.0
C First frequency is zero, where the result is 0.0
       do 100 i=2,m
          freq_now = fdelta*(i-1)
          freq_ratio1 = (freq_now/f1)**2
          freq_ratio4 = (freq_now/f4)**2
          weightc = const2 * (freq_ratio1**2) /
     1               ( ((1 + freq_ratio1)**2) * 
     2               ((1 + freq_ratio4)**2) )
c (With the 1/3 o.b. levels, we divided by 0.07 (sec) because of the integration
c time of the ear (a la Johnson and Robinson), as the band levels were destined 
c for the PL routine. Here, because of the fact that sound level meters are 
c assumed to have a 1 sec. integration time, we divide by 1.0 (sec) which is
c invisible.)
	  wtxt(i) = xt(i) * weightc 
          splc = splc + wtxt(i) 

 100   continue


       if (splc .gt. 0.0) then
          splc = 10.0*log10(splc/pref2)
       else
          splc = 0.0
       endif
 
       return
       end


       subroutine al(spla,pref2,wtxt)
C
c put array size parameters in include file - BMS Aug 4 2005

c make it a tiny (and so far as I can see unneccesary) bit safer
c            by using ctemp (qv) - BMS Oct 3 1994
c increase storage space from 32768 to 131072 - july 19 1993
C  Calculates a-weighted level of spectrum based on
c  returned results from FFT passed through in array xt in common spec.
c  Returned value in spla.
c  Square of reference pressure passed through in pref2.
C
c                            B.M.Sullivan Oct 19 1988
c ref = "Algebraic Expression for A-Weighting Response" R.M.Aarts, Journal of
c          Sound and Vibration (1987), vol 115(2), p 372

c       parameter (power2 = 131072)
c parameters now in include file
       include 'sigpair_size.inc'
       common /spec/ xt(power2), m, fdelta

       real wtxt(power2)

       complex compfrq,compwt,compdenom,compfact(6),ctemp
       real param(6),factor

       data param / 20.6,20.6,107.7,737.9,12200.0,12200.0/
       data consta /7.397234E+9/

       pi = acos(-1.)
       piby2 = 2.*pi
       do 100 j=1,6
          factor = piby2 * param(j)
          compfact(j) = cmplx(factor,0.0)
 100   continue

       spla = 0.0
C First frequency is zero, where the result is 0.0, so start at 2
       do 200 i=2,m
          freq_now = fdelta*(i-1)
c Redefine frequency from Hz to radians, and make it complex
          freq_rad = freq_now * piby2
          compfrq = cmplx(0.0,freq_rad)
          compdenom  = (compfrq + compfact(1))
          do 150 j=2,6
             compdenom  = compdenom  * (compfrq + compfact(j))
 150      continue
          ctemp = compdenom / consta
          compwt = ( (compfrq**4) / ctemp )
          weighta = cabs (compwt)

c This is a weighting on pressure (not pressure**2), so square it for XT use.
c (With the 1/3 o.b. levels, we divided by 0.07 (sec) because of the integration
c time of the ear (a la Johnson and Robinson), as the band levels were destined 
c for the PL routine. Here, because of the fact that sound level meters are 
c assumed to have a 1 sec. integration time, we divide by 1.0 (sec) which is
c invisible.)
	  wtxt(i) = xt(i) * weighta * weighta
          spla = spla + wtxt(i)
 200   continue

       if (spla .gt. 0.0) then
          spla = 10.0*log10(spla/pref2)
       else
          spla = 0.0
       endif
 
       return
       end


       subroutine lin(spllin,pref2)
C
c put array size parameters in include file - BMS Aug 4 2005

c increase storage space from 32768 to 131072 - july 19 1993
C  Calculates unweighted (linear) level of spectrum based on
c  returned results from FFT passed through in array xt in common spec.
c  Returned value in spllin.
c  Square of reference pressure passed through in pref2.
C
c                            B.M.Sullivan Aug 30 1990

c       parameter (power2 = 131072)
c parameters now in include file
       include 'sigpair_size.inc'
       common /spec/ xt(power2), m, fdelta

       spllin = 0.0
C First frequency is zero, where the result is 0.0
       do 100 i=2,m
c (With the 1/3 o.b. levels, we divided by 0.07 (sec) because of the integration
c time of the ear (a la Johnson and Robinson), as the band levels were destined 
c for the PL routine. Here, because of the fact that sound level meters are 
c assumed to have a 1 sec. integration time, we divide by 1.0 (sec) which is
c invisible.)
          spllin = spllin + xt(i)
 100   continue

       if (spllin .gt. 0.0) then
          spllin = 10.0*log10(spllin/pref2)
       else
          spllin = 0.0
       endif
 
       return
       end
