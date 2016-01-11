       SUBROUTINE threshold(splthr)
C
c This routine passes the 1/3 octave band data in a common block, which could
c be changed so the data are passed in as parameters to fit in a more modular
c program.
c
c returns level weighted using the human threshold of hearing
c				BMS Feb 7 2003

c L=level (43 1/3 octave bands)
c SPL=calculated loudness level (calculated from L for bands 1 to 40) 
      REAL*4 L(43),SPL
      COMMON /CRUX/ SPL,L

	real thrdb(29)
c	real hz(29), awt(31)
	real splthr, sum, now

c	data hz / 
c     1  20.,     25.,     31.5,    40.,     50.,     63.,     80., 
c     1  100.,
c     2  125.,    160.,    200.,    250.,    315.,    400.,    500.,    
c     2  630.,
c     3  800.,    1000.,   1250.,   1600.,   2000.,   2500.,   3150.,   
c     3  4000.,
c     4  5000.,   6300.,   8000.,   10000.,  12500./

c Threshold from ISO ISO 266:1987E MAF (see ISO_threshold.xls)
       data thrdb /
     1   74.3, 65.0, 56.3, 48.4, 41.7, 35.5, 29.8, 25.1,
     2   20.7, 16.8, 13.8, 11.2, 8.9, 7.2, 6.0, 5.0,
     3   4.4, 4.2, 3.7, 2.6, 1.0, -1.2, -3.6, -3.9,
     4   -1.1, 6.6, 15.3, 16.4, 11.6 /


c the following is 0 phons, not MAF (threshold)
c     1  72.5283, 63.1014, 54.2712, 46.1848, 39.2837, 32.8897, 27.0421, 
c     1  22.2560, 
c     2  17.7767, 13.8101, 10.7624, 8.0990,  5.7027,  3.8784,  2.5284,  
c     2  1.3147, 
c     3  0.4533,  0.000,  -0.6507, -1.8672, -3.5252, -5.7230, -8.1034, 
c     3  -8.3290, 
c     4  -5.4224,  2.5104, 11.6117, 13.1107, 8.8396/

c	data awt / -50.4, -44.7, -39.2, 
c     1  -34.6, -30.2, -26.1, -22.4, -19.1, -16.0, -13.3, -10.8,
c     2  -8.6, -6.6, -4.8, -3.2, -1.9, -0.8, 0.0, 0.6, 1.0, 1.2, 
c     3  1.3, 1.2, 1.0, 0.6, -0.1, -1.1, -2.4, -4.3, -6.5, -9.2/

c  work with bands 13 to 41 (20Hz to 12.5kHz)

c	print *,hz
c	print *,thrdb
c	print *,awt

	sum = 0.0
	do i=13,41
            now = (L(i) - thrdb(i-12))
c	    now = (L(i) + awt(i-12))
c	    print *,hz(i-12),L(i),now
	    sum = sum + 10.**(now/10.)
	enddo

	splthr = 10. * alog10(sum)

	print *,splthr

 
         RETURN
         END
