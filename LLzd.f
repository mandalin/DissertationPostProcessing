	SUBROUTINE llzd(itype)
c
c McCurdy-METRIC routine modified for use in the Sonic Boom Loudness program
c          B.M.Sullivan (LESC for SAB, ACoD, NASA-Langley, May 28 1991)
c                       (now LMES for SAB, FMAD, NASA-Langley, Oct 24 1995)
c  (ie Structural Acoustics Branch of Fluid Mechanics and Acoustics Division)

C     This program computes Zwicker Phons and was translated
C     from FORTRAN to HP Enhanced BASIC by Charlotte McGary
C     on May 21, 1982.

C     Modified for METRIC 27JUN83 by David A. McCurdy
C                                                                        
C     SOURCE:  Paulus & Zwicker, Acustica 1972, 27, 253-266.

C     INPUT:
C          Lt     =  Array containing the 27 third octave levels
C                    from 27 Hz to 10 kHz.  [The program adds Lt(28)=0]
C          itype  =  0 if Plane Field
C                    1 if Diffuse Field
C     OUTPUT:
C          Nz   =  Loudness in Sones
C          Ns   =  (Has to do with Barks)

C          Llzs =  Loudness in Sones
C          Llzp =  Loudness Level in Phons

c L= level (for 43 1/3 o.b.)
c SPL = calculated loudness level (calculated from L for bands 17 to 40)
c                                                       (ie 25Hz to 10kHz)
	real*4 L(43),SPL
        common /crux/ SPL,L

	REAL*4 Lt(28),Ns(241),Nz,N1,N2,Le,Lehs(20),Lg(3),Kern(21),
     1			Lhs(3),Ni,Llzs,Llzp
	REAL*4 A0(20),Dltg(20),Dled(20),Zg(21),Grenz(16),Tang(8,16),
     1			Ti(11)
	REAL*4 Z,Z1,Z2,Dz,Xp,Gi,Hsf,C63
	INTEGER*2 Option,Mod

	DATA Lhs /63.0,54.0,47.0/
	DATA Lehs/36.0,21.0,12.5,9.0,7.3,6.0,5.0,4.4,12*4.0/
	DATA  A0/10*0.00,-0.50,-1.60,-3.20,-5.40,-5.60,-4.00,-1.50,
     1           			 2.00,5.00,12.00/
	DATA  Dled/2*0.00,0.50,0.90,1.20,1.60,2.30,2.80,3.00,2.00,
     1   0.00,-1.40,-2.00,-1.90,-1.00,0.50,3.00,4.00,4.30,4.00/
	DATA  Dltg/-0.25,-0.60,-0.80,-0.80,-0.50,0.00,0.50,1.10,1.50,
     1   1.70, 2*1.80,1.70,1.60,1.40,1.20,0.80,0.50,0.00,-0.50/
	DATA  Zg/0.90,1.80,2.80,3.50,4.40,5.40,6.60,7.90,9.20,10.60,
     1   12.30,13.80,15.20,16.70,18.10,19.30,20.60,
     2   21.80,22.70,23.60,24.0/
	DATA Grenz/23.50,19.00,15.10,11.90,9.00,6.60,4.60,3.20,2.13,
     1   1.36,0.82,0.43,0.21,0.08,0.03,0.00/
	DATA Tang/13.00, 8.20, 5.70, 5.00, 5.00, 5.00, 5.00, 5.00,
     1     9.00, 7.50, 6.00, 5.10, 4.50, 4.50, 4.50, 4.50,
     1     7.80, 6.70, 5.60, 4.90, 4.40, 3.90, 3.90, 3.90,
     1     6.40, 5.50, 4.70, 4.10, 3.60, 3.20, 3.20, 3.20,
     1     5.60, 5.00, 4.50, 4.30, 3.50, 2.90, 2.90, 2.90,
     1     4.20, 3.90, 3.70, 3.30, 2.90, 2.42, 2.42, 2.42,
     1     3.20, 2.80, 2.50, 2.30, 2.20, 2.20, 2.20, 2.02,
     1     2.80, 2.10, 1.90, 1.80, 1.70, 1.60, 1.60, 1.41,
     1     1.60, 1.50, 1.40, 1.30, 1.20, 1.10, 1.10, 1.02,
     1     1.50, 1.20, 0.94, 0.77, 0.77, 0.77, 0.77, 0.77,
     1     0.72, 0.66, 0.61, 0.54, 0.54, 0.54, 0.54, 0.54,
     1     0.44, 0.41, 0.40, 0.39, 0.39, 0.39, 0.39, 0.39,
     1     0.29, 0.25, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     1     0.15, 0.13, 0.13, 0.13, 0.13, 0.13, 0.13, 0.13,
     1     0.06, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
     1     0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04/


C INPUT "Do you want Zwicker computed in (1=Sones, 2=Phons, 3=Both)?",Option
	   Option=2
C INPUT "ENTER 0 if Plane Field, 1 if Diffuse",Mod
	   Mod=itype

	   Z=0.
	   DO 1698 I=14,40
	      Lt(I-13)=L(I)
 1698	   CONTINUE
	   C63=.064*10.**(.025*Lehs(1))
c this loop does strange things to the bottom 1/3 octave bands
	   DO 1709 I=1,3
	      Lg(I)=Lehs(I)
	      Hsf=.064*10.**(.025*Lhs(I))
	      Ni=Hsf*((1.+.25*10.**(.1*(Lt(I)-Lhs(I))))**.25-1.)
	      Gi=4*((Ni/C63+1.)**4.-1.)
	      Ti(I)=0.
	      IF (Gi .GT. 0.) THEN
		 Xp=ALOG10(Gi)+.1*Lehs(I)
		 Ti(I)=10.**Xp
	      ENDIF
 1709	   CONTINUE

	   DO 1712 I=4,11
	      Ti(I)=10.**(.1*Lt(I))
 1712	   CONTINUE

c summation of first critical bands from 1/3 ob data - results don't agree
c with graphical method (Matt Sneddon, BBN, Nov 6 1995)
	   Gi=Ti(1)+Ti(2)+Ti(3)+Ti(4)+Ti(5)+Ti(6)
	   IF (Gi .GT. 0.) Lg(1)=10.*ALOG10(Gi)
	   Gi=Ti(7)+Ti(8)+Ti(9)
	   IF (Gi .GT. 0.) Lg(2)=10.*ALOG10(Gi)
	   Gi=Ti(10)+Ti(11)
	   IF (Gi .GT. 0.) Lg(3)=10.*ALOG10(Gi)
	   DO 1729 I=1,20
	      IF (I .LE. 3) THEN
		 Le=Lg(I)
	      ELSE
		 Le=Lt(I+8)
	      ENDIF
	      Le=Le-A0(I)
	      Kern(I)=0.
	      IF (Mod .EQ. 1) Le=Le+Dled(I)
	      IF (Le .GT. Lehs(I)) THEN
		 Le=Le-Dltg(I)
		 Hsf=.064*10.**(.025*Lehs(I))
		 Kern(I)=Hsf*((1.+.25*10.**(.1*(Le-Lehs(I))))**.25-1.)
	      ENDIF
 1729	   CONTINUE
	   Kern(21)=0.
	   Nz=.2
	   Z1=0.
	   N1=0.
	   J=16
	   Iz=1
	   Z=.1
	   DO 1773 I=1,21
	      Ig=I-1
	      IF (Ig .GT. 8) Ig=8
 1740	      IF (N1 .GT. Kern(I)) THEN
		 IF (Grenz(J) .LT. Kern(I)) THEN
		    N2=Kern(I)
		 ELSE
		    N2=Grenz(J)
		 ENDIF
		 Dz=(N1-N2)/Tang(Ig,J)
		 Z2=Z1+Dz
		 IF (Z2 .GE. Zg(I)) THEN
		    Z2=Zg(I)
		    Dz=Z2-Z1
		    N2=N1-Dz*Tang(Ig,J)
		 ENDIF
		 Nz=Nz+(N1+N2)/2. * Dz
		 DO 1767 WHILE (Z .LE. Z2)
		    Ns(Iz)=N1-(Z-Z1)*Tang(Ig,J)
		    Iz=Iz+1
		    Z=Z+.1
 1767		 END DO

	      ELSE
	         IF (N1 .LT. Kern(I)) THEN
		    DO 1745 J=1,16
		       IF (Grenz(J) .LT. Kern(I)) GOTO 1746
 1745		    CONTINUE
		 ENDIF
 1746		 Z2=Zg(I)
		 N2=Kern(I)
		 Nz=Nz+N2*(Z2-Z1)
		 DO 1753 WHILE (Z .LE. Z2)
		    Ns(Iz)=N2
		    Iz=Iz+1
		    Z=Z+.1
 1753		 END DO
	      ENDIF

 	      IF (N2 .EQ. Grenz(J)) J=J+1
	      IF (J .GT. 16) J=16
	      N1=N2
	      Z1=Z2
	      IF (Z1 .LT. Zg(I)) GOTO 1740
 1773	   CONTINUE
 
C		Note: Llzs is in Sones

	   Llzs=Nz

C  In order to get Phons from Sones, do the following:
C     Take the Log(Base 2) of Nz (the value in Sones),
C          multiply it by 10, then add 40.
C     Therefore, Llzp=40+10*(ALOG10(Nz)/ALOG10(2))
C                          =40+10*(ALOG10(Nz)/.301)
C                          =40+33.219*ALOG10(Nz)

	   Llzp=40.+33.219*ALOG10(Nz)
     	   Spl=Llzp

	   RETURN
	   END
