	SUBROUTINE Weight(index)

c  Removed reference to J9 for Mets version - BMS May 17 2002
c index = 1 for overall; 2 for dBA; 3 for dBB; 4 for dBC; 5 for dBD; 6 for dBE;

C  Common block of standard weighting data

	REAL*4 Overall_data(24),A_weighting(24),B_weighting(24),
     1     C_weighting(24),D_weighting(24),E_weighting(24),
     2     L1_weighting(27),L2_weighting(27)
	COMMON /Weight_com/ Overall_data,A_weighting,B_weighting,
     1     C_weighting,D_weighting,E_weighting,
     2     L1_weighting,L2_weighting

C  Common block of weighting factors

	REAL*4 W(24),W1(27) 
	COMMON /SPL_CALS/ W,W1

	IF (index .EQ. 1) THEN
C   Overall:
	    DO 100 J=1,24
		W(J) = 0.0
 100	    CONTINUE
	    CALL Splcal 

	ELSE IF (index .EQ. 2) THEN
C   A:
	    DO 200 J=1,24
		W(J) = A_weighting(J)
 200	    CONTINUE
	    CALL Splcal 

	ELSE IF (index .EQ. 3) THEN
C   B:
	    DO 300 J=1,24
		W(J) = B_weighting(J)
 300	    CONTINUE
	    CALL Splcal 

	ELSE IF (index .EQ. 4) THEN
C   C:
	    DO 400 J=1,24
		W(J) = C_weighting(J)
 400	    CONTINUE
	    CALL Splcal 

	ELSE IF (index .EQ. 5) THEN
C   D:
	    DO 500 J=1,24
		W(J) = D_weighting(J)
 500	    CONTINUE
	    CALL Splcal 

	ELSE IF (index .EQ. 6) THEN
C   E:
	    DO 600 J=1,24
		W(J) = E_weighting(J)
 600	    CONTINUE
	    CALL Splcal 

c	ELSE IF (index .EQ. 7) THEN
C   L1:
c	    DO 700 J=1,27
c		W1(J) = L1_weighting(J)
c 700	    CONTINUE
c	    CALL Splcal1

c	ELSE IF (index .EQ. 8) THEN
C   L2:
c	    DO 800 J=1,27
c		W1(J) = L2_weighting(J)
c 800	    CONTINUE
c	    CALL Splcal2 

	ELSE
C   IMPOSSIBLE!?
	    PRINT *,' IMPOSSIBLE WIGHTING - I QUIT!'
	    PRINT *,'index'
	    PRINT *,index
	    STOP
	ENDIF

	RETURN
	END

	SUBROUTINE Splcal

C  Common block of weighting factors

	REAL*4 W(24),W1(27) 
	COMMON /SPL_CALS/ W,W1

c L= level (for 43 1/3 o.b.)
c SPL = calculated loudness level (calculated from L for bands 17 to 40)
c                                                       (ie 25Hz to 10kHz)
	real*4 L(43),SPL
        common /crux/ SPL,L

	Psqtot=0.
	DO 3890 J=17,40
	   K=J-16
	   Psqtot=Psqtot+10.**((L(J)+W(K))/10.)
 3890	CONTINUE
	Spl=10.*ALOG10(Psqtot)
	RETURN
	END

	SUBROUTINE Weightdata

C  Common block of standard weighting data

	REAL*4 Overall_DATA(24),A_weighting(24),B_weighting(24),
     1     C_weighting(24),D_weighting(24),E_weighting(24),
     2     L1_weighting(27),L2_weighting(27)
	COMMON /Weight_com/ Overall_data,A_weighting,B_weighting,
     1     C_weighting,D_weighting,E_weighting,
     2     L1_weighting,L2_weighting

C	Overall:
	DATA Overall_DATA / 24*0.0 /

C	A:
	DATA A_weighting / -30.2,-26.2,-22.5,-19.1,-16.1,-13.4,
     1     -10.9,-8.6,-6.6,-4.8,-3.2,-1.9,-0.8,0.0,0.6,1.0,1.2,
     2     1.3,1.2,1.0,0.5,-0.1,-1.1,-2.5 /

C	B:
	DATA B_weighting / -11.6,-9.3,-7.4,-5.6,-4.2,-3.0,-2.0,
     1     -1.3,-0.8,-0.5,-0.3,-0.1,4*0.0,-0.1,-0.2,-0.4,-0.7,
     2     -1.2,-1.9,-2.9,-4.3 /

C	C:
	DATA C_weighting / -1.3,-0.8,-0.5,-0.3,-0.2,-0.1,9*0.0,
     1     -0.1,-0.2,-0.3,-0.5,-0.8,-1.3,-2.0,-3.0,-4.4 /

C	D:
	DATA D_weighting / -12.8,-10.9,-9.0,-7.2,-5.5,-4.0,-2.6,
     1     -1.6,-0.8,-0.4,-0.3,-0.5,-0.6,0,2.0,4.9,7.9,10.6,11.5,
     2     11.1,9.6,7.6,5.5,3.4 /

C	E:
	DATA E_weighting / -17.6,-14.2,-10.5,-9.0,-7.5,-6.0,-4.5,
     1     -3.0,-1.5,6*0.0,2.0,4.0,6.0,5*8.0,4.0 /

C	L1:
	DATA L1_weighting / -41.8,-34.8,-28.7,-23.8,-19.5,-15.9,
     1     -13.1,-10.9,-8.8,-7.4,-6.2,-5.2,-4.3,-3.4,-2.5,-1.4,
     2     0.,2.,4.9,7.9,10.6,11.5,11.1,9.6,7.6,5.5,3.4 /

C	L2:
	DATA L2_weighting / -41.8,-34.8,-28.7,-23.8,-19.5,-15.9,
     1     -13.1,-10.9,-8.8,-7.4,-6.2,-5.2,-4.3,-3.4,-2.5,-1.4,0.,
     2     2.,4.9,7.9,10.6,11.5,11.1,9.6,7.6,5.5,3.4 /

	RETURN
	END
