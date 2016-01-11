	SUBROUTINE Pnl

C CALCULATES PERCEIVED NOISE LEVEL OF SPECTRA BASED ON         
C 1/3 OCTAVE BANDS FROM 50 Hz TO 10 kHz.   
c
c                         Dave McCurdy; modified for use with a
c                         single spectrum by BMS, Oct 20 1992
c           Corrected April 2 1993 (based on CR-2376;
c     Handbook of Noise Metrics - Karl S. Pearsons and Ricarda L. Bennett 
c                               (April 1974)
c           Re-Corrected May 25 1993
C  
c Tables defined in pnldata.f
c
C NOTE:  This program for computing PNL differs from 'PNL2' in that      
C       this one also uses tables M1,L1,M2,L2 in the computations.       
C       (Minimum Noy is .1)      
C  
C FOR L1 .LE. L .LT. L2     Noy=10**(M1*(L-L1))      
C WRONG!!!!  FOR L2 .LE. L .LT. L3     Noy=10**(M2*(L-L2))      
C Corrected is FOR L2 .LE. L .LT. L3     Noy=10**(M2*(L-L3))      
c plus L3a in this formulation differs from the FAR 36 version used in PNL2
c plus LCa in this formulation differs from the FAR 36 version used in PNL2
c plus M3a in this formulation differs from the FAR 36 version used in PNL2
C FOR L3 .LE. L .LT. Lc     Noy=10**(M3*(L-L3))      
C FOR Lc .LE. L .LE. 150    Noy=10**(M4*(L-L4))      

c L=level (43 1/3 octave bands)
c SPL=calculated loudness level (calculated from L for bands 1 to 40) 
        REAL*4 L(43),SPL
        COMMON /CRUX/ SPL,L

C CONTAINS DATA TABLES FOR PNL CALCULATIONS
	INCLUDE 'pnl_pl.cmn'


	REAL*4 Lvalue

	Noys=0.
	Xmaxx=0.
	DO 1069 I=17,40
	   Lvalue=L(I)
	   J=I-16
	   Noy(J)=0.
c Wherever this code came from, it differs slightly and probably undetectably
c from CR-2376 as I (BMS) read it; in order to bring them into line
c I am adding the next two IF statements, May 25 1993 - so who cares anyway?
	   if (Lvalue .ge. Tabl1(j)) then
	    if (Lvalue .gt. 150.0) then
                   print *,'Noy value undefined in CR-2376'
	           print *,'I shall make up a value ...'
	    endif 
	    IF (Lvalue .LT. Tabl3a(J)) THEN
	      Y=Lvalue-Tabl2(J)      
	      IF (Y .LT. 0) THEN
	         Noy(J)=.1*10.**(Tabm1(J)*(Lvalue-Tabl1(J)))
	      ELSE
	         Noy(J)=10.**(Tabm2(J)*(Lvalue-Tabl3a(J)))     
	      ENDIF
	    ELSE
	      X=Lvalue-Tablca(J)
	      IF ((X .LE. 0) .AND. (J .LE. 9 .OR. J .GT. 22)) THEN
	         Noy(J)=10.**(Tabm3a(J)*(Lvalue-Tabl3a(J)))
	      ELSE
	         Noy(J)=10.**(Tabm4(J)*(Lvalue-Tabl4(J)))
	      ENDIF
	    ENDIF
	    Noys=Noys+Noy(J)
	   endif
 1069	CONTINUE

	Xmaxx=Noy(1)

	DO 1075 M=2,24
	   Xmaxx=AMAX1(Xmaxx,Noy(M))
 1075	CONTINUE

	Nn=Xmaxx+.15*(Noys-Xmaxx)        
c May 25 1993; the following code rewritten
        IF (Nn .EQ. 0.) then
	   PRINT *,'N=0 WITH NOYS',Noys,'AND XMAX=',Xmaxx 
	   Spl = 0.
	else
	   Spl=40.+ALOG10(Nn)/.03010299957   
	endif

	RETURN
	END
