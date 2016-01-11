       SUBROUTINE Pl
C
C  CALCULATES PERCEIVED LEVEL OF SPECTRA BASED ON
C  1/3 OCTAVE BANDS FROM 1 Hz TO 10 kHz.
C
c                         Dave McCurdy; modified for use with a
c                         single spectrum by BMS, Feb 12 1988
c                         Extended the frequency range, Feb 19 1988
C      (Can't be directly extended up in frequency - 12.5 KHz doesn't work)
c ref1 = S.S.Stevens, `Perceived Level of Noise by Mark VII and Decibels (E)'
c                  JASA 51,2(2),575-593, Feb 1972
c ref2 = K.S.Pearsons and R.L.Bennett, `Handbook of Noises Ratings'
c                  NASA CR-2376

c This routine passes the 1/3 octave band data in a common block, which could
c be changed so the data are passed in as parameters to fit in a more modular
c program.

c L=level (43 1/3 octave bands)
c SPL=calculated loudness level (calculated from L for bands 1 to 40) 
      REAL*4 L(43),SPL
      COMMON /CRUX/ SPL,L

      REAL*4 Noy(43),Noys

      REAL*4 Sm(93),Fs(93)
      REAL*4 Smax

C  READ IN DATA FOR TABLE Sm      

      DATA Sm /.181,.196,.212,.230,.248,.269,.290,.314,.339,.367,
     1    .396,.428,.463,.500,.540,.583,.630,.680,.735,.794,
     1    .857,.926,1.00,1.08,1.17,1.26,1.36,1.47,1.59,1.72,
     1    1.85,2.00,2.16,2.33,2.52,2.72,2.74,3.18,3.43,3.70,
     1    4.00,4.32,4.67,5.04,5.44,5.88,6.35,6.86,7.41,8.00,
     1    8.64,9.33,10.1,10.9,11.8,12.7,13.7,14.8,16.0,17.3,
     1    18.7,20.2,21.8,23.5,25.4,27.4,29.6,32.0,34.6,37.3,
     1    40.3,43.5,47.0,50.8,54.9,59.3,64.0,69.1,74.7,80.6,
     1    87.1,94.1,102.,110.,119.,128.,138.,149.,161.,174.,
     1   188.,203.,219./

C READ IN DATA FOR TABLE Fs      
      DATA Fs /.100,.122,.140,.158,.174,.187,.200,.212,.222,.232,
     1    .241,.250,.259,.267,.274,.281,.287,.293,.298,.303,
     1    .308,.312,.316,.319,.320,.322,.322,.320,.319,.317,
     1    .314,.311,.308,.304,.300,.296,.292,.288,.284,.279,
     1    .275,.270,.266,.262,.258,.253,.248,.244,.240,.235,
     1    .230,.226,.222,.217,.212,.208,.204,.200,.197,.195,
     1    .194,.193,.192,.191,.190,.190,.190,.190,.190,.190,
     1    .191,.191,.192,.193,.194,.195,.197,.199,.201,.203,
     1    .205,.208,.210,.212,.215,.217,.219,.221,.223,.224,
     1    .225,.226,.227/
C
c Initialize variables
         DO 1411 J=1,43
            Noy(J) = 0.0
 1411    CONTINUE
         Noys=0
         Smax=0

c Look at every 1/3 octave band (Nu)
         DO 1458 Nu=1,40
c Find the 1/3 o.b. level in dB
            P=L(Nu)
            IF (Nu-19 .LT. 0) THEN
c If the band center frequency is less than 80 Hz
               Pp=160.-19./Nu*(160.-P)
               Ppp=115.-26./19.*(115.-Pp)
               IF (Ppp .GT. 76.) Ppp=Pp-10.5
               IF (Ppp .GT. 121.) Ppp=160.-26./Nu*(160.-P)
            ELSE IF (Nu-26 .LT. 0) THEN
c If the band c.f. is greater than or equal to 80 and less than 400 Hz
               Pp=P
               Ppp=115.-26./Nu*(115.-Pp)
               IF (Ppp .GT. 76.) Ppp=Pp-(26.-Nu)*1.5
               IF (Ppp .GT. 121.) Ppp=160.-26./Nu*(160.-P)
            ELSE
c If the band c.f. is greater than or equal to 400 Hz
               Ppp=P
            ENDIF
C
            IF (Nu-31 .LE. 0) THEN
c If the band c.f. is less than or equal to 1250 Hz
               Pppp=40.
            ELSE IF (Nu-32 .LE. 0) THEN
c If the band c.f. is greater than 1250 and less than or equal to 1600 Hz
c (i.e. for 1600 Hz band)
               Pppp=38.
            ELSE IF (Nu-33 .LE. 0) THEN
c If the band c.f. is greater than 1600 and less than or equal to 2000 Hz
c (i.e. for 2000 Hz band)
               Pppp=36.
            ELSE IF (Nu-34 .LE. 0) THEN
c If the band c.f. is greater than 2000 and less than or equal to 2500 Hz
c (i.e. for 2500 Hz band)
               Pppp=34.
            ELSE IF (Nu-39 .LE. 0) THEN
c If the band c.f. is greater than 2500 and less than or equal to 8000 Hz
               Pppp=32.
            ELSE
c If the band c.f. is greater than 8000 Hz
               Pppp=36.
            ENDIF
c
            Noy(Nu)=2.**((Ppp-Pppp)/9.)
            Noys=Noys+Noy(Nu)
            Smax=AMAX1(Smax,Noy(Nu))  
c	    print *,Nu,Noy(Nu)
 1458    CONTINUE

         Fofsm=0.
         S=Smax
         IF (S .GE. Sm(1)) THEN
            DO 1471 I=2,93
               IF (S-Sm(I) .LT. 0.) THEN
                  Fofsm=Fs(I-1)+(Fs(I)-Fs(I-1))*(S-Sm(I-1))
     1                        /(Sm(I)-Sm(I-1))
                  GOTO 1473
               ELSE IF (S-Sm(I) .EQ. 0.) THEN
                  Fofsm=Fs(I)
                  GOTO 1473
               ENDIF
 1471       CONTINUE
            Fofsm=Fs(93)
         ENDIF

 1473    St=Smax+Fofsm*(Noys-Smax)
         Spl=32+29.89735285*ALOG10(St)

C	 print *,Smax,Noys,Fofsm,Spl
 
         RETURN
         END
