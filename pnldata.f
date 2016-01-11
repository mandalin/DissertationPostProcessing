	SUBROUTINE pnldata

c rename the include file! - BMS Oct 20 1992
c change some of the values & include different arrays for FAR36 vs. 
c Pearsons&Bennett NASA CR-2376 (which is better than NASA CR-3406?)
c                             BMS April2 1993
c     Brought into line with CFR Title 14, Part 36, Appendix A of 1-01-2004 - April 1 2004

c               PART 36 -- NOISE STANDARDS: AIRCRAFT TYPE AND AIRWORTHINESS CERTIFICATION 
c       (So far all I've done is add comments and revise Tablcf)
c       [variable names in square brackets are those used in table A36-3 of Appendix A]

C CONTAINS DATA TABLES FOR PNL CALCULATIONS
	INCLUDE 'pnl_pl.cmn'

C READ IN DATA FOR TABLE M1      [M(d)]
	DATA Tabm1 / 0.079520,0.068160,0.068160,0.059640,10*0.053013,
     1     0.059640,2*0.053013,2*0.047712,
     2     2*0.053013,0.068160,0.079520,0.0596401 /

C READ IN DATA FOR TABLE L1      [SPL(d)]
	DATA Tabl1 / 49.,44.,39.,34.,30.,27.,24.,21.,18.,5*16.,
     1     15.,12.,09.,05.,04.,05.,06.,10.,17.,21. / 

C READ IN DATA FOR TABLE M2      [M(e)]
	DATA Tabm2 / 2*0.058098,0.052288,0.047534,2*0.043573,
     1     0.040221,0.037349,7*0.034859,0.040221,0.037349,4*0.034859,
     2     0.037349,0.037349,0.043573 /

C READ IN DATA FOR TABLE L2      [SPL(e)]
	DATA Tabl2 / 55.,51.,46.,42.,39.,36.,33.,30.,27.,5*25.,
     1     23.,21.,18.,15.,14.,14.,15.,17.,23.,29. /

C READ IN DATA FOR TABLE M3        OLD
	DATA Tabm3 / 0.043478,0.040570,2*0.036831,0.035336,
     1     2*0.033333,0.032051,0.030675,13*0.0,2*0.042285 /

C READ IN DATA FOR TABLE L3        OLD
	DATA Tabl3 / 64.,60.,56.,53.,51.,48.,46.,44.,42.,13*0.,37.,41. /

C READ IN DATA FOR TABLE LC        BOTH OLD, though LCF is almost SPL(a)
	DATA Tablc / 91.01,85.88,87.32,79.85,79.76,75.96,73.96,74.91,
     1     94.63,13*0.0,44.29,50.72 /
C	DATA Tablcf / 91.0,85.9,87.3,79.9,79.8,76.0,74.0,74.9,94.6,
C     1     13*0.0,44.3,50.7 /
C READ IN DATA FOR TABLE LC        [SPL(a) except FAR36 has infinity where I have 9999.0]
	DATA Tablcf / 91.0,85.9,87.3,79.9,79.8,76.0,74.0,74.9,94.6,
     1     13*9999.0,44.3,50.7 /


C READ IN DATA FOR TABLE M4      effectively [M(c)] with fill-in data for the undefined parts.
	DATA Tabm4 / 15*0.030103,9*0.029960 /

C READ IN DATA FOR TABLE L4      [SPL(c)]
	DATA Tabl4 / 52.,51.,49.,47.,46.,45.,43.,42.,41.,5*40.,38.,
     1     34.,32.,30.,2*29.,30.,31.,34.,37. /

c correction for L3 differing between FAR36 and Bennett, et al
C READ IN DATA FOR TABLES M3a, L3a and LCa (Pearsons & Bennett)
c  [M(b)]
	DATA Tabm3a / 0.043478,0.040570,2*0.036831,0.035336,
     1     2*0.033333,0.032051,0.030675,
     2     6*0.030103,7*0.029960,2*0.042285 /

c  [SPL(b)]
	DATA Tabl3a / 64.,60.,56.,53.,51.,48.,46.,44.,42.,
     1     5*40.,38.,34.,32.,30.,2*29.,30.,31.,37.,41. /

c OLD
	DATA Tablca / 91.01,85.88,87.32,79.85,79.76,75.96,73.96,74.91,
     1     94.63,13*100.0,44.29,50.72 /


	RETURN
	END
