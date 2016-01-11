	subroutine open_sig(file$,iunit,iflag,ierr)

c changed the OPEN statement and removed the defaults for SGI version
c				BMS Oct 10 2000
c
c THIS VERSION WORKS FOR any use if the correct INCLUDE file exists;
c Routine to OPEN a signal file -- binary time history data for
c the Sonic Boom study.  
c	Close file before opening it - Aug 7 1991

c INPUT  - file$ = file name of signal file
c          iunit = logical unit to be used
c          iflag = 0 means open old file and close it 
c          iflag = 1 means open old file and leave file open
c          iflag = 2 means open new file and leave file open
c          iflag = 3 means open old CAL file and leave file open
c          iflag = 4 means open new CAL file and leave file open
c          iflag = 5 means open old A/D file and leave file open
c          iflag = 6 means open new A/D file and leave file open

c OUTPUT - ierr = error code

	character*(*) file$
	character*3 status$(0:6)
	character*4 deffile$(0:6)

c include the data storage directory for the signal files
c	character*5 direct$
c	data direct$ /'psig:'/

	data status$ /'old','old','new','old','new','old','new'/
	data deffile$ /3*'.dat',2*'.cal',2*'.a2d'/

	ierr = 0
	close (unit=iunit)
	open (unit=iunit,
     $    file=file$,
     $    status=status$(iflag),form='unformatted',
     $	  err=8000)
c     $	  defaultfile=deffile$(iflag),err=8000)
c     $    file=direct$//file$,

	if (iflag .eq. 0) close (iunit)

	return

c Could not open the file
 8000	ierr = 1
	return

	end
