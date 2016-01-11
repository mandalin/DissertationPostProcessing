c Common block for signal data storage for use with PLAYBOOM and WRITE_BOOM.
c Use an include file for easy alteration.  - Bigger version - April 29 1993

c SIGUNIT is the logical unit used for the signal file
c ISTANDARD = integer array to hold the CSD standard signal
c ISIGNAL = integer array to hold the test signal
c SIGNAL = real array to hold the data as read in from the storage
c			file (being stored as real numbers)

c	parameter (isize_sigpair = 65536)
	include 'sigpair_size.inc'
	integer sigunit
	integer*2 istandard(isize_sigpair),
     1			isignal(isize_sigpair)
	real signal(isize_sigpair)
	common /sigpair/ signal,istandard,isignal,sigunit

