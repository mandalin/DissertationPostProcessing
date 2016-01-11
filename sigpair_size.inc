c all the large parameters (except for the one in fftscs_replacement)
c				BMS Aug 4 2005
	integer isize_sigpair
	parameter (isize_sigpair = 524288)
c We have room for 2**19 data points
	integer exponent
        parameter (exponent = 19)
        integer power2
        parameter (power2 = 524288)
        integer halfp2a1
        parameter (halfp2a1 = 262144)
c Matlab array size
	integer max_size
        parameter (max_size=524288)


