	subroutine open_mat_file(sample_rate,ref_volt,equiv_db,filename$
     1 ,ierr)


c MATLAB data file version - BMS Aug 21 2002

c variables for Matlab
        integer ep, status
c Matlab wants everything in double precision so make a holding variable.
        double precision dptemp(65536)
        double precision dp_size
        common /Matcom/ ep, status, dptemp, dp_size

c Matlab functions
        integer engGetArray, mxCreateDoubleMatrix, mxGetPr
        integer mxCreateScalarDouble, mxCreateString
        integer engPutArray, engEvalString

        double precision dp_sample_rate
        real sample_rate
        integer ierr

        double precision dp_channel, dp_start_index, dp_cal_fac
        double precision dp_nexus
        integer Channel, Start_index, Cal_fac, Nexus, Mat_name
        integer One_Boom, Samprate, Lenth

	character*(*) filename$

	print *,' '

	return
	end
