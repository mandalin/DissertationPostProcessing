function [PLdBvalue,varargout]=PLdB(varargin)
% PLdB(waveform,Fs) Compute PLdB based on Stevens Mark VII
%
% PLdB(waveform,Fs) where waveform is the waveform in question and Fs is
% the sampling frequency.
% PLdB(waveform,Fs,S,N) where S stands for symetry, (0 for symetric, 1 for
% non-symetric. The default is S=0 whichs is the symetric case.
% N stands for the number of points to use on the hanning
% window. If N is not specified, the default is 200.
inputs=nargin;

%%
switch nargin
  case 1
    disp('No sampling frequency specified')
    return
  case 2
    N=200; % points used in the hanning window
    S=0; % symetric case
    waveform=varargin{1};
    Fs=varargin{2};
  case 3
    waveform=varargin{1};
    Fs=varargin{2};
    S=varargin{3};
    N=200;
  case 4
    waveform=varargin{1};
    Fs=varargin{2};
    S=varargin{3};
    N=varargin{4};
  otherwise disp('something is wrong. Too many input arguments')
end

[rows,cols]=size(waveform);
if rows==1
  waveform=waveform';
end

%% Save current path
%curdir=cd;

%% Change to the directory where the weightsv2.exe program is
%cd('C:\research')
%%
fid=fopen('options.pl','wt');
count=fprintf(fid,'%1s\n','9');
count=fprintf(fid,'%1s\n',num2str(S));
count=fprintf(fid,'%1s\n','N');
count=fprintf(fid,'%1s\n','longmeas1.pl');
count=fprintf(fid,'%7s\n','1 93.98');
count=fprintf(fid,'%13s\n','groundmeas1.pl');
count=fprintf(fid,'%5s\n',num2str(Fs));
count=fprintf(fid,'%1s\n','0');
count=fprintf(fid,'%1s\n','1');
count=fprintf(fid,'%1s\n','0');
count=fprintf(fid,'%1s\n','1');
count=fprintf(fid,'%1s\n','1');
count=fprintf(fid,'%1s\n','1');
count=fprintf(fid,'%1s\n','0');
count=fprintf(fid,'%1s\n',num2str(N));
count=fprintf(fid,'%1s\n','0');
count=fprintf(fid,'%1s\n','0');
fclose(fid);


%%
if isstruct(dir('groundmeas1.pl'))
  delete('groundmeas1.pl');
end
save(['groundmeas1.pl'], 'waveform','-ascii')
% send the ground measurement to Brenda's Code, along with the
% instructions contained in options.pl
if isstruct(dir('longmeas.pl'))
  delete('longmeas.pl');
end 
dos('"/Users/mandalin/Desktop/Sort Me Now/DissertationPostProcessing/weightsv2" < options.pl');
%%
fid=fopen('longmeas1.pl'); % longmeas.pl =output data file from Sullivan
% code.  %Lind suffix with 1
junk=textscan(fid,'%*[^\n]',9);
Pl=textscan(fid,'%*s%*s%*s%*s%f32*[^\n]',1)
if S==1
  junk=textscan(fid,'%*[^\n]',10);
  Front=double(cell2mat(textscan(fid,'%*s%*s%*s%*s%f32*[^\n]',1)));
  junk=textscan(fid,'%*[^\n]',10);
  Back=double(cell2mat(textscan(fid,'%*s%*s%*s%*s%f32*[^\n]',1)));
  varargout(1)={Front};
  varargout(2)={Back}; % tack on the front and back PL values 
end
PLdBvalue=double(cell2mat(Pl));
fclose(fid);
%delete('subf7_shape.txt'); %Lind commented

%% change back to the working directory
%cd(curdir)