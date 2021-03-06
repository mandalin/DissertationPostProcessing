%%
% This script reads the contents of a folder, and convolves them with the
% contents of a second folder. The output appends the two filenames
% together. 

clear all 
close all 
clc




%identify the IR folder
%IRFolder='/Volumes/AMPULINA/2016NovemberSims/Wall - 2016 - Nov - 25 /Specular_IR'; 
IRFolder='/Volumes/AMPULINA/2017/Wall/GroundLevel/SpecularIR/1 commit 66346a3a92d739d2ea3df131637dc61b8273eb49'; 
cd(IRFolder)
IRlisting=dir();

ContourOutFolder='/Volumes/AMPULINA/2017/Wall/GroundLevel/SpecularContour/1';
OneStepBack='/Volumes/AMPULINA/2017/Wall/GroundLevel/SpecularContour';
PLdBCalcFolder='/Users/mandalin/Desktop/Sort Me Now/DissertationPostProcessing';

%______________________________________

cd('/Users/mandalin/Desktop/Sort Me Now/Dissertation_PLdB_PostProcessing/Inputs')
truncate_at=500;
load('Channel_188.mat')
input = X_desired_fs/2;

%% %We need to do our own upsampling....
input_voltage_fs25600=Channel_188_Voltage;
input_pressure_fs25600=input_voltage_fs25600 / scaling_V_per_Pa;

input_pressure_fs25600x15 = interp(double(input_pressure_fs25600),15); 
input_pressure_fs25600x15div8 = decimate(input_pressure_fs25600x15,8);
input=input_pressure_fs25600x15div8/2; %this should be sampled at 48khz

%%
  c=clock;
  hr_start=c(4)
  min_start=c(5)
%identify the input folder
InputFolder='/Users/mandalin/Desktop/Sort Me Now/Dissertation_PLdB_PostProcessing/Inputs'; 
%cd(InputFolder)
%Inputlisting=dir();


%remove erronious entries to the list
%for(index=1:length(Inputlisting))
%    if(Inputlisting(1).name(1)=='.')
%        Inputlisting(1)=[];
%    else
%        break;
%    end
%end



%remove erronious entries to the list
for(index=1:length(IRlisting))
    if(IRlisting(1).name(1)=='.')
        IRlisting(1)=[];
    elseif (strcmp(IRlisting(1).name,'TheVolumeSettingsFolder'))
       IRlisting(1)=[];
    else
        break;
    end
end
cd(OneStepBack);



fs=48000;

%load one Input and one IR

inputnum=1;
%    cd(InputFolder)
%    fid = fopen(Inputlisting(inputnum).name);
%     C = textscan(fid,'%f%*[^\n]');
%     fclose(fid);
%     input=C{1};
%     clear C; 


Inputlisting(inputnum).name='Mic188';

%Creat text file for outputing positions and PLdB Values
% cd(ContourOutFolder)
%  OutputPLdB_File_fid=fopen('PLdB_Contour_Wall_2.txt','w')
%  OutputPmax_File_fid=fopen('Pmax_Contour_Wall_2.txt','w')
%  OutputNumRefs_File_fid=fopen('NumRefs_Contour_Wall_2.txt','w')
%   
%   fprintf(OutputPLdB_File_fid, '%s\n', IRlisting(1).name);
%   fprintf(OutputPLdB_File_fid, '%s\n', Inputlisting(inputnum).name);
%   fprintf(OutputPLdB_File_fid, '%s\t%s\t%s\t%s\t%s\n', 'irnum', 'xpos', 'ypos', 'zpos', 'PLdB');
%    
%   fprintf(OutputPmax_File_fid, '%s\n', IRlisting(1).name);
%   fprintf(OutputPmax_File_fid, '%s\n', Inputlisting(inputnum).name);
%   fprintf(OutputPmax_File_fid, '%s\t%s\t%s\t%s\t%s\n', 'irnum', 'xpos', 'ypos', 'zpos', 'PLdB');
%    
%   fprintf(OutputNumRefs_File_fid, '%s\n', IRlisting(1).name);
%   fprintf(OutputNumRefs_File_fid, '%s\n', Inputlisting(inputnum).name);
%   fprintf(OutputNumRefs_File_fid, '%s\t%s\t%s\t%s\t%s\n', 'irnum', 'xpos', 'ypos', 'zpos', 'PLdB');

last=length(IRlisting)

IRnum=1;

last=7; %this is how many microphones there are

for(IRnum=1:last)
    
    cd(IRFolder)
    fid = fopen(IRlisting(IRnum).name);
    IRlisting(IRnum).name
    C = textscan(fid,'%f%*[^\n]');
    fclose(fid);
    IR=C{1};
    clear C;
    cd(OneStepBack);

    %calculate PLdB
    cd(PLdBCalcFolder)
    
    FS=48000;
    Y=conv(IR(1:truncate_at),input);
    Y=[zeros(fs*3,1); Y]; 
    PLdBofY(IRnum)=PLdB(double(Y),FS);
    PmaxofY(IRnum)=max(Y);
    NumRefs(IRnum)=sum(IR);
    Ystored{IRnum}=Y;
    FSstored=FS;
    IRstored{IRnum}=IR(1:truncate_at);
     %parse_the_input_file_name
     [a,b,c,d,e,f,g,h,i,j,k,l,m,n] = strread(IRlisting(IRnum).name,'%s%s%s%s%s%s%s%s%s%s%s%s%s%s','delimiter','_');
  
  
     [tempx,trash]= strread(l{1},'%s%s','delimiter','i')
     [tempy,trash]= strread(m{1},'%s%s','delimiter','j');
     [tempz,trash,trash1]= strread(n{1},'%s%s%s','delimiter','k');
      
        
%      fprintf(OutputPLdB_File_fid, '%s\t%s\t%s\t%s\t%f\n',e{1} , tempx{1}, tempy{1}, tempz{1}, PLdBofY(IRnum));
%      fprintf(OutputPmax_File_fid, '%s\t%s\t%s\t%s\t%f\n',e{1} , tempx{1}, tempy{1}, tempz{1}, PmaxofY(IRnum));
%      fprintf(OutputNumRefs_File_fid, '%s\t%s\t%s\t%s\t%f\n',e{1} , tempx{1}, tempy{1}, tempz{1}, NumRefs(IRnum));
%             
      x{IRnum}= tempx{1};
      y{IRnum}= tempy{1};
      z{IRnum}= tempz{1};
      irnum{IRnum}=d{1};
end

  c=clock;
  hr_complete=c(4)
  min_complete=c(5)
   

%   fclose(OutputNumRefs_File_fid)
%   fclose(OutputPmax_File_fid)
%   fclose(OutputPLdB_File_fid)

  c=clock;
  hr_complete=c(4)
  min_complete=c(5)
   
   save;


hr_complete-hr_start
min_complete-min_start
%% Plot the Input Waveform
IRnum=1;

delta_t=1/FS;
time_input=([1:length(input)]-1)*delta_t;
plot(time_input,input)
%% Plot the IR
IRnum=1;

delta_t=1/FS;
time_IR=([1:length(IRstored{IRnum})]-1)*delta_t;
plot(time_IR,IRstored{IRnum})
%% Plot the Convolved Waveform
mic_num=1; 

delta_t=1/FS;
time=([1:length(Ystored{mic_num})]-1)*delta_t;
plot(time,Ystored{mic_num})


