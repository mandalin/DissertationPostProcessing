%%
% This script reads the contents of a folder, and convolves them with the
% contents of a second folder. The output appends the two filenames
% together. 

clear all 
close all 
clc

%identify the input folder
InputFolder='/Users/mandalin/Desktop/Dissertation_PLdB_PostProcessing/Inputs'; 
cd(InputFolder)
Inputlisting=dir();


%remove erronious entries to the list
for(index=1:length(Inputlisting))
    if(Inputlisting(1).name(1)=='.')
        Inputlisting(1)=[];
    else
        break;
    end
end

%identify the IR folder
IRFolder='/Volumes/WORKSPACE/InProgress'; 
cd(IRFolder)
IRlisting=dir();


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

fs=48000;

%load one Input and one IR

inputnum=3;
    cd(InputFolder)
    fid = fopen(Inputlisting(inputnum).name);
     C = textscan(fid,'%f%*[^\n]');
     fclose(fid);
     input=C{1};
     clear C; 
  
%Creat text file for outputing positions and PLdB Values
cd('/Users/mandalin/Desktop/Dissertation_PLdB_PostProcessing/PLdB Contours')
 OutputPLdB_File_fid=fopen('PLdB_Contour_Output3.txt','w')
 OutputPmax_File_fid=fopen('Pmax_Contour_Output3.txt','w')
 OutputNumRefs_File_fid=fopen('NumRefs_Contour_Output3.txt','w')
  
  fprintf(OutputPLdB_File_fid, '%s\n', IRlisting(1).name);
  fprintf(OutputPLdB_File_fid, '%s\n', Inputlisting(inputnum).name);
  fprintf(OutputPLdB_File_fid, '%s\t%s\t%s\t%s\t%s\n', 'irnum', 'xpos', 'ypos', 'zpos', 'PLdB');
  
  fprintf(OutputPmax_File_fid, '%s\n', IRlisting(1).name);
  fprintf(OutputPmax_File_fid, '%s\n', Inputlisting(inputnum).name);
  fprintf(OutputPmax_File_fid, '%s\t%s\t%s\t%s\t%s\n', 'irnum', 'xpos', 'ypos', 'zpos', 'PLdB');
  
 fprintf(OutputNumRefs_File_fid, '%s\n', IRlisting(1).name);
 fprintf(OutputNumRefs_File_fid, '%s\n', Inputlisting(inputnum).name);
 fprintf(OutputNumRefs_File_fid, '%s\t%s\t%s\t%s\t%s\n', 'irnum', 'xpos', 'ypos', 'zpos', 'PLdB');
 
last=length(IRlisting)
for(IRnum=1:last)
% IRnum=1;
    cd(IRFolder)
    fid = fopen(IRlisting(IRnum).name);
    C = textscan(fid,'%f%*[^\n]');
    fclose(fid);
    IR=C{1};
    clear C;


    %calculate PLdB
    cd(InputFolder)
    cd .. 
    
    FS=48000;
     Y=conv(IR,input);
     Y=[zeros(fs*3,1); Y]; 
     PLdBofY(IRnum)=PLdB(Y,FS);
     PmaxofY(IRnum)=max(Y);
    NumRefs(IRnum)=sum(IR);
     
     %parse_the_input_file_name
     [a,b,c,d,e,f,g,h,i,j,k,l,m,n] = strread(IRlisting(IRnum).name,'%s%s%s%s%s%s%s%s%s%s%s%s%s%s','delimiter','_');
  
  
      [tempx,trash]= strread(l{1},'%s%s','delimiter','i')
     [tempy,trash]= strread(m{1},'%s%s','delimiter','j');
      [tempz,trash,trash1]= strread(n{1},'%s%s%s','delimiter','k');
      
%        
     fprintf(OutputPLdB_File_fid, '%s\t%s\t%s\t%s\t%f\n',e{1} , tempx{1}, tempy{1}, tempz{1}, PLdBofY(IRnum));
     fprintf(OutputPmax_File_fid, '%s\t%s\t%s\t%s\t%f\n',e{1} , tempx{1}, tempy{1}, tempz{1}, PmaxofY(IRnum));
     fprintf(OutputNumRefs_File_fid, '%s\t%s\t%s\t%s\t%f\n',e{1} , tempx{1}, tempy{1}, tempz{1}, NumRefs(IRnum));
            
%      x{IRnum}= tempx{1};
%      y{IRnum}= tempy{1};
%      z{IRnum}= tempz{1};
%      irnum{IRnum}=d{1};
end
  fclose(OutputNumRefs_File_fid)

    save;



