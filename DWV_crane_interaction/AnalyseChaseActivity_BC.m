clear all
%% ---load data----------------------------------------------------
load ChaserData2018CorrectedNEwSabsampled
load filedsPerday
load AllInfoChaser
dates=numberOfDays2018(numberOfDays2018>datenum('15/10/2017','dd/mm/yyyy') & numberOfDays2018<datenum('21/02/2018','dd/mm/yyyy'));
%%------------
HowFreqAll=[];
InfoPerFieldAll=[];
for d=1:length(dates)
    Date=dates(d);
    Fieldsdate=FiledDay(numberOfDays2018==Date,:);
    FieldsdateT = struct2table(Fieldsdate);
    %---- all chasers of this day
    INDDay=floor(AllChaserData.Date_Time)==Date;
    chasersDay=unique(AllChaserData.Tag(INDDay));
    for ch=1:length(chasersDay)
       %-- chaser data
       INDDayCh=floor(AllChaserData.Date_Time)==Date & AllChaserData.Tag==chasersDay(ch);
       DateTime=AllChaserData.Date_Time(INDDayCh);
       [LonRecLoc, LatRecLoc]=TransWGS2NIGrid(AllChaserData.Lat(INDDayCh),AllChaserData.Lon(INDDayCh),ones(sum(INDDayCh),1)); % convert to local grid
      
       %-- chaser filed info---------------------------------------------
       filedInfo=AllInfoChaser(find(datenum(AllInfoChaser.Date)==Date & AllInfoChaser.Chaser==chasersDay(ch)),:); 
       [uvals, Ind, uidx] = unique(filedInfo.FieldNumber);
       output = [uvals, accumarray(uidx, 1)];
       %-take only fileds which were visited at least 3 times
       Fdch=output(output(:,2)>2,1);
       if isempty(Fdch)
           continue;
       end
       %-take from the info table only the relevant fields
       RelevanInfo=[];
       Crop=[];
       for f=1:length(Fdch)
           RelevanInfo=[RelevanInfo; filedInfo(filedInfo.FieldNumber==Fdch(f),:)];   
           Crop=[Crop; FieldsdateT.CropCode(FieldsdateT.ID==Fdch(f))];
       end
       TimeWorked=(max(datenum(RelevanInfo.Exit))-min(datenum(RelevanInfo.Enter)))*86400/60; % time worked in minures
       if TimeWorked/60<3
           continue;
       end
       [uvals, Ind, uidx] = unique(RelevanInfo.FieldNumber);
       FieldsVisited = [uvals, accumarray(uidx, 1)];
       FieldsVisited=[FieldsVisited,Crop];
       %%--- colect stats for this day and chaser
       %-(1) how fequently the chaser come back to the fields
       HowFreq=[];
       InfoPerField=[];
       for f=1:length(FieldsVisited(:,1))
           FildData=RelevanInfo(RelevanInfo.FieldNumber==FieldsVisited(f,1),:);
           Gaps=(datenum(FildData.Enter(2:end))-datenum(FildData.Exit(1:end-1)))*86400/60;
           Crops=ones(FieldsVisited(f,2)-1,1)*FieldsVisited(f,3);
           HowFreq=[HowFreq; [FildData(1:end-1,1:3), table(Gaps),table(Crops)]];
           meanGap=mean(Gaps);
           stdGaps=std(Gaps);
           crop=FieldsVisited(f,3);
           meanTime=mean(FildData.TimeIn);
           timesVisited=height(FildData);
           InfoPerField=[InfoPerField; [FildData(1,1:3)], table(crop),table(meanGap),table(stdGaps),table(meanTime),table(timesVisited)];
       end
       HowFreqAll=[HowFreqAll; HowFreq];
       InfoPerFieldAll=[InfoPerFieldAll; InfoPerField];
    end    
end

%% Add period
HowFreqAll.Period=ones(height(HowFreqAll),1);
HowFreqAll.Period(datenum(HowFreqAll.Date)<datenum('05/12/2017','dd/mm/yyyy'))=0;
HowFreqAll.Period(datenum(HowFreqAll.Date)>datenum('20/12/2017','dd/mm/yyyy'))=2;

InfoPerFieldAll.Period=ones(height(InfoPerFieldAll),1);
InfoPerFieldAll.Period(datenum(InfoPerFieldAll.Date)<datenum('05/12/2017','dd/mm/yyyy'))=0;
InfoPerFieldAll.Period(datenum(InfoPerFieldAll.Date)>datenum('20/12/2017','dd/mm/yyyy'))=2;
%% clean data to make csv
InfoPerFieldAllF=InfoPerFieldAll(InfoPerFieldAll.crop==5 |InfoPerFieldAll.crop==8 | InfoPerFieldAll.crop==11,:);
InfoPerFieldAllF=InfoPerFieldAllF(InfoPerFieldAllF.timesVisited>4,:);
writetable(InfoPerFieldAllF,'InfoPerFieldAll.csv')