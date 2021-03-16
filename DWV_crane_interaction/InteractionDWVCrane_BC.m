clear all
%% load data
%-- crane segmented data
%load StopInformationTable_new1
load StopInformationTable_agraegate
StopInformationTable2018=StopInformationTable_new_FuncUnt_agr;
%-- DWV data --------------------------------------------------
load ChaserData2018CorrectedNEwSabsampled
%-- DWV metadata ----------------------------------------------
load AllInfoChaser
%-- habitat (agricaltural units) ------------------------------
load filedsPerday_new
TypesOfCrops=[SumWinType; 'Feeding Station'];
%-- directory containing the crane GPS data
directory='../DataArchive';
% ---- loop over all the indeviduals in the directory ---------
FilesIn=struct2table(dir('../DataArchive'));
IndevidualsExistTemp=FilesIn.name(FilesIn.bytes>0);
for ind=1:length(IndevidualsExistTemp)
    if contains(IndevidualsExistTemp{ind},'mat')==0
        continue
    end
    Name=IndevidualsExistTemp{ind};
    IndevidualsExistNum(ind,1)=str2num(Name(1:end-4));
end
% --- Time correction -----------------------------------------
hToAddW=2/24; %Winter time (starting Oct 30)
hToAddS=3/24; %Winter time (until Oct 29)

%% Loop over all the dates in table
DistanceR=2000; %maximal distance checked for DWV-crane interaction
dates=unique(StopInformationTable2018.Date);

Cranes=[];
LocalLonAll=[];
LocalLatAll=[];
LoacalmldateAll=[];
ChaserNameAll1=[];
stayed=[];

for d=1:length(dates)
    Date=datenum(dates(d));
    IND=StopInformationTable2018.Date==dates(d);
    StopInfoDate=StopInformationTable2018(IND,:);
    interval=StopInfoDate.TimeInMax-StopInfoDate.TimeInMin;
    %--- make sure there are no stops with large intervals ------------
    INDREL= interval<=15;
    StopInfoDate=StopInfoDate(INDREL,:);
    %-- all cranes this date ---------------------------------------------
    tags=unique(StopInfoDate.indev);
    %-- DWV this date ---------------------------------------------
    INDDay=floor(AllChaserData.Date_Time)==Date;
    chasersDay=unique(AllChaserData.Tag(INDDay));
    
    for c=1:length(tags)
        %-- load GPS data ----------------------------------
        ind=find(IndevidualsExistNum==tags(c));
        load([directory,'\',char(IndevidualsExistTemp(ind))]);
        Relevantdata=~isnan(TagMAT(:,8))  & TagMAT(:,5)~=0 & floor(TagMAT(:,3))==datenum(dates(d));
        lon=TagMAT(Relevantdata,4);
        lat=TagMAT(Relevantdata,5);
        ele=TagMAT(Relevantdata,6);
        heading=TagMAT(Relevantdata,9);
        mldate=TagMAT(Relevantdata,3);
        speed=TagMAT(Relevantdata,8);
        %-- project
        [LocalLon, LocalLat]=TransWGS2NIGrid(lat,lon,ones(size(lat)));
        %-- convert to local time
        Loacalmldate=mldate;
        Wint=Loacalmldate>=datenum('29/10/2017 00:00','dd/mm/yyyy HH:MM');
        Summ=Loacalmldate<datenum('29/10/2017 00:00','dd/mm/yyyy HH:MM');
        Loacalmldate(Wint)=mldate(Wint)+hToAddW;
        Loacalmldate(Summ)=mldate(Summ)+hToAddS;
        
        %-- index of crane and date in StopInformationTable2018------------
        indTag=StopInfoDate.indev==tags(c);
        tagData=StopInfoDate(indTag,:);
        
        ARR=[];
        DEP=[];
        for tt=1:height(tagData)
            ARR=[ARR; find(mldate==datenum(tagData.TimeArrival_utc(tt)),1,'first')];
            DEP=[DEP; find(mldate==datenum(tagData.TimeDeparture_utc(tt)),1,'first')];
        end
        TimeIn=(tagData.TimeInMax+tagData.TimeInMin)/2;
        
        disp(['Working on tag ',num2str(tags(c)),' on ',datestr(dates(d))])
        %-- prepare variables ---------------------------------------------
        infoDayCrane=nan(length(mldate),5);
        Left=nan(length(mldate),1);
        TimeStayed=nan(length(mldate),1);
        
        %% Prepare an info vector for every GPS point of this day
        Filed_Last=[];
        TimeTotal=[];
        for i=1:length(ARR)
            % fill the infoDayCrane
            if tagData.functional_crop_number(i)==2 |tagData.functional_crop_number(i)==3
                isAllowed=0;
            else
                isAllowed=1;
            end
            
            if i==1 % first arrival
                infoDayCrane(1:ARR(i)-1,1)=0;
                infoDayCrane(ARR(i):DEP(i),1)=1;
                infoDayCrane(ARR(i):DEP(i),2)=tagData.index_field(i);
                infoDayCrane(ARR(i):DEP(i),3)=tagData.functional_crop_number(i);                
                infoDayCrane(ARR(i):DEP(i),4)=isAllowed;
                infoDayCrane(ARR(i):DEP(i),5)=i; 
               
                Left(ARR(i):DEP(i),1)=0;
                Left(DEP(i),1)=1;
                TimeStayed(DEP(i),1)=TimeIn(i);
            elseif i==height(tagData) % last arrival
                if ARR(i)-DEP(i-1)>1 % if there is a gap of flying between the points
                    infoDayCrane(DEP(i-1)+1:ARR(i)-1,1)=2;
                end
                
                infoDayCrane(DEP(i):end,1)=0;
                infoDayCrane(ARR(i):DEP(i),1)=1;
                infoDayCrane(ARR(i):DEP(i),2)=tagData.index_field(i);
                infoDayCrane(ARR(i):DEP(i),3)=tagData.functional_crop_number(i);
                infoDayCrane(ARR(i):DEP(i),4)=isAllowed;
                infoDayCrane(ARR(i):DEP(i),5)=i;

                Left(ARR(i):DEP(i),1)=0;
                Left(DEP(i),1)=1;
                TimeStayed(DEP(i),1)=TimeIn(i);
            else
                if ARR(i)-DEP(i-1)>1 % if there is a gap of flying between the points
                    infoDayCrane(DEP(i-1)+1:ARR(i)-1,1)=2;
                end
                infoDayCrane(ARR(i):DEP(i),1)=1;
                infoDayCrane(ARR(i):DEP(i),2)=tagData.index_field(i);
                infoDayCrane(ARR(i):DEP(i),3)=tagData.functional_crop_number(i);
                infoDayCrane(ARR(i):DEP(i),4)=isAllowed;
                infoDayCrane(ARR(i):DEP(i),5)=i;
                
                Left(ARR(i):DEP(i),1)=0;
                Left(DEP(i),1)=1;
                TimeStayed(DEP(i),1)=TimeIn(i);
            end
        end
       
        LocalLonAll=[LocalLonAll; LocalLon];
        LocalLatAll=[LocalLatAll; LocalLat];
        LoacalmldateAll=[LoacalmldateAll; Loacalmldate];
        %% Check if met with a DWV
        MetChaser=nan(length(mldate),3);
        TooFarChaser=nan(length(mldate),2);
        ChaserNameAll=cell(length(mldate),1);
        TooFarChaserName=cell(length(mldate),1);
        for cr=1:length(LocalLon)-1
            if infoDayCrane(cr,1)~=1 % if flying or not at field
                continue;
            elseif infoDayCrane(cr,4)==1  % if the field alowed
                continue;
            end
            % time of point
            TimePoint=Loacalmldate(cr:cr+1);
            PloC=0;
            % look if we have DWV data is a chaser in the area
            FieldCraneOn=infoDayCrane(cr,2);
            DateToday=floor(TimePoint(1));
            % find DWVs that visited the field this day
            kl=AllInfoChaser(find(datenum(AllInfoChaser.Date)==DateToday & AllInfoChaser.FieldNumber==FieldCraneOn),:);
            [uvals, Ind, uidx] = unique(kl.Chaser);
            output = [uvals, accumarray(uidx, 1)];
            chasersDay=output(:,1); % look for interaction only with the relevant DWVs
            %% ---look for cranes which left filed in chaser area when the DWV was far
            %-------- Choose the DWV that visited more maximal amount of time and it was at least 3 times
            ChaserOfArea=((output(:,2)==max(output(:,2)) & output(:,2)>2));
            %% ---look for chasing events or non chasing events
            for ch=1:length(chasersDay)
                INDDayCh=floor(AllChaserData.Date_Time)==Date & AllChaserData.Tag==chasersDay(ch);
                DateTime=AllChaserData.Date_Time(INDDayCh);
                [LonRecLoc, LatRecLoc]=TransWGS2NIGrid(AllChaserData.Lat(INDDayCh),AllChaserData.Lon(INDDayCh),ones(sum(INDDayCh),1)); % convert to local grid
                ChaserName=unique(AllChaserData.worker_Name(INDDayCh));
                ChaserNum=unique(AllChaserData.worker_Number(INDDayCh));
                % find relevant time
                ThirtySec=30/86400;
                indR=DateTime>=TimePoint(1)-ThirtySec & DateTime<TimePoint(2);
                if sum(indR)==0
                    continue;
                end
                % calculate the distance
                distTemp=((LatRecLoc(indR)-LocalLat(cr)).^2+(LonRecLoc(indR)-LocalLon(cr)).^2).^0.5;
                % ---- DID THE CRANE MET DWV?---------------------------
                RelevantDist=distTemp<=DistanceR;
                if sum(RelevantDist)>0 % if in some point there was a DWV near
                    MetChaser(cr,1)=nansum([MetChaser(cr,1),1]); % we want to know if met more than one DWV, if yes, write down the minimal distance
                    if MetChaser(cr,1)>1
                        diss=round(min(distTemp(RelevantDist)));
                        if MetChaser(cr,3)>diss
                            MetChaser(cr,2)=chasersDay(ch);
                            MetChaser(cr,3)=round(min(distTemp(RelevantDist)));
                            ChaserNameAll(cr,1)=ChaserName;
                        else % leave the info we had before
                            continue
                        end
                    else
                        MetChaser(cr,2)=chasersDay(ch);
                        MetChaser(cr,3)=round(min(distTemp(RelevantDist)));
                        ChaserNameAll(cr,1)=ChaserName;
                    end
                    PloC=PloC+1;
                end
                % ---- DID THE CRANE LEFT WHEN DWV WAS FAR?-------------
                %-- first check if that is the DWV that this is his area (may not be any of the chasers)
                NonChasingDist=1000; % the distance the chaser won't chase the crane from
                VisitedAfter=datenum(kl.Enter(kl.Chaser==chasersDay(ch)))>TimePoint(1);
                if ChaserOfArea(ch)==1  &  sum(VisitedAfter)>0 % if this is the relevat DWV check how far was it
                    % and he also revisited the field at least once after the time
                    %(so we know the DWV didn't leave home and other, non tagged DWV took over)
                    TooFarDist=distTemp>=NonChasingDist;
                    if sum(TooFarDist)==length(TooFarDist)
                        TooFarChaser(cr,1)=1;
                        TooFarChaser(cr,2)=chasersDay(ch);
                        TooFarChaserName(cr,1)=ChaserName;
                    end
                end                
         end
        end
        Cranes=[Cranes; [ones(length(LocalLon),1)*tags(c),infoDayCrane,Left,MetChaser,TooFarChaser,TimeStayed]];
        ChaserNameAll1=[ChaserNameAll1; [ChaserNameAll,TooFarChaserName]];
        
    end
    %-- fill in the table -----------------------------------------------
    tvec = datetime(LoacalmldateAll,'ConvertFrom','datenum');
    InterCrCha=table(tvec,LocalLonAll,LocalLatAll,Cranes(:,1),Cranes(:,2),Cranes(:,3),Cranes(:,4),Cranes(:,5),...
        Cranes(:,6),Cranes(:,7),Cranes(:,13),Cranes(:,8),Cranes(:,9),Cranes(:,10),ChaserNameAll1(:,1),Cranes(:,11),Cranes(:,12),ChaserNameAll1(:,2),...
        'VariableNames',{'DateTime','LocalLon','LocalLat','Tag','TypeOfPoint','FieldNim','Crop',...
        'IsAllowed','StopNumberInDay','NextPointLeft','TimeStayed','InteractWithChaser',...
        'ChaserTagNum','Distance','ChaserName','TooFarChaser','TooFarChaserTagNum','TooFarChaserName'});
end
save('CraneChaserIntrTempSensitivity','InterCrCha','TypesOfCrops')
