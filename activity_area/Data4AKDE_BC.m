clear all
%% === load data =========================================================
load('MigVSWintIsrael.mat') % night locations
DateStart=datenum({'20/10/17'},'dd/mm/yy');
DateEnd=datenum({'31/01/18'},'dd/mm/yy');
NightLocationsTNewR=NightLocationsTNew(((datenum(NightLocationsTNew.Date)>=DateStart(1) &...
    datenum(NightLocationsTNew.Date)<=DateEnd(1)+1)),:);
RoostsNewR=RoostsNew(RoostsNew.year==2017,:);
Indeviduals=unique(NightLocationsTNewR.Indevidual);
directory='../DataArchive';
%--Roost polygons
Water = kml2struct('../../LandCover\Agmaon\AgmonWater.kml');
HulaReserve=kml2struct('../../LandCover\Agmaon\HulaReserve.kml');
HatzafaReserve=kml2struct('../../LandCover\Agmaon\HatzafaReserve.kml');
MaagarEynan=kml2struct('../../LandCover\Agmaon\MaagarEynan.kml');
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');
%% ===  perepare variables =================================================
data=[];
Tag_DateAll=[];
inOutBothtags=[];
Tag_SplitAll=[];
Stats=[];
Info4TagAll=[];

MaxSpeed=1.1; % 4 km/hour treshold above which definetly flight
%% === load GPS data =========================================================
FilesIn=struct2table(dir('../DataArchive'));
IndevidualsExistTemp=FilesIn.name(FilesIn.bytes>0);
for ind=1:length(IndevidualsExistTemp)-1
    Name=IndevidualsExistTemp{ind};
    IndevidualsExistNum(ind,1)=str2num(Name(1:end-4));
end
%% === Loop for all the days and individuals
for i=1:length(Indeviduals)
    indev=Indeviduals(i);
    ind=find(IndevidualsExistNum==Indeviduals(i));
    load([directory,'\',char(IndevidualsExistTemp(ind))]);
    lon=TagMAT(:,4);
    lat=TagMAT(:,5);
    mldate=TagMAT(:,3);
    speed=TagMAT(:,8);  
    %--- indevidual's night roosts and wintering grounds ------
    NightsIndev=NightLocationsTNewR(NightLocationsTNewR.Indevidual==indev &...
        floor(datenum(NightLocationsTNewR.Date))>=datenum('15/10/2017','dd/mm/yyyy')&...
        floor(datenum(NightLocationsTNewR.Date))<=datenum('31/01/2018','dd/mm/yyyy'),:);
    dates=floor(datenum(NightsIndev.Date));
    RoostsNewIndev=RoostsNew(RoostsNew.indevidual==indev  & RoostsNew.year==2017,:);
    %--- remove migratory tags or tags with no data --------------------------------------------
    if strcmp(RoostsNewIndev.Status(1),'migrating')==1 | indev==161152 | isempty(RoostsNewIndev)
        continue
    end
    %------ convert to local time--------------------------
    zd=(timezone(lon))*(-1);
    mldateTime=datetime(mldate+zd/24,'ConvertFrom','datenum','format','y-MM-dd HH:mm:ss');
    for r=1:length(dates)-1
        disp(['Working on individial ',num2str(indev),' on ',datestr(dates(r))])
        % to collect info regarding dates droped---------------------
        insDay=find(AllDaysTag==dates(r));
        Info4Tag(insDay,9)=0;
        % roost locations--------------------------------------------
        meanLat=NightsIndev.meanLat(r:r+1);
        meanLon=NightsIndev.meanLon(r:r+1);
        %-- check roost data (is day after day and in Hula) -----------------------------------------------
        if strcmp(NightsIndev.NightRoost(r),'HulaValley')==0 | ....
                strcmp(NightsIndev.NightRoost(r+1),'HulaValley')==0
            Info4Tag(insDay,3)=1;
            continue
        elseif dates(r+1)-dates(r)~=1
            Info4Tag(insDay,4)=1;
            continue
        end
        % get data for the indevidual and day----------------------------
        IND=floor(datenum(mldateTime))==datenum(dates(r));
        localmldate1=datenum(mldateTime(IND)); 
        Lon1=lon(IND);
        Lat1=lat(IND);
        speed1=speed(IND);
        mldateTime1=mldateTime(IND);
        %------- we want only out of roost data-------------------------
        OutOfRoost=ones(length(Lat1),1);
        % sunset and sunrise
        [SunRiseSet]=suncycle(Lat1(1) ,Lon1(1) ,localmldate1(1)); %in order to get time to add to the date divide in 24
        SunRiseSet2use=SunRiseSet/24; %sunrise and sunset in UTC
        SunRiseSet2use(1)=SunRiseSet2use(1)-1.5/24+floor(dates(r))+(timezone(Lon1(1)))*(-1)/24;
        SunRiseSet2use(2)=SunRiseSet2use(2)+1/24+floor(dates(r))+(timezone(Lon1(1)))*(-1)/24;
        
        DistNightBefore=distance(Lat1,Lon1,meanLat(1),meanLon(1),wgs84Ellipsoid);
        DistNightAfter=distance(Lat1,Lon1,meanLat(2),meanLon(2),wgs84Ellipsoid);
        
        left=[]; returned=[];
       inRoost=inpolygon(Lat1,Lon1,Water.Lat,Water.Lon)|...
                inpolygon(Lat1,Lon1,HulaReserve.Lat,HulaReserve.Lon) |...
                inpolygon(Lat1,Lon1,MaagarEynan.Lat,MaagarEynan.Lon) |...
                inpolygon(Lat1,Lon1,HatzafaReserve.Lat,HatzafaReserve.Lon);
            
        % if roosting in in the feeeding station start/end day just based on time
        inDaled=inpolygon(meanLat,meanLon,Daled.Lat,Daled.Lon);
        if inDaled(1)==1
            left=find(localmldate1>=SunRiseSet2use(1),1,'first');
        else
             left=find(localmldate1>=SunRiseSet2use(1) & ...
                DistNightBefore>200 & ~inRoost,1,'first');
        end
        if inDaled(2)==1
            returned=find(localmldate1>SunRiseSet2use(2),1,'first');
        else
            returned=find(DistNightAfter>200 & ~inRoost,1,'last');
        end
        
        if ~isempty(left)
            OutOfRoost(left:end)=1;
        else
            OutOfRoost(1:end)=0;
            disp(['tag ',num2str(indev),' on ',datestr(dates(r)), ' did not leave roost'])
        end
        if ~isempty(returned)
            OutOfRoost(returned:end)=0;
        else
            disp(['tag ',num2str(indev),' on ',datestr(dates(r)), ' did not return to roost'])
        end
        
        if sum(OutOfRoost)==0 % if didn't leave roost    
            continue
        end
        %%--- check sampling resolution--------------------------------
        % if the resolution very high, subsample
        Interval=round(median(minutes(diff(mldateTime1(left:returned)))));
        %-- if sampling resolution is less than 1 point per 30 minutes
        if Interval >30
            continue
        end
        s=left+1;
        IndF=left;
        maxint=0;
        minint=60;
        while s<returned
            int=minutes(mldateTime1(s)-mldateTime1(IndF(end)));
            if int<14
                s=s+1;
            else
                IndF=[IndF; s];
                maxint=max(maxint,int);
                minint=min(minint,int);
                s=s+1;
            end
        end
        %############################################################
        % add the last point of the day if missing
        if IndF<returned
            IndF=[IndF; returned];
        end
        %############################################################
        medint=round(median(minutes(diff(mldateTime1(IndF)))));
        %-- create the identifier of the indevidual + period
        if dates(r)<datenum('05/12/2017','dd/mm/yyyy')  
            for st=1:length(Lon1)
                Tag_Date(st,1)=cellstr([num2str(indev),'_','Before']);
                Tag_Split(st,1)=cellstr(['Before']);
            end
            BA=0;
            
        elseif dates(r)>=datenum('05/12/2017','dd/mm/yyyy') & dates(r)<datenum('21/12/2017','dd/mm/yyyy')            
            for st=1:length(Lon1)
                Tag_Date(st,1)=cellstr([num2str(indev),'_','Middle']);
                Tag_Split(st,1)=cellstr(['Middle']);
            end
            BA=1;
            
        else  
            for st=1:length(Lon1)
                Tag_Date(st,1)=cellstr([num2str(indev),'_','After']);
                Tag_Split(st,1)=cellstr(['After']);
            end
            BA=2;
            
        end
        % do we want this day
        if length(Lon1(IndF))<10 || round(median(diff(localmldate1(IndF))*86400/60))>31
            continue
        end
       
        Stats=[Stats; [indev,dates(r),maxint,minint,medint,(returned-left+1),round(median(diff(localmldate1(IndF))*86400/60)),length(Lon1(IndF))]];
        data=[data; [ones(length(Lon1(IndF)),1)*indev,localmldate1(IndF),...
            Lon1(IndF),Lat1(IndF),...
            ones(length(Lon1(IndF)),1)*BA]];
        Tag_DateAll=[Tag_DateAll; Tag_Date(IndF)];
        Tag_SplitAll=[Tag_SplitAll; Tag_Split(IndF)];
        clear Tag_Date Tag_Split
    end
end
% === prepare the table =========================================================
DD=datetime(data(:,2),'ConvertFrom','datenum','format','dd-MM-y HH:mm:ss');
DD1=datetime(data(:,2),'ConvertFrom','datenum','format','dd-MM-y');
T = table(data(:,1),Tag_DateAll,Tag_SplitAll,DD,DD1,data(:,3),data(:,4),data(:,5),...
    'VariableNames',{'Tag' 'ID','Split','Date_Time','Date','Lon','Lat','BeforeAfter'});
%% === calculate the stats for the data =========================================
StatsT=array2table(Stats,....
    'VariableNames',{'Tag' 'Date','subsumpled_max_int','subsumpled_min_int','subsumpled_med_int','number_of_points','subsampled_int','subsampled_nummber_point'});
DD1=datetime(Stats(:,2),'ConvertFrom','datenum','format','dd-MM-y');
StatsT.Date=DD1;
%--- check how many indeviduals and how many points per indevidual
PeriodStats=[];
for i=0:2
    Perperiod=T(T.BeforeAfter==i,:);
    [uvals, Ind, uidx] = unique(Perperiod.Tag);
    output = [uvals, accumarray(uidx, 1)];
    NumPoint=[];
    for ii=1:length(uvals)
        NumPoint=[NumPoint; length(unique(floor(datenum(Perperiod.Date(Perperiod.Tag==uvals(ii))))))];
    end
    PeriodStats=[PeriodStats; [output,ones(length(output),1)*i,NumPoint]];
end
PeriodStats1=PeriodStats(PeriodStats(:,3)~=1,:);
PeriodStatsT=array2table(PeriodStats1,....
    'VariableNames',{'Tag','number_of_points','Period','NumberOfDays'});

writetable(PeriodStatsT,'PeriodStats_forKernel1.csv')
%% === save the data =========================================
save('ForKernel_CorrecterFeb2021','T','Stats','PeriodStats')
%% === create CSVs for R =========================================
%--Write csv for home range kernel in R (Before and after, 18 tags)
%-- take only indeviduals with at least 10 days in both periods
temp=PeriodStatsT(PeriodStatsT.NumberOfDays>9,:);
 [uvals, Ind, uidx] = unique(temp.Tag);
 output = [uvals, accumarray(uidx, 1)];
 tags2Work=output(output(:,2)==2,:);
 T1=[];
for i=1:length(tags2Work)
    temp=T(T.Tag==tags2Work(i)& (T.BeforeAfter==0|T.BeforeAfter==2),:);
    T1=[T1; temp];
end
writetable(T1,'KernelDataInBeforeMiddleAfterFinal_new1.csv')

%--Write csv for home range kernel overlap with feeding station in R (Only after, 21 tags)
%---- take all indevidual to determine if they are DF-dependent or DF-independent 
temp=PeriodStatsT(PeriodStatsT.Period==2,:);
 [uvals, Ind, uidx] = unique(temp.Tag);
 output = [uvals, accumarray(uidx, 1)];
 tags2Work=output;
 T2=[];
for i=1:length(tags2Work)
    temp=T(T.Tag==tags2Work(i)& T.BeforeAfter==2,:);
    T2=[T2; temp];
end
writetable(T2,'KernelData4Overlap.csv')
%% === plot all to check per crane =========================================
temp=PeriodStatsT(PeriodStatsT.NumberOfDays>9,:);
 [uvals, Ind, uidx] = unique(temp.Tag);
 output = [uvals, accumarray(uidx, 1)];
 tags2Work=output(output(:,2)==2,:);
 fig1=figure;
 sgtitle('Before')
 fig2=figure;
 sgtitle('After')
for i=1:length(tags2Work)
    data=T(T.Tag==tags2Work(i),:);
    
    figure(fig1)
    subplot(5,4,i)
    plot(data.Lon(data.BeforeAfter==0),data.Lat(data.BeforeAfter==0),'.-y','MarkerSize',8)
    plot_google_map('MapType','satellite')
    title(num2str(tags2Work(i)))
    
    figure(fig2)
    subplot(5,4,i)
    plot(data.Lon(data.BeforeAfter==2),data.Lat(data.BeforeAfter==2),'.-y','MarkerSize',8)
    plot_google_map('MapType','satellite')
    title(num2str(tags2Work(i)))
end
