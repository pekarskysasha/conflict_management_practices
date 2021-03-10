clear all
%% === load data =========================================================
load('MigVSWintIsrael.mat') % night locations
load ForKernel_CorrecterFeb2021
DateStart=datenum({'20/10/17'},'dd/mm/yy');
DateEnd=datenum({'31/01/18'},'dd/mm/yy');
NightLocationsTNewR=NightLocationsTNew(((datenum(NightLocationsTNew.Date)>=DateStart(1) &...
    datenum(NightLocationsTNew.Date)<=DateEnd(1)+1)),:);
RoostsNewR=RoostsNew(RoostsNew.year==2017,:);
Indeviduals=unique(NightLocationsTNewR.Indevidual);
directory='../DataArchive';
%-- Roost polygons ---------------------------
Water = kml2struct('../../LandCover\Agmaon\AgmonWater.kml');
HulaReserve=kml2struct('../../LandCover\Agmaon\HulaReserve.kml');
HatzafaReserve=kml2struct('../../LandCover\Agmaon\HatzafaReserve.kml');
MaagarEynan=kml2struct('../../LandCover\Agmaon\MaagarEynan.kml');
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');
%-- read food amount --------------------------------
Dir=['../FeedingStationR\'];
FoodAvaliability=readtable([Dir, 'FoodAlsoBefore.csv']);
%% ===  perepare variables =================================================
InFeedingS=[];
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
        floor(datenum(NightLocationsTNewR.Date))>=datenum('20/10/2017','dd/mm/yyyy')&...
        floor(datenum(NightLocationsTNewR.Date))<=datenum('29/02/2018','dd/mm/yyyy'),:);
    dates=floor(datenum(NightsIndev.Date));
    RoostsNewIndev=RoostsNew(RoostsNew.indevidual==indev  & RoostsNew.year==2017,:);
    %--- remove migratory tags,tags with no data and DF-independent tags--------------------------------------------
    if strcmp(RoostsNewIndev.Status(1),'migrating')==1 | indev==161152 | isempty(RoostsNewIndev)|...
            indev==170831 | indev==17084 | indev==170951 | indev==170991
        continue
    end
    %------ convert to local time--------------------------
    zd=(timezone(lon))*(-1);
    mldateTime=datetime(mldate+zd/24,'ConvertFrom','datenum','format','y-MM-dd HH:mm:ss');
    for r=1:length(dates)-1
        Food_amount_kg=FoodAvaliability.Corn_amount(datenum(FoodAvaliability.Date)==dates(r));
        if isempty(Food_amount_kg)
            Food_amount_kg=0; 
        end
        disp(['Working on individial ',num2str(indev),' on ',datestr(dates(r))])
         % roost locations--------------------------------------------
        meanLat=NightsIndev.meanLat(r:r+1);
        meanLon=NightsIndev.meanLon(r:r+1);
        %-- check roost data (is day after day and in Hula) -----------------------------------------------
        if strcmp(NightsIndev.NightRoost(r),'HulaValley')==0 | ....
                strcmp(NightsIndev.NightRoost(r+1),'HulaValley')==0
            continue
        elseif dates(r+1)-dates(r)~=1
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
        
        %--tiem in the feeding station
        day_Lon=Lon1(left:returned);
        day_Lat=Lat1(left:returned);
        inDaled=inpolygon(day_Lat,day_Lon,Daled.Lat,Daled.Lon);
        Date=datetime(dates(r),'ConvertFrom','datenum','format','dd-MM-y');
        tag=Indeviduals(i);
        Percent_presence=sum(inDaled)/length(day_Lon);
        InFeedingS=[InFeedingS; table(tag,Date,Food_amount_kg,Percent_presence)];
    end
end

writetable(InFeedingS,'TablePresenceDaledOnlyDaledTagsIncludeZero.csv')