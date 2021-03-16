%% ======= load data =======================================
clear all
load filedsPerday_new
load MigVSWintIsrael1
DateStart=datenum({'14/10/17'},'dd/mm/yy');
DateEnd=datenum({'29/02/18'},'dd/mm/yy');
NightLocationsTNewR=NightLocationsTNew(((datenum(NightLocationsTNew.Date)>=DateStart(1) &...
    datenum(NightLocationsTNew.Date)<=DateEnd(1)+1)),:);
RoostsNewR=RoostsNew(RoostsNew.year==2017,:);
Indeviduals=unique(NightLocationsTNewR.Indevidual);
directory='../DataArchive';

%-- Roost polygons -------------------------------------------
Water = kml2struct('../../LandCover\Agmaon\AgmonWater.kml');
HulaReserve=kml2struct('../../LandCover\Agmaon\HulaReserve.kml');
HatzafaReserve=kml2struct('../../LandCover\Agmaon\HatzafaReserve.kml');
MaagarEynan=kml2struct('../../LandCover\Agmaon\MaagarEynan.kml');
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');

%-- speed threshold -------------------------------------------
MaxSpeed=3; % 4 km/hour treshold above which definetly fligh
MaxSpeedCalc=0.80; % 90 percintile of walking (calculated) speeds

% ---- loop over all the indeviduals we have data on ----------
FilesIn=struct2table(dir('../DataArchive'));
IndevidualsExistTemp=FilesIn.name(FilesIn.bytes>0);
for ind=1:length(IndevidualsExistTemp)
    if contains(IndevidualsExistTemp{ind},'mat')==0
        continue
    end
    Name=IndevidualsExistTemp{ind};
    IndevidualsExistNum(ind,1)=str2num(Name(1:end-4));
end

%% Breaking data for Hula before and after
Stats=[];
StopInformationTable_new=[];
for i=4:length(Indeviduals)
    indev=Indeviduals(i);
    ind=find(IndevidualsExistNum==Indeviduals(i));
    load([directory,'\',char(IndevidualsExistTemp(ind))]);    
    Relevantdata=~isnan(TagMAT(:,8))  & TagMAT(:,5)~=0;
    lon=TagMAT(Relevantdata,4);
    lat=TagMAT(Relevantdata,5);
    ele=TagMAT(Relevantdata,6);
    heading=TagMAT(Relevantdata,9);
    mldate=TagMAT(Relevantdata,3);
    speed=TagMAT(Relevantdata,8); 
    %--- indevidual indevidual's night roosts and wintering grounds ------
    NightsIndev=NightLocationsTNewR(NightLocationsTNewR.Indevidual==indev &...
        floor(datenum(NightLocationsTNewR.Date))>=datenum('20/10/2017','dd/mm/yyyy')&...
        floor(datenum(NightLocationsTNewR.Date))<=datenum('29/02/2018','dd/mm/yyyy'),:);
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
        % get data
        IND=floor(datenum(mldateTime))==datenum(dates(r));
        localmldate1=datenum(mldateTime(IND));
        Lon1=lon(IND);
        Lat1=lat(IND);
        mldate1=mldate(IND);
        heading1=heading(IND);
        ele1=ele(IND);
        speed1=speed(IND);
        mldateTime1=mldateTime(IND);
        % clean duplicates or bad data
        aa=find(diff(mldate1)==0);
        ind2Delete=[];
        for a=1:length(aa)
           indD=find(heading1(aa(a):aa(a)+1)==0 & ele1(aa(a):aa(a)+1)==0);
           if indD==1
            ind2Delete=[ind2Delete; aa(a)];  
           elseif indD==2
             ind2Delete=[ind2Delete; aa(a)+1];    
           else
             ind2Delete=[ind2Delete; aa(a)];  
           end
        end
        localmldate1(ind2Delete)=[];
        Lon1(ind2Delete)=[];
        Lat1(ind2Delete)=[];
        mldate1(ind2Delete)=[];
        speed1(ind2Delete)=[];
        mldateTime1(ind2Delete)=[];
        % project to local grid
        [localLon1, localLat1]=TransWGS2NIGrid(Lat1,Lon1,ones(size(Lat1)));
        %------- we want only out of roost data-------------------------
        OutOfRoost=ones(length(Lat1),1);
        % sunset and sunrise
        [SunRiseSet]=suncycle(Lat1(1) ,Lon1(1) ,localmldate1(1)); %in order to get time to add to the date divide in 24
        SunRiseSet2use=SunRiseSet/24; %sunrise and sunset in UTC
        % convert to local time and add time before/after
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
        
        if sum(OutOfRoost)==0 | isempty(returned) | returned<left % if didn't leave roost
            continue
        end
        %%--- check sampling resolution--------------------------------
        medint=round(median(minutes(diff(mldateTime1(left:returned)))));
        minint=round(min(minutes(diff(mldateTime1(left:returned)))));
        maxint=round(max(minutes(diff(mldateTime1(left:returned)))));
        %-- if sampling resolution of 3 or 5 minutes
        if medint >6
            continue
        end
        %%--- save stats of the day --------------------------------
        Stats=[Stats; [indev,dates(r),maxint,minint,medint,(returned-left+1)]];
        %% ======= look for stops ===============================================
        ii=left;
        count=0;
        ArriveLeft=[];
        stop=[];
        distanceC=round(((localLat1(2:end)-localLat1(1:end-1)).^2+(localLon1(2:end)-localLon1(1:end-1)).^2).^0.5);
        speed_calculated = ((localLat1(2:end)-localLat1(1:end-1)).^2+(localLon1(2:end)-localLon1(1:end-1)).^2).^0.5./...
            seconds(diff(mldateTime1)); % m/sec
        speed_calculated=[nan; speed_calculated];
        distTemp=[];
        
        while ii<=returned
            while ii<=returned && speed1(ii)<=MaxSpeed %if NOT flying
                if count~=0 % if NOT first point
                    distTemp=[distTemp; ((localLat1(ii)-localLat1(ii-1))^2+(localLon1(ii)-localLon1(ii-1))^2)^0.5];
                end
                if count==0 % if first point
                    stop=ii; % arrived
                elseif speed_calculated(ii) < MaxSpeedCalc
                    ii=ii+1;
                    count=count+1;
                    continue;
                else
                    break;
                end
                ii=ii+1;
                count=count+1;
            end
            
            if~isempty(stop)
                ArriveLeft(ii,1)=stop; % arrived
                if ii>length(localLon1) %if the end of the day
                    if count>1 % if more then one point
                        ArriveLeft(ii,2)=length(localLon1); % left
                    else % if only one point erase it
                        ArriveLeft(ii,1)=0;
                        ArriveLeft(ii,2)=0;
                        count=0;
                    end
                else
                    ArriveLeft(ii,2)=ii; % left
                end
                ArriveLeft(ii,3)=count; % number of points in
                stop=[];
                count=0;
            else
                ii=ii+1;
            end
        end
        if isempty(ArriveLeft)
            continue;
        end
        ArriveLeft=ArriveLeft(ArriveLeft(:,3)>0,:);
        % take the relevant fileds
        In=find(numberOfDays2018==dates(r));
        FiledsS=FiledDay(In,:);
        FiledsS_T = struct2table(FiledsS);
        Date=datetime(dates(r),'ConvertFrom','datenum','format','y-MM-dd');
        temp=[];
        for z=1:length(ArriveLeft(:,1))
            indArr=ArriveLeft(z,1);
            indDep=ArriveLeft(z,2)-1;
            TimeArrival_utc=datetime(mldate1(indArr),'ConvertFrom','datenum','format','y-MM-dd HH:mm:ss');
            TimeDeparture_utc=datetime(mldate1(indDep),'ConvertFrom','datenum','format','y-MM-dd HH:mm:ss');
            TimeInMin=round((mldate1(indDep)-mldate1(indArr))*86400/60);
            TimeInMax=round((mldate1(indDep+1)-mldate1(indArr-1))*86400/60);
            departureLocalLon=localLon1(indDep);
            departureLocalLat= localLat1(indDep);
            departureLon=Lon1(indDep);
            departureLat= Lat1(indDep);
            arrivalLocalLon= localLon1(indArr);
            arrivalLocalLat= localLat1(indArr);
            arrivalLon= Lon1(indArr);
            arrivalLat= Lat1(indArr);
            
            % find main functional crop and regular crop
            [indField,Crop,CropName,disturbance,functional_crop,functional_crop_names]=...
            FindField(localLon1(indArr:indDep),localLat1(indArr:indDep),FiledsS,Daled);
        
            [uvals, Ind, uidx] = unique([functional_crop]);
            field_visited = [uvals, accumarray(uidx, 1),Ind];
            ind_max_func=field_visited(field_visited(:,2)==max(field_visited(:,2)),:);
            if length(ind_max_func(:,1))>1
                ind_max_func=ind_max_func(1,:);
            end
            
            [uvals, Ind, uidx] = unique([Crop]);
            field_visited = [uvals, accumarray(uidx, 1),Ind];
            ind_max_c=field_visited(field_visited(:,2)==max(field_visited(:,2)),:);
            if length(ind_max_c(:,1))>1
                ind_max_c=ind_max_c(1,:);
            end
            main_Crop_indField=indField(ind_max_c(3));
            main_disturbance=disturbance(ind_max_c(3));
            main_Crop=Crop(ind_max_c(3));
            main_CropName=CropName(ind_max_c(3));
            main_per=ind_max_c(2)/length(functional_crop);
            main_functional_crop=functional_crop(ind_max_func(3));
            main_functional_crop_names=functional_crop_names(ind_max_func(3));
            main_functional_per=ind_max_func(2)/length(functional_crop);
            
            temp=[temp; table(indev,Date,TimeArrival_utc,TimeDeparture_utc,TimeInMin,TimeInMax,...
                arrivalLocalLon,arrivalLocalLat,arrivalLon,arrivalLat,...
                departureLocalLon,departureLocalLat,departureLon,departureLat,...
                main_Crop_indField,main_disturbance,main_Crop,main_CropName,main_per,main_functional_crop,main_functional_crop_names,main_functional_per)];
        end
        [arrival_indField,arrival_Crop,arrival_CropName,arrival_disturbance,arrival_functional_crop,arrival_functional_crop_names]=...
            FindField(temp.arrivalLocalLon,temp.arrivalLocalLat,FiledsS,Daled);
        [departure_indField,departure_Crop,departure_CropName,departure_disturbance,departure_functional_crop,departure_functional_crop_names]=...
            FindField(temp.departureLocalLon,temp.departureLocalLat,FiledsS,Daled);
        
          [departure_indField,departure_Crop,departure_CropName,departure_disturbance,departure_functional_crop,departure_functional_crop_names]=...
            FindField(temp.departureLocalLon,temp.departureLocalLat,FiledsS,Daled);
        
        StopInformationTable_new=[StopInformationTable_new; [temp,...
            table(arrival_indField,arrival_Crop,arrival_CropName,arrival_disturbance,arrival_functional_crop,arrival_functional_crop_names,...
            departure_indField,departure_Crop,departure_CropName,departure_disturbance,departure_functional_crop,departure_functional_crop_names)]];
        
        if rand(1,1)>0.995
            % plot to see
            figure
            plot(Lon1,Lat1,'.-w','MarkerSize',9)
            hold on
%             colors=distinguishable_colors(length(ArriveLeft(:,1)));
%             for z=1:length(ArriveLeft(:,1))
%                 plot(Lon1(ArriveLeft(z,1):ArriveLeft(z,2)-1),Lat1(ArriveLeft(z,1):ArriveLeft(z,2)-1),'.','Color',colors(z,:),'MarkerSize',20)
%             end
            colors=distinguishable_colors(height(temp));
            for z=1:height(temp)
                indplot=find(mldate1>=datenum(temp.TimeArrival_utc(z)) & mldate1<=datenum(temp.TimeDeparture_utc(z)));
                if length(indplot)>2
                plot(Lon1(indplot(2:end-1)),Lat1(indplot(2:end-1)),'.','Color',colors(z,:),'MarkerSize',20,'LineWidth',2)
                end
                plot(temp.arrivalLon(z),temp.arrivalLat(z),'x','Color',colors(z,:),'MarkerSize',10,'LineWidth',2)
                plot(temp.departureLon(z),temp.departureLat(z),'^','Color',colors(z,:),'MarkerSize',5,'LineWidth',2)
            end
            plot_google_map('MapType','satellite')
            title(['tag ',num2str(indev),' ' ,datestr(Date)])
        end
    end
end

save('StopInformationTable_new_3_08','StopInformationTable_new')


