clear all
load StopInformationTable_4SSF_3_08
%load filedsPerday_new
load MigVSWintIsrael1

%-- feeding station polygon -------------------------------------------
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');
%-- Roost polygons -------------------------------------------
Water = kml2struct('../../LandCover\Agmaon\AgmonWater.kml');
HulaReserve=kml2struct('../../LandCover\Agmaon\HulaReserve.kml');
HatzafaReserve=kml2struct('../../LandCover\Agmaon\HatzafaReserve.kml');
MaagarEynan=kml2struct('../../LandCover\Agmaon\MaagarEynan.kml');
% ---- loop over all the indeviduals we have data on ----------
directory='../DataArchive';
FilesIn=struct2table(dir('../DataArchive'));
IndevidualsExistTemp=FilesIn.name(FilesIn.bytes>0);
for ind=1:length(IndevidualsExistTemp)
    if contains(IndevidualsExistTemp{ind},'mat')==0
        continue
    end
    Name=IndevidualsExistTemp{ind};
    IndevidualsExistNum(ind,1)=str2num(Name(1:end-4));
end

% variables
StopInformationTable2018=StopInformationTable_new_FuncUnt;
dates=datenum(unique(StopInformationTable2018.Date));
tags=unique(StopInformationTable2018.indev);
Distances=[];
last_p_before_leavingRoost=[];
for i=1:length(tags)
    %% Run date by date
    for ii=1:length(dates)
        % cut only the part relevant for tag and date
        TableTagDate=StopInformationTable2018(datenum(StopInformationTable2018.Date)==dates(ii) & StopInformationTable2018.indev==tags(i),:);
        if isempty(TableTagDate) % if the tag not there at the date
            continue
        end
        disp(['working on indevidual ',num2str(tags(i)),' on ',datestr(dates(ii))])
        %---- load GPS data ------------------------------------------
        ind=find(IndevidualsExistNum==tags(i));
        load([directory,'\',char(IndevidualsExistTemp(ind))]);
        Relevantdata=~isnan(TagMAT(:,8))  & TagMAT(:,5)~=0 & floor(TagMAT(:,3))==datenum(dates(ii));
        lon=TagMAT(Relevantdata,4);
        lat=TagMAT(Relevantdata,5);
        mldate=TagMAT(Relevantdata,3);
        speed=TagMAT(Relevantdata,8); 
        [localLon1, localLat1]=TransWGS2NIGrid(lat,lon,ones(size(lat)));
        
        %--load roost data ------------------------------------------------
        r=NightLocationsTNew.Indevidual==tags(i) & datenum(NightLocationsTNew.Date)==dates(ii);
        RoostLat=NightLocationsTNew.meanLat(r);
        RoostLon=NightLocationsTNew.meanLon(r);
        [xRoost, yRoost]=TransWGS2NIGrid(RoostLat,RoostLon,1); % convert to local grid
        
        %-- calculate sunrize and distance to roost -----------------------
        [SunRiseSet]=suncycle(lon(1) ,lat(1) ,mldate(1)); %in order to get time to add to the date divide in 24
        SunRiseSet2use=SunRiseSet/24; %sunrise and sunset in UTC
        SunRiseSet2use(1)=SunRiseSet2use(1)-1.5/24+floor(dates(ii)); 
        DistNightBefore=distance(lat,lon,RoostLat,RoostLon,wgs84Ellipsoid); 
        %-- check in which roost was --------------------------------
            
        % if roosting in in the feeeding station start/end day just based on time
        inDaled=inpolygon(RoostLat,RoostLon,Daled.Lat,Daled.Lon);
        if inDaled(1)==1
            left=find(mldate>=SunRiseSet2use(1),1,'first');
        else
            inRoost=inpolygon(lat,lon,Water.Lat,Water.Lon)|...
                inpolygon(lat,lon,HulaReserve.Lat,HulaReserve.Lon) |...
                inpolygon(lat,lon,MaagarEynan.Lat,MaagarEynan.Lon) |...
                inpolygon(lat,lon,HatzafaReserve.Lat,HatzafaReserve.Lon);
            left=find(mldate>=SunRiseSet2use(1) & ...
                DistNightBefore>200 & ~inRoost,1,'first');
        end
        %--check that the last point is not flight---
        IND=[1:1:length(lon)]';
        IndPBL=find(speed<3 & IND<left,1,'last');
        lon_lastPR=lon(IndPBL);
        lat_lastPR=lat(IndPBL);
        [x_lastPR, y_lastPR]=TransWGS2NIGrid(lat_lastPR,lon_lastPR,1); % convert to local grid
        % == add the roost point before the day points ================
        xarr=TableTagDate.arrivalLocalLon;
        xdep=TableTagDate.departureLocalLon;
        yarr=TableTagDate.arrivalLocalLat;
        ydep=TableTagDate.departureLocalLat;  
        xdep=[x_lastPR;xdep(1:end-1)];  
        ydep=[y_lastPR;ydep(1:end-1)];
        D=[sqrt((xarr-xdep).^2+(yarr-ydep).^2)];
        TimeIn=mean([TableTagDate.TimeInMin,TableTagDate.TimeInMax],2);
        Distances=[Distances; [D,[nan; TimeIn(1:end-1)],TimeIn]]; 
        
        last_p_before_leavingRoost=[last_p_before_leavingRoost; [tags(i),dates(ii),lon_lastPR,lat_lastPR,x_lastPR,y_lastPR]];
        
        if rand(1,1)>0.995
            D;
        end
    end
end
DistancesT=array2table(Distances,'VariableNames',{'distance','TimeBeforeLeft','TimeAfterArrived'});
last_point_blr=array2table(last_p_before_leavingRoost,'VariableNames',{'tag','time_num','lon_lastBLR','lat_lastBLR','x_lastPR','y_lastPR'});
save('distributionSSF_3_08','DistancesT','last_point_blr')

%% plot for Fig. S1 appendix S4
figure

subplot(2,1,1)
hold on
histogram(DistancesT.distance/1000,100,'FaceAlpha',0.5)
xlabel('Distance (km)')
ylabel('Frequency of moves')
ylim([0 2500])
xlim([0 14])
ax1 = gca;
ax1.FontSize=20;
ax1.YTick=[0,1000,2000];

subplot(2,1,2)
hold on
histogram(DistancesT.TimeAfterArrived,100,'FaceAlpha',0.5)
xlabel('Stop durations (min.)')
ylabel('Frequency of moves')
ylim([0 2500])
xlim([0 600])
ax1 = gca;
ax1.YTick=[0,1000,2000];
ax1.FontSize=20;


figure
Lon1=lon;
Lat1=lat;
mldate1=mldate;
temp1=TableTagDate;
plot(Lon1,Lat1,'.-w','MarkerSize',9)
hold on
colors=distinguishable_colors(height(temp1));
for z=1:height(temp1)
    indplot=find(mldate>=datenum(temp1.TimeArrival_utc(z)) & mldate<=datenum(temp1.TimeDeparture_utc(z)));
    plot(lon(indplot),lat(indplot),'.','Color',colors(z,:),'MarkerSize',20)
end
plot(RoostLon,RoostLat,'xr','MarkerSize',10,'LineWidth',2)
plot(lon_lastPR,lat_lastPR,'xy','MarkerSize',10,'LineWidth',2)
plot_google_map('MapType','satellite')
title(['tag ',num2str(tags(i)),' ' ,datestr(dates(ii))])