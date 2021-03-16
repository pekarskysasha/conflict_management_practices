clear all
load StopInformationTable_4SSF_3_08
directory='../DataArchive';
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');
StopInformationTable_new=StopInformationTable_new_FuncUnt;
%% ---- load tag data
FilesIn=struct2table(dir('../DataArchive'));
IndevidualsExistTemp=FilesIn.name(FilesIn.bytes>0);
for ind=1:length(IndevidualsExistTemp)
    Name=IndevidualsExistTemp{ind};
    IndevidualsExistNum(ind,1)=str2num(Name(1:end-4));
end

% Do we want to agragate flight inside the same functional unit?
StopInformationTable_new_Try=[];
tags=unique(StopInformationTable_new.indev);
Distancesagr=[];
for i=1:length(tags)
    % load movement data
    ind=find(IndevidualsExistNum==tags(i));
    load([directory,'\',char(IndevidualsExistTemp(ind))]);
    lon=TagMAT(:,4);
    lat=TagMAT(:,5);
    mldate=TagMAT(:,3);
    speed=TagMAT(:,8);
    % remove duplicate data
    aa=find(diff(mldate)==0);
    lon(aa)=[];
    lat(aa)=[];
    speed(aa)=[];
    mldate(aa)=[];
    Allind=[1:1:length(lon)]';
    
    dates=unique(StopInformationTable_new.Date(StopInformationTable_new.indev==tags(i)));
    TimeGap=[];
    for ii=1:length(dates)
        temp=[];
        temp1=StopInformationTable_new(StopInformationTable_new.Date==dates(ii) & StopInformationTable_new.indev==tags(i),:);
        c=2;
        temp=[temp; temp1(1,:)];
        while c<=height(temp1)
            numbP=find((mldate==datenum(temp1.TimeArrival_utc(c))))-find(mldate==datenum(temp.TimeDeparture_utc(end)));
            LatA=lat((mldate==datenum(temp1.TimeArrival_utc(c))));
            LonA=lon((mldate==datenum(temp1.TimeArrival_utc(c))));
            LatD=lat((mldate==datenum(temp.TimeDeparture_utc(end))));
            LonD=lon((mldate==datenum(temp.TimeDeparture_utc(end))));
            [localLon1, localLat1]=TransWGS2NIGrid([LatD;LatA],[LonD;LonA],ones(2,1));
            distanceC=round(((localLat1(2:end)-localLat1(1:end-1)).^2+(localLon1(2:end)-localLon1(1:end-1)).^2).^0.5);
            if temp1.index_field(c)==temp.index_field(end) && numbP<=2
                Arrived=temp.TimeArrival_utc(end);
                Left=temp1.TimeDeparture_utc(c);
                TimeGap=[TimeGap; round(minutes(temp1.TimeArrival_utc(c)-temp.TimeDeparture_utc(end)))];
                %%-------------------- find the GPS data for median location-----------------------------------------------------------------------
                %--lat & lon locations of this and previous stops
                LonAll=[lon(mldate>=datenum(temp.TimeArrival_utc(end)) & mldate<=datenum(temp.TimeDeparture_utc(end)));
                    lon(mldate>=datenum(temp1.TimeArrival_utc(c)) & mldate<=datenum(temp1.TimeDeparture_utc(c)))];
                LatAll=[lat(mldate>=datenum(temp.TimeArrival_utc(end)) & mldate<=datenum(temp.TimeDeparture_utc(end)));
                    lat(mldate>=datenum(temp1.TimeArrival_utc(c)) & mldate<=datenum(temp1.TimeDeparture_utc(c)))];
                [localLon1, localLat1]=TransWGS2NIGrid(LatAll,LonAll,ones(size(LatAll)));
                %-- time from arrival previous point to departue this point
                TimeInMin=round((mldate(mldate==datenum(temp1.TimeDeparture_utc(c)))-...
                    mldate(mldate==datenum(temp.TimeArrival_utc(end))))*86400/60);
                TimeInMax=round((mldate(find(mldate==datenum(temp1.TimeDeparture_utc(c)),1,'first')+1)-...
                    mldate(find(mldate==datenum(temp1.TimeArrival_utc(c)),1,'first')-1))*86400/60);
                %%-------------------- Fill the new dat to the last row, instead of the info there---------------------------------------------------
                temp.departureLocalLon(end)=temp1.departureLocalLon(c);
                temp.departureLocalLat(end)=temp1.departureLocalLat(c);
                temp.departureLon(end)=temp1.departureLon(c);
                temp.departureLat(end)=temp1.departureLat(c);
                temp.TimeInMin(end)=TimeInMin;
                temp.TimeInMax(end)=TimeInMax;
                temp.TimeDeparture_utc(end)=temp1.TimeDeparture_utc(c);
                c=c+1;
                Distancesagr=[Distancesagr; [numbP,distanceC]];
            else
                temp=[temp; temp1(c,:)];
                c=c+1;
            end
        end
        StopInformationTable_new_Try=[StopInformationTable_new_Try; temp];
    end
end
StopInformationTable_new_FuncUnt_agr=StopInformationTable_new_Try;
save('StopInformationTable_agraegate','StopInformationTable_new_FuncUnt_agr')