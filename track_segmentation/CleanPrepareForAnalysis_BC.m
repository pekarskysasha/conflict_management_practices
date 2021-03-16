clear all
load StopInformationTable_new_3_08
load filedsPerday_new
StopInformationTable_new_FuncUnt=[];
directory='../DataArchive';
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');
[LocLon, LocLat]=TransWGS2NIGrid(Daled.Lat,Daled.Lon,ones(size(Daled.Lon))); % convert to local grid
Daled.LocLon=LocLon;
Daled.LocLat=LocLat;
%% ---- load tag data
FilesIn=struct2table(dir('../DataArchive'));
IndevidualsExistTemp=FilesIn.name(FilesIn.bytes>0);
for ind=1:length(IndevidualsExistTemp)
    if contains(IndevidualsExistTemp{ind},'mat')==0
        continue
    end
    Name=IndevidualsExistTemp{ind};
    IndevidualsExistNum(ind,1)=str2num(Name(1:end-4));
end

% Do we want to agragate flight inside the same functional unit?
aggregate=0;
countWasNan=0;
countbecameNan=0;
countJoin=0;
tags=unique(StopInformationTable_new.indev);
for i=1:length(tags)
    % load movement data
    ind=find(IndevidualsExistNum==tags(i));
    load([directory,'\',char(IndevidualsExistTemp(ind))]);
    lon=TagMAT(:,4);
    lat=TagMAT(:,5);
    mldate=TagMAT(:,3);
    speed=TagMAT(:,8);
    % get rid of duplicate data
    aa=find(diff(mldate)==0);
    lon(aa)=[];
    lat(aa)=[];
    speed(aa)=[];
    mldate(aa)=[];
    Allind=[1:1:length(lon)]';
    
    dates=unique(StopInformationTable_new.Date(StopInformationTable_new.indev==tags(i)));
    TimeGap=[];
    for ii=1:length(dates)
        temp1=[];
        temp=[];
        disp(['Working on individial ',num2str(tags(i)),' on ',datestr(dates(ii))])
        tag_date_stop=StopInformationTable_new(StopInformationTable_new.Date==dates(ii) & StopInformationTable_new.indev==tags(i),:);
        % take the relevant fileds
        In=find(numberOfDays2018==datenum(dates(ii)));
        FiledsS=FiledDay(In,:);
        FiledsT = struct2table(FiledsS); % convert to table
        %% -(1)- if one stop includes more the one functional unit
        %------(1.1) if it is the same functional crop, do nothing as the crop did not change
        %------------* it is important because some functinal units contain the same crop and devided for economical/birocratic reasons
        %------(1.2) if the functional crop is different:
        %---------(1.2.1) if stayed in the main crop for at least 80% of the time: do nothing (as it is probably location errors)
        %---------(1.2.2) if stayed bteween 51% and 80%
        %--------------(1.2.2.1) if the location in other field are no more than 30 meters from the border of the main field:
        %------------------------make them the main fiels and recalculate percent. if >80% keep the main field as crop
        %--------------(1.2.2.2) if locations were not continuous, do nothing (as it is probably location errors)
        %--------------(1.2.2.3) in other cases put NaN in the functional crop and this stop would not be used for SSF
        %---------(1.2.3) in all other cases put NaN in the functional crop and this stop would not be used for SSF
        for b=1:height(tag_date_stop)
            Lon=lon(mldate>=datenum(tag_date_stop.TimeArrival_utc(b)) & mldate<=datenum(tag_date_stop.TimeDeparture_utc(b)));
            Lat=lat(mldate>=datenum(tag_date_stop.TimeArrival_utc(b)) & mldate<=datenum(tag_date_stop.TimeDeparture_utc(b)));
            MLdate=mldate(mldate>=datenum(tag_date_stop.TimeArrival_utc(b)) & mldate<=datenum(tag_date_stop.TimeDeparture_utc(b)));
            %-- find location for every point
            [localLon1, localLat1]=TransWGS2NIGrid(Lat,Lon,ones(size(Lat)));
            [indField,Crop,CropName,disturbance,functional_crop,functional_crop_names]=FindField(localLon1,localLat1,FiledsS,Daled);
            functional_crop(isnan(functional_crop)) = inf; % replace for unique to work
            %-- check if the functional crop is diffrent
            [uvals, Ind, uidx] = unique([functional_crop]);
            field_visited_temp = [indField(Ind),uvals, accumarray(uidx, 1),Ind];
            IndmaxStay=find(field_visited_temp(:,3)== max(field_visited_temp(:,3)));
            if length(IndmaxStay)==1
                main_field=field_visited_temp(IndmaxStay,1);
                main_field_crop_name=functional_crop_names(find(indField==main_field,1,'first'));
                main_field_crop_number=functional_crop(find(indField==main_field,1,'first'));
                ProportionMain=field_visited_temp(IndmaxStay,3)/length(indField);
            else
                ProportionMain=0.5;
                main_field=nan;
            end
            %=== Check the stop=============================================================
            if ProportionMain==1  && ~isnan(main_field) % (1.1) functional crop is the same
                functional_crop_number=main_field_crop_number;
                functional_crop_name=main_field_crop_name;
                index_field=main_field;
                %- fill the table
                temp1=[temp1; [tag_date_stop(b,1:14), ...
                    table(index_field,functional_crop_number,functional_crop_name)]];
            else
                if ProportionMain>=0.8 && ~isnan(main_field)% (1.2.1) stayed in the main crop for at least 80%
                    functional_crop_number=main_field_crop_number;
                    functional_crop_name=main_field_crop_name;
                    index_field=main_field;
                    %- fill the table
                    temp1=[temp1; [tag_date_stop(b,1:14), ...
                        table(index_field,functional_crop_number,functional_crop_name)]];
                elseif ProportionMain>=0.51 &&  ProportionMain<0.8 && ~isnan(main_field) %(1.2.2) stayed bteween 51% and 80%
                    %--calculate the distance to the main field
                    if main_field==0 % feeding station
                        Distances=p_poly_dist(localLat1,localLon1,Daled.LocLat,Daled.LocLon);
                    else
                        indexInStruct=find(FiledsT.ID==main_field);
                        Distances=p_poly_dist(localLat1,localLon1,FiledsS(indexInStruct).Lat,FiledsS(indexInStruct).Lon);
                    end
                    indField_new=indField;
                    indField_new(indField~=main_field & Distances<30)=main_field;
                    if sum(indField_new==main_field)/length(indField_new)>=0.8 %(1.2.2.1)location in other field are no more than 30m
                        functional_crop_number=main_field_crop_number;
                        functional_crop_name=main_field_crop_name;
                        
                        index_field=main_field;
                        %- fill the table
                        temp1=[temp1; [tag_date_stop(b,1:14), ...
                            table(index_field,functional_crop_number,functional_crop_name)]];
                    else
                        field_visited=[];
                        for tt=1:length(field_visited_temp(:,1))
                            m=functional_crop==field_visited_temp(tt,2);
                            mm=diff(m);
                            s=find(mm==1)+1;
                            e=find(mm==-1);
                            %-- if changed at last point
                            if isempty(e)
                                e=[e; length(functional_crop)];
                            end
                            %-- if first already in the filed
                            if  isempty(s) || s(1)> e(1)
                                s=[1; s];
                            end
                            %-- if last still in the field
                            if length(s)>length(e)
                                e=[e; length(functional_crop)];
                            end
                            field_visited=[field_visited; [ones(length(s),1)*field_visited_temp(tt,1),ones(length(s),1)*field_visited_temp(tt,1),s,e]];
                        end
                        field_visited=sortrows(field_visited,3);
                        field_visited_NOTmain=field_visited(field_visited(:,1)~=main_field,:);
                        if sum(field_visited_NOTmain(:,4)-field_visited_NOTmain(:,3)==0)==length(field_visited_NOTmain(:,1)) %(1.2.2.2) if all of them one point
                            functional_crop_number=main_field_crop_number;
                            functional_crop_name=main_field_crop_name;
                            index_field=main_field;
                            %- fill the table
                            temp1=[temp1; [tag_date_stop(b,1:14), ...
                                table(index_field,functional_crop_number,functional_crop_name)]];
                        else %(1.2.2.3) in other cases put NaN
                            functional_crop_number=nan;
                            functional_crop_name=cellstr('unknown');
                            index_field=nan;
                            %- fill the table
                            temp1=[temp1; [tag_date_stop(b,1:14), ...
                                table(index_field,functional_crop_number,functional_crop_name)]];
                            countbecameNan=countbecameNan+1;
                        end
                    end
                else %(1.2.3) in all other cases put NaN
                    functional_crop_number=nan;
                    functional_crop_name=cellstr('unknown');
                    index_field=nan;
                    %- fill the table
                    temp1=[temp1; [tag_date_stop(b,1:14), ...
                        table(index_field,functional_crop_number,functional_crop_name)]];
                    if ProportionMain >= 0.51 & isnan(main_field)
                        countWasNan=countWasNan+1;
                    else
                        countbecameNan=countbecameNan+1;
                    end
                end
            end
        end
        
        %% -(2)- if there is only one point bewteen stops and the next stop is within GPS error, join the stops
        c=2;
        temp=temp1(1,:);
        while c<=height(temp1)
            numbP=find((mldate==datenum(temp1.TimeArrival_utc(c))))-find(mldate==datenum(temp.TimeDeparture_utc(end)));
            LatA=lat((mldate==datenum(temp1.TimeArrival_utc(c))));
            LonA=lon((mldate==datenum(temp1.TimeArrival_utc(c))));
            LatD=lat((mldate==datenum(temp.TimeDeparture_utc(end))));
            LonD=lon((mldate==datenum(temp.TimeDeparture_utc(end))));
            [localLon1, localLat1]=TransWGS2NIGrid([LatD;LatA],[LonD;LonA],ones(2,1));
            distanceC=round(((localLat1(2:end)-localLat1(1:end-1)).^2+(localLon1(2:end)-localLon1(1:end-1)).^2).^0.5);
            if temp1.index_field(c)==temp.index_field(end) && numbP<=2 && distanceC<10
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
                countJoin=countJoin+1;
            else
                temp=[temp; temp1(c,:)];
                c=c+1;
            end
        end
        StopInformationTable_new_FuncUnt=[StopInformationTable_new_FuncUnt; temp];
    end
end
save('StopInformationTable_4SSF_3_08','StopInformationTable_new_FuncUnt','countJoin','countWasNan','countbecameNan')
% plot to see OLD
figure
Lon1=lon(floor(mldate)==datenum(tag_date_stop.Date(1)));
Lat1=lat(floor(mldate)==datenum(tag_date_stop.Date(1)));
mldate1=mldate(floor(mldate)==datenum(tag_date_stop.Date(1)));
plot(Lon1,Lat1,'.-w','MarkerSize',9)
hold on
colors=distinguishable_colors(height(tag_date_stop));
for z=1:height(tag_date_stop)
    indplot=find(mldate1>=datenum(tag_date_stop.TimeArrival_utc(z)) & mldate1<=datenum(tag_date_stop.TimeDeparture_utc(z)));
    plot(Lon1(indplot),Lat1(indplot),'.','Color',colors(z,:),'MarkerSize',20)
end
plot_google_map('MapType','satellite')
title(['tag ',num2str(tags(i)),' ' ,datestr(dates(ii))])

% plot to see NEW
figure
Lon1=lon(floor(mldate)==datenum(temp1.Date(1)));
Lat1=lat(floor(mldate)==datenum(temp1.Date(1)));
mldate1=mldate(floor(mldate)==datenum(temp1.Date(1)));
plot(Lon1,Lat1,'.-w','MarkerSize',9)
hold on
colors=distinguishable_colors(height(temp1));
for z=1:height(temp1)
    indplot=find(mldate>=datenum(temp1.TimeArrival_utc(z)) & mldate<=datenum(temp1.TimeDeparture_utc(z)));
    plot(lon(indplot),lat(indplot),'.','Color',colors(z,:),'MarkerSize',20)
end
plot_google_map('MapType','satellite')
title(['tag ',num2str(tags(i)),' ' ,datestr(dates(ii))])


figure
Lon1=lon(floor(mldate)==datenum(temp.Date(1)));
Lat1=lat(floor(mldate)==datenum(temp.Date(1)));
mldate1=mldate(floor(mldate)==datenum(temp.Date(1)));
plot(Lon1,Lat1,'.-w','MarkerSize',9)
hold on
colors=distinguishable_colors(height(temp));
for z=1:height(temp)
    indplot=find(mldate1>=datenum(temp.TimeArrival_utc(z)) & mldate1<=datenum(temp.TimeDeparture_utc(z)));
    plot(Lon1(indplot),Lat1(indplot),'.','Color',colors(z,:),'MarkerSize',20)
end
plot_google_map('MapType','satellite')
title(['tag ',num2str(tags(i)),' ' ,datestr(dates(ii))])


Penut=[239, 195, 112]/255;
Corn=[247, 245, 150]/255;
Tayarut=[224, 241, 207]/255;
figure
clf
for iii=1:length(FiledsS) %all fields
    x= FiledsS(iii).Lon;
    y= FiledsS(iii).Lat;
    x=x(~isnan(x));
    y=y(~isnan(y));
    hold on
    if FiledsS(iii).CropCode==4 || FiledsS(iii).CropCode==6 || FiledsS(iii).CropCode==3
        s=fill(x,y,Penut);
        set(s,'FaceAlpha',0.7,'EdgeAlpha',0.5,'HandleVisibility','off')
    elseif  FiledsS(iii).CropCode==5
        s=fill(x,y,Tayarut);
        set(s,'FaceAlpha',0.7,'EdgeAlpha',0.5,'HandleVisibility','off')
    elseif  FiledsS(iii).DisturbanceLevel>0
        s=fill(x,y,Corn);
        set(s,'FaceAlpha',0.7,'EdgeAlpha',0.5,'HandleVisibility','off')
    else
        s=fill(x,y,'w');
        set(s,'FaceAlpha',0.7,'EdgeAlpha',0.5,'HandleVisibility','off')
    end
end

Lon1=lon(floor(mldate)==datenum(tag_date_stop.Date(1)));
Lat1=lat(floor(mldate)==datenum(tag_date_stop.Date(1)));
[localLon1, localLat1]=TransWGS2NIGrid(Lat1,Lon1,ones(size(Lat1)));
mldate1=mldate(floor(mldate)==datenum(tag_date_stop.Date(1)));
plot(localLon1,localLat1,'.-k','MarkerSize',9)
hold on
colors=distinguishable_colors(height(tag_date_stop));
for z=1:height(tag_date_stop)
    indplot=find(mldate1>=datenum(tag_date_stop.TimeArrival_utc(z)) & mldate1<=datenum(tag_date_stop.TimeDeparture_utc(z)));
    plot(localLon1(indplot),localLat1(indplot),'.','Color',colors(z,:),'MarkerSize',20)
end

% 16113, 2017-10-22 6 => 7
% 170941, 2018-01-16 first stop departure is zero

%% 
%- 159 events joined with the next stop
%- 1351 stops are nan and not used for ssf