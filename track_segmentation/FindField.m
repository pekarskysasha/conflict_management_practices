function [indField,Crop,CropName,disturbance,functional_crop,functional_crop_names]=FindField(MeanLocalLonAll,MeanLocalLatAll,FiledsS,Deled)
%% Daled (feeding station)
[LocLon, LocLat]=TransWGS2NIGrid(Deled.Lat,Deled.Lon,ones(size(Deled.Lon))); % convert to local grid
Deled.LocLon=LocLon;
Deled.LocLat=LocLat;
%% find distances and location in polygons
fields=[];
Distances=[];
problems=[];
for g=1:length(FiledsS)
    fields=[fields,inpolygon(MeanLocalLatAll,MeanLocalLonAll,FiledsS(g).Lat,FiledsS(g).Lon)];
    try
        Distances=[Distances,p_poly_dist(MeanLocalLatAll,MeanLocalLonAll,FiledsS(g).Lat,FiledsS(g).Lon)];
    catch
        Distances=[Distances,nan(size(MeanLocalLonAll))];
        problems=[problems; g];
    end
    
end
%% Is at the feeding station (Daled)
InDaled=inpolygon(MeanLocalLatAll,MeanLocalLonAll,Deled.LocLat,Deled.LocLon);
%% look where is it
for f=1:length(MeanLocalLonAll)
    if InDaled(f)==0
        I=find(fields(f,:)==1);
        if ~isempty(I) && length(I)==1 % if only one field
            Crop(f,1)=FiledsS(I).CropCode;
            CropName(f,1)=cellstr(FiledsS(I).Crop);
            indField(f,1)=FiledsS(I).ID;
            disturbance(f,1)=FiledsS(I).DisturbanceLevel;
       %--empty, check if is just on the border
        elseif isempty(I) 
            % find the nearest
            IndNEarest=find(Distances(f,:)==min(Distances(f,:))); %find the nearest field
             if length(IndNEarest)>1
                IndNEarest=IndNEarest(1);
             end
            if ~isempty(IndNEarest) && Distances(f,IndNEarest)>0 && Distances(f,IndNEarest)<30 % if outside (>0) and less then 30 meters
                Crop(f,1)=FiledsS(IndNEarest).CropCode;
                CropName(f,1)=cellstr(FiledsS(IndNEarest).Crop);
                indField(f,1)=FiledsS(IndNEarest).ID;
                disturbance(f,1)=FiledsS(IndNEarest).DisturbanceLevel;
            else
                indField(f,1)=NaN;
                Crop(f,1)=NaN;
                CropName(f,1)=cellstr('unknown');
                disturbance(f,1)=NaN;
            end
        elseif length(I)>1 % more then one polygon
            %-- first look, if we have bare and not bare, shoose the one with crop 
            cropsin=[];
            for z=1:length(I)
               cropsin=[cropsin; FiledsS(I(z)).CropCode]; 
            end
            if sum(cropsin==7)<length(cropsin)
                I(cropsin==7)=[];
            end
            
            %----we take the field in which it furthest from the border
            IndNEarest=find(Distances(f,:)==min(Distances(f,I))); %find the nearest field
            if length(IndNEarest)>1
                IndNEarest=IndNEarest(1);
            end
            if ~isempty(IndNEarest) && Distances(f,IndNEarest)<0 % inside
                Crop(f,1)=FiledsS(IndNEarest).CropCode;
                CropName(f,1)=cellstr(FiledsS(IndNEarest).Crop);
                indField(f,1)=FiledsS(IndNEarest).ID;
                disturbance(f,1)=FiledsS(IndNEarest).DisturbanceLevel;
            else
                indField(f,1)=NaN;
                Crop(f,1)=NaN;
                CropName(f,1)=cellstr('unknown');
                disturbance(f,1)=NaN;
            end
        else
           indField(f,1)=NaN;
           Crop(f,1)=NaN;
           CropName(f,1)=cellstr('unknown');
           disturbance(f,1)=NaN;
        end
    else
        indField(f,1)=0;
        Crop(f,1)=0;
        CropName(f,1)=cellstr('feeding station');
        disturbance(f,1)=0;
        
    end
end
functional_crop=zeros(length(Crop),1);
for r=1:length(Crop)
 IND=Crop(r);
 if IND==3 | IND==4 | IND==6 | IND==18  | IND==19 | IND==21 | IND==25 
     %Harvested (peanut, corn, watermelon, beans, peas,chickpeas,sitaria)
     functional_crop(r)=1;  
     functional_crop_names(r,1)=cellstr('harvested');
 elseif IND==5
     %alfalfa
     functional_crop(r)=2; 
     functional_crop_names(r,1)=cellstr('perennial');
 elseif IND==8 | IND==11 | IND==12 | IND==10
     %Annual 
     functional_crop(r)=3; 
     functional_crop_names(r,1)=cellstr('annual');   
 elseif  isnan(IND)
    functional_crop(r)=nan; 
    functional_crop_names(r,1)=cellstr('unknown');
 elseif IND==13 | IND==15
     %Almond & pecan
     functional_crop(r)=5;
     functional_crop_names(r,1)=cellstr('orchard');
 elseif IND==0
     functional_crop(r)=6;
     functional_crop_names(r,1)=cellstr('feeding'); 
 else
     %Other
     functional_crop(r)=4; 
     functional_crop_names(r,1)=cellstr('other');
 end
end
end