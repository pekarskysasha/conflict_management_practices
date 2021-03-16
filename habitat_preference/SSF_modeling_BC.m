clear all
load StopInformationTable_4SSF_3_08
load filedsPerday_new
load distributionSSF_3_08
Daled=kml2struct('../../LandCover\Agmaon\Deled.kml');

figure
All=[];
% number of random points (generate 100, take first 20 that have habitat info)
NR=100;
% variables
StopInformationTable2018=StopInformationTable_new_FuncUnt;
dates=datenum(unique(StopInformationTable2018.Date));
tags=unique(StopInformationTable2018.indev);
%% Create period dates
Period=zeros(length(dates),1);
Period(dates>=datenum('04/12/17','dd/mm/yy')& dates<datenum('21/12/17','dd/mm/yy'))=1;
Period(dates>=datenum('21/12/17','dd/mm/yy'))=2;
%% distribution for modelling
%--distance ----------------------------------------
dist_all=DistancesT.distance;
Dist_roost=DistancesT.distance(isnan(DistancesT.TimeBeforeLeft));
Dirst_interfirld=DistancesT.distance(~isnan(DistancesT.TimeBeforeLeft));
%-- time --------------------------------------------
MTimeInFeild=DistancesT.TimeAfterArrived;
%% Check correlation between time in filed and diatnce traveled before and after
[R_TimeBeforeLeft,P_TimeBeforeLeft] = corrcoef(DistancesT.TimeBeforeLeft,DistancesT.distance);
[R_Timeafterarrived,P_Timeafterarrived] = corrcoef(DistancesT.TimeAfterArrived,DistancesT.distance);
% writetable(DistancesT,'TimeInFieldAndDistance.csv')
%% prepare variables
indFieldAll=[];
CropAll=[];
Real=[];
strata=[];
tag=[];
TimIn=[];
Period=[];
count=1;
%% Analyse per tag per date
for i=1:length(tags)
    %% Run date by date
    for ii=1:length(dates)
        Short=0;
        % take the relevant fileds
        In=find(numberOfDays2018==dates(ii));
        FiledsS=FiledDay(In,:);
        % cut only the part relevant for tag and date
        TableTagDate=StopInformationTable2018(datenum(StopInformationTable2018.Date)==dates(ii) & StopInformationTable2018.indev==tags(i),:);
        if isempty(TableTagDate) % if the tag not there at the date
            continue
        end
        % find the roost
        r=last_point_blr.tag==tags(i) & last_point_blr.time_num==dates(ii);
        yRoost=last_point_blr.y_lastPR(r);
        xRoost=last_point_blr.x_lastPR(r);
        
        disp(['working on indevidual ',num2str(tags(i)),' on ',datestr(dates(ii))])
            
        % == add the roost point before the day points ================
        xarr=TableTagDate.arrivalLocalLon;
        xdep=TableTagDate.departureLocalLon;
        yarr=TableTagDate.arrivalLocalLat;
        ydep=TableTagDate.departureLocalLat;  
        xdep=[xRoost;xdep];  
        ydep=[yRoost;ydep];
        % == field departure and field arrival ================ 
        %--- nan: means the point is outside the mapped agricaltural area; this point is not used for modelling
        %--- inf: it is the roost
        crNarr=TableTagDate.functional_crop_number;
        crNdep=[inf; TableTagDate.functional_crop_number];
        for s=1:length(xarr)
        %% -- (a) realized step:
        if isnan(crNarr(s))
            continue;
        end
        TimeInFieldReal=mean([TableTagDate.TimeInMin(s),TableTagDate.TimeInMax(s)]);
        LonReal=xarr(s);
        LatReal=yarr(s);
        functional_crop_numberReal=TableTagDate.functional_crop_number(s);
        functional_crop_nameReal=TableTagDate.functional_crop_name(s);
        %% -- (b) create the random steps:
        %----(1) create every time 10 steps and remove steps not in mapped agricaltural land
        %----(2) continue until thre are are 20 steps
        TimeInFieldRand=[];
        LonRand=[];
        LatRand=[];
        functional_crop_numberRand=[];
        functional_crop_nameRand=[];
        while length(LonRand)<21
            % == random step length: from lognormal distribution ==========
            ra=randi([1 length(dist_all)],10,1);
            steps=dist_all(ra);
        
            % == random turning angle: number between zero and one=========
            turnA=rand(10,1);    
            
            % == random time in field: from distribution ==================
            raT=randi([1 length(MTimeInFeild)],10,1);
            TimeInField=MTimeInFeild(raT);

            % == create the steps =========================================
            x1=xdep(s)+turnA.*steps.*sign(rand(10,1)-0.5);
            y1=ydep(s)+sqrt(1-turnA.^2).*steps.*sign(rand(10,1)-0.5);
            Lontemp=x1;
            Lattemp=y1;
            
            % == find crop and field index ================================
            [indField,Crop,CropName,disturbance,functional_crop,functional_crop_names]=FindField(Lontemp,Lattemp,FiledsS,Daled);
            % remove the points outside the mapped agricaltural land
            IndRand=isnan(indField);
            functional_crop(IndRand)=[];
            functional_crop_names(IndRand)=[];
            Lontemp(IndRand)=[];
            Lattemp(IndRand)=[];
            TimeInField(IndRand)=[];
            % fill in the final points
            TimeInFieldRand=[TimeInFieldRand; TimeInField];
            LonRand=[LonRand; Lontemp];
            LatRand=[LatRand; Lattemp];
            functional_crop_numberRand=[functional_crop_numberRand; functional_crop];
            functional_crop_nameRand=[functional_crop_nameRand; functional_crop_names];
        end
        if length(TimeInFieldRand)>20
            TimeInFieldRand=TimeInFieldRand(1:20);
            LonRand=LonRand(1:20);
            LatRand=LatRand(1:20);
            functional_crop_numberRand=functional_crop_numberRand(1:20);
            functional_crop_nameRand=functional_crop_nameRand(1:20);
        end 
        Time_Field=[TimeInFieldReal; TimeInFieldRand];
        Lon=[LonReal; LonRand];
        Lat=[LatReal; LatRand];
        crop_number=[functional_crop_numberReal; functional_crop_numberRand];
        crop_name=[functional_crop_nameReal; functional_crop_nameRand];
        
        %--- plot to see -------------------------------------
        %figure
        %plotsteps(FiledsS,Lon,Lat,xdep(s),ydep(s),dates(ii),tags(i)) % the colors go from blue to red
        
        % save the random and real point location and data
            Tag=ones(length(crop_number),1)*tags(i);
            Strata=ones(length(crop_number),1)*count;
            DateV=datetime(ones(length(crop_number),1)*dates(ii),'ConvertFrom','datenum','format','y-MM-dd');
            Random_realized=false(length(crop_number),1);
            Random_realized(1)=true;
            All=[All; table(Tag,DateV,Random_realized,crop_number,crop_name,Time_Field,Strata,...
                'VariableNames',{'Tag','Date','Random_Realized','Crop','Field','Time_in','Strata'})];
            count=count+1;
        end
        
    end
end
%% reshape the table with diffrent names

Crops=zeros(height(All),6);
for r=1:height(All)
 Crops(r,All.Crop(r))=round(All.Time_in(r))/60;   
end

CropsT=array2table(Crops,...
'VariableNames',{'Harvested','Perennial','Annual','Other','Orchard','Feeding'});
%% for 3 periods
BeforeAfter=zeros(height(All),1);
BeforeAfter(datenum(All.Date)>datenum('14/10/2017','dd/mm/yyyy') & datenum(All.Date)<datenum('05/12/2017','dd/mm/yyyy'))=1;
BeforeAfter(datenum(All.Date)>datenum('04/12/2017','dd/mm/yyyy') & datenum(All.Date)<datenum('21/12/2017','dd/mm/yyyy'))=2;
BeforeAfter(datenum(All.Date)>datenum('20/12/2017','dd/mm/yyyy') & datenum(All.Date)<datenum('21/02/2018','dd/mm/yyyy'))=3;
BeforeAfterT=table(BeforeAfter);
DaledNonDaled=table(All.Tag~=170831 & All.Tag~=17084 & All.Tag~=170951 & All.Tag~=170991,'VariableNames',{'DaledNonDaled'});
AllReshaped=[All(:,1:3),All(:,7),BeforeAfterT,CropsT,DaledNonDaled];
%% count how much do we have of each one
AllReshaped1=AllReshaped(AllReshaped.Random_Realized==1,:);
PeriodStats=[];
for i=1:3
    Perperiod=AllReshaped1(AllReshaped1.BeforeAfter==i,:);
    [uvals, Ind, uidx] = unique(Perperiod.Tag);
    output = [uvals, accumarray(uidx, 1)];
    NumPoint=[];
    for ii=1:length(uvals)
        NumPoint=[NumPoint; length(unique(floor(datenum(Perperiod.Date(Perperiod.Tag==uvals(ii))))))];
    end
    PeriodStats=[PeriodStats; [output,ones(length(output),1)*i,Perperiod.DaledNonDaled(Ind),NumPoint]];
end
numberOfPoints=[[sum(PeriodStats(PeriodStats(:,3)==1 & PeriodStats(:,4)==0,2)),...
    sum(PeriodStats(PeriodStats(:,3)==2 & PeriodStats(:,4)==0,2)),...
    sum(PeriodStats(PeriodStats(:,3)==3 & PeriodStats(:,4)==0,2))];...
    [sum(PeriodStats(PeriodStats(:,3)==1 & PeriodStats(:,4)==1,2)),...
    sum(PeriodStats(PeriodStats(:,3)==2 & PeriodStats(:,4)==1,2)),...
    sum(PeriodStats(PeriodStats(:,3)==3 & PeriodStats(:,4)==1,2))]];
numberOfPointsT=array2table(numberOfPoints,'VariableNames',{'before','low','intensive'});
numberOfPointsT.TagType=[cellstr('Df-independent'); cellstr('DF-dependent')];
%% Count how many points per indevidual per period (for Appendix S7)
TT=unique(All.Tag);
PerPeriod=[];
for i=1:3
    Perperiod=AllReshaped1(AllReshaped1.BeforeAfter==i,:);
    [uvals, Ind, uidx] = unique(Perperiod.Tag);
    output = [uvals, accumarray(uidx, 1)];
    PerPeriod=[PerPeriod;output,ones(length(output(:,1)),1)*i]; 
end
disp(['mean points per period, per indevidul: ', num2str(round(mean(PerPeriod(:,2)))),...
    ' range: (',num2str(min(PerPeriod(:,2))),' - ',num2str(max(PerPeriod(:,2))),')'])
%% save
save('ForStepSelection_UpdFeb2021_Amp_3_08','All')
writetable(AllReshaped,'ForStepSelection_Amp_3_08.csv')
writetable(AllReshaped(AllReshaped.BeforeAfter==1,:),'ForStepSelection_BeforeFeeding_Amp_3_08.csv')
writetable(AllReshaped(AllReshaped.BeforeAfter==2,:),'ForStepSelection_LowFeeding_Amp_3_08.csv')
writetable(AllReshaped(AllReshaped.BeforeAfter==3,:),'ForStepSelection_IntenseFeeding_Amp_3_08.csv')

% devide for tag types:
NonDaledTagsInd=AllReshaped.Tag==170831 | AllReshaped.Tag==17084 | AllReshaped.Tag==170951 | AllReshaped.Tag==170991;
writetable(AllReshaped(AllReshaped.DaledNonDaled==1,:),'ForStepSelection_RegularTags_Amp_3_08.csv')
writetable(AllReshaped(AllReshaped.DaledNonDaled==0,:),'ForStepSelection_NonDaledTags_Amp_3_08.csv')

%% Plot proportion of realized points (Appendix 7 Fig S1)
titles={'before feeding','low feeding','intensive feeding'};
figure
    for ii=1:3
        AllDFDP=All(All.Random_Realized==1 & BeforeAfter==ii & DaledNonDaled.DaledNonDaled==1,:);
        edgesDFDP = unique(AllDFDP.Crop);
        countsDFDP=[];
        for i=1:length(edgesDFDP)
            countsDFDP(i,1)=sum(AllDFDP.Time_in(AllDFDP.Crop==edgesRan(i)))/sum(AllDFDP.Time_in);
        end
        
        AllDFIND=All(All.Random_Realized==1 & BeforeAfter==ii & DaledNonDaled.DaledNonDaled==0,:);
        edgesDFIND = unique(AllDFIND.Crop);
        countsDFIND=[];
        for i=1:length(edgesDFIND)
            countsDFIND(i,1)=sum(AllDFIND.Time_in(AllDFIND.Crop==edgesRan(i)))/sum(AllDFIND.Time_in);
        end

        subplot(1,3,ii)
        y=[countsDFDP,countsDFIND];
        ySort=y([6,3,2,1,5,4],:);
        bar(edgesDFDP,y,'BarWidth', 1)
        SumWinType1={'F','AN','PRN','LGW','AP','OTH'};
        ax = gca;
        ax.XTickLabel=SumWinType1;
        xtickangle(45)
        legend({'DF-dependent','DF-independent'})
        ax.YLim=[0 1];
        title(titles(ii))
    end
    
function plotsteps(FiledsS,Lontemp,Lattemp,X,Y,date,Tag)
Fileds = struct2table(FiledsS); % convert to table
Fileds=Fileds(Fileds.CropCode<13,:); % we don't need orchards%% load data
%% color
Penut=[239, 195, 112]/255;
Corn=[247, 245, 150]/255;
Tayarut=[224, 241, 207]/255;
grey=[128/255,128/255,128/255]/255;
DarkRed=[155, 6, 31]/255;
C=jet(length(Lontemp)-1);
%% ---Plot fields-----------------------------------------------
for iii=1:height(Fileds) %all fields
    x=Fileds.Lon{iii};
    y=Fileds.Lat{iii};
    x=x(~isnan(x));
    y=y(~isnan(y));
    hold on
    if Fileds.DisturbanceLevel(iii)==0 %Alowed
        s=fill(x,y,'w');
        set(s,'FaceAlpha',0.9,'EdgeAlpha',0.5,'HandleVisibility','off')
    elseif  Fileds.DisturbanceLevel(iii)==1 % Alfalfa
        s=fill(x,y,Corn);
        set(s,'FaceAlpha',0.9,'EdgeAlpha',0.5,'HandleVisibility','off')
    elseif   Fileds.DisturbanceLevel(iii)==2 %not alowed
        s=fill(x,y,Tayarut);
        set(s,'FaceAlpha',0.9,'EdgeAlpha',0.5,'HandleVisibility','off')
    end 
end

%% plot random steps data
for i=2:length(Lontemp)
    Xplot=[X,Lontemp(i)];
    Yplot=[Y,Lattemp(i)];
    plot(Xplot,Yplot,'-o','LineWidth',2,'Color',C(i-1,:),'HandleVisibility','off') %line
end
%% plot realized step data
plot([X,Lontemp(1)],[Y,Lattemp(1)],'-o','LineWidth',2,'Color',grey,'HandleVisibility','off') %line
%% add title
%T=strcat(Date',{': '}, datestr(date));
title(['Tag: ', num2str(Tag),' at ', datestr(date)]);
%title(sprintf(char(T)));
end