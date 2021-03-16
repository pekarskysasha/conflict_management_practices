clear all
load filedsPerday_new
load CraneChaserIntrTempSensitivity
%% --- crate histogram of allevents crane left a sensitive field with DWV distance during event (Appendix S5 Fig S1)
INDInt=find(InterCrCha.NextPointLeft==1 & InterCrCha.InteractWithChaser>0 & InterCrCha.IsAllowed<1);
Left=InterCrCha(INDInt,:);
N=height(Left);
bins=50;

figure
hold on
histogram(Left.Distance,bins)
plot([0 2000],[N/bins N/bins],'-k','LineWidth',2)
plot([300 300],[0 70],'--b','LineWidth',2)
plot([1000 1000],[0 70],'--b','LineWidth',2)
xlabel('Distance to DWV working in the area (m)')
ylabel('Frequency of crane departures from sensitive fields')
ax1 = gca;
ax1.FontSize=22;
ax1.XLim=[0 2000];
%% -- Interaction with DWV -------------------------------------------
% indexes of chasing events using the threshold chosen (300 meter)
INDInt=find(InterCrCha.NextPointLeft==1 & InterCrCha.InteractWithChaser>0 &...
    InterCrCha.IsAllowed<1 & InterCrCha.Distance<301);
indexes=[1:1:height(InterCrCha)]';
% --create an empty matrix
Returned=nan(length(INDInt),9);
for i=1:length(INDInt)
    tag=InterCrCha.Tag(INDInt(i));
    date=floor(datenum(InterCrCha.DateTime(INDInt(i))));
    field=InterCrCha.FieldNim(INDInt(i));
    IND=InterCrCha.Tag==tag & indexes>INDInt(i);
    R=IND & InterCrCha.FieldNim==field;
    %--- did the crane returned to the same field-----------
    if sum(R)==0
        Returned(i,1)=0;
        Returned(i,11)=100;
    else
        ReturnedTemp=(datenum(InterCrCha.DateTime(R))-datenum(InterCrCha.DateTime(INDInt(i))))*86400/60;
        Returned(i,1)=ReturnedTemp(1);
        DaysUntilReturned=floor(datenum(InterCrCha.DateTime(R)))-floor(datenum(InterCrCha.DateTime(INDInt(i))));
        Returned(i,11)=DaysUntilReturned(1);
    end
  %-- what type of field is it------------
    Returned(i,2)=InterCrCha.Crop(INDInt(i));
  %--- what type of field was it-------------
    WasCrop=FiledDay(10,field).CropCode;
    Returned(i,3)=WasCrop;
  %--- where did the crane go after was chased---------
    Datetemp=InterCrCha(IND,:);
    IN=find(Datetemp.TypeOfPoint~=2,1,'first');
    Returned(i,4)=Datetemp.TypeOfPoint(IN);
    Returned(i,5)=Datetemp.Crop(IN);
    DIS=((InterCrCha.LocalLat(INDInt(i))-Datetemp.LocalLat(IN)).^2+(InterCrCha.LocalLon(INDInt(i))-Datetemp.LocalLon(IN)).^2).^0.5;
    Returned(i,6)=DIS;
    Returned(i,7)=field;
    Returned(i,8)=tag;
    Returned(i,9)=datenum(InterCrCha.DateTime(INDInt(i)));
    Returned(i,10)=InterCrCha.TimeStayed(INDInt(i));
end
ReturnedCor=Returned;
%% -- Lack of interaction with DWV -------------------------------------------
% indexes of cranes leaving without chasing using the threshold chosen (1000 meter)
INDLeftAlowed=find(InterCrCha.NextPointLeft==1 & InterCrCha.TooFarChaser==1 & ...
    InterCrCha.Distance >1000 & InterCrCha.IsAllowed<1);
indexes=1:1:height(InterCrCha);
indexes=indexes';
Returned2=nan(length(INDLeftAlowed),9);
for i=1:length(INDLeftAlowed)
    tag=InterCrCha.Tag(INDLeftAlowed(i));
    date=floor(datenum(InterCrCha.DateTime(INDLeftAlowed(i))));
    field=InterCrCha.FieldNim(INDLeftAlowed(i));
    IND=InterCrCha.Tag==tag & indexes>INDLeftAlowed(i);
    R=IND & InterCrCha.FieldNim==field;
    %--- did the crane returned to the same field-----
    if sum(R)==0
        Returned2(i,1)=0;
        Returned2(i,11)=100;
    else 
        ReturnedTemp=(datenum(InterCrCha.DateTime(R))-datenum(InterCrCha.DateTime(INDLeftAlowed(i))))*86400/60;
        Returned2(i,1)=ReturnedTemp(1);
        DaysUntilReturned=floor(datenum(InterCrCha.DateTime(R)))-floor(datenum(InterCrCha.DateTime(INDLeftAlowed(i))));
        Returned2(i,11)=DaysUntilReturned(1);
    end
    
  %--- what type of field is it-----
    Returned2(i,2)=InterCrCha.Crop(INDLeftAlowed(i));
  %--- what type of field was it--------
    WasCrop=FiledDay(10,field).CropCode;
    Returned2(i,3)=WasCrop;
  %---- where did the crane go after was chased------
    Datetemp=InterCrCha(IND,:);
    IN=find(Datetemp.TypeOfPoint~=2,1,'first');
    Returned2(i,4)=Datetemp.TypeOfPoint(IN);
    Returned2(i,5)=Datetemp.Crop(IN);
    DIS=((InterCrCha.LocalLat(INDLeftAlowed(i))-Datetemp.LocalLat(IN)).^2+(InterCrCha.LocalLon(INDLeftAlowed(i))-Datetemp.LocalLon(IN)).^2).^0.5;
    Returned2(i,6)=DIS;
    Returned2(i,7)=field;
    Returned2(i,8)=tag;
    Returned2(i,9)=datenum(InterCrCha.DateTime(INDLeftAlowed(i)));
    Returned2(i,10)=InterCrCha.TimeStayed(INDLeftAlowed(i));
end
Returned2Cor=Returned2;
%% make table for all
ReturnedCor=[ReturnedCor,ones(length(ReturnedCor),1)];
Returned2Cor=[Returned2Cor,zeros(length(Returned2Cor),1)];
ReturnedCorT=array2table([ReturnedCor;Returned2Cor],'VariableNames',{'TimeUntilReturened','cropNow','cropBefore',...
    'TypeOfPoint','cropArrived','Distance','FiledLeft','tag','date','TiemeStayed','daysUntilReturened','WasChased'});
%-- *if never returned, days until returned was turned into 100
ReturnedCorT.date = datetime(ReturnedCorT.date,'ConvertFrom','datenum');
ReturnedCorT.Distance=round(ReturnedCorT.Distance);
%---- DF-independant tags------------------------------------
TypeOfgTag=ReturnedCorT.tag~=170831 & ReturnedCorT.tag~=17084 & ReturnedCorT.tag~=170951 & ReturnedCorT.tag~=170991;
%---- before feeding=0, low feeding=1 and intense feeding=2 -------
Period=ones(height(ReturnedCorT),1);
Period(floor(datenum(ReturnedCorT.date))<datenum('05/12/2017','dd/mm/yyyy'))=0;
Period(floor(datenum(ReturnedCorT.date))>datenum('20/12/2017','dd/mm/yyyy'))=2;
ReturnedCorT.TypeOfgTag=TypeOfgTag;
ReturnedCorT.Period=Period;

save('ReturnedCorT','ReturnedCorT')
writetable(ReturnedCorT,'DWVIneraction_300to1000_aggr.csv')
%% some Info
Chased=ReturnedCorT.WasChased==1;
NotChased=ReturnedCorT.WasChased==0;
% display 
disp(['Never returned: ', num2str(sum(ReturnedCorT.daysUntilReturened(Chased)==100)/sum(Chased)), ' when chased and ',...
    num2str(sum(ReturnedCorT.daysUntilReturened(NotChased)==100)/sum(NotChased)),' when not chased'])
disp(['Returned the first day: ', num2str(sum(ReturnedCorT.daysUntilReturened(Chased)==0)/sum(Chased)), ' when chased and ',...
    num2str(sum(ReturnedCorT.daysUntilReturened(NotChased)==0)/sum(NotChased)),' when not chased'])
disp(['Returned the after 10 days: ', num2str(sum(ReturnedCorT.daysUntilReturened(Chased)<=10)/sum(Chased)), ' when chased and ',...
    num2str(sum(ReturnedCorT.daysUntilReturened(NotChased)<=10)/sum(NotChased)),' when not chased'])
%% Plot per day with field type (Fig 4.b)
%-- colors for figure-----------------------------------
colCh=[227, 102, 30]/255;
colNCh=[98, 152, 239]/255;
Grey=[59, 64, 71]/255;
DarkGrey=[22, 23, 23]/255;
LightGrey=[127, 130, 130]/255;

klChR=sortrows(ReturnedCorT.daysUntilReturened(Chased & ReturnedCorT.daysUntilReturened<100));
klNChR=sortrows(ReturnedCorT.daysUntilReturened(NotChased & ReturnedCorT.daysUntilReturened<100));

TimeVec=[0:1:60]';
ForplotCh=[0,0];
ForplotNCh=[0,0];
for i=1:length(TimeVec)
    ForplotCh=[ForplotCh; [TimeVec(i)+1,sum(klChR<=TimeVec(i))/sum(ReturnedCorT.WasChased==1)]];
    ForplotNCh=[ForplotNCh; [TimeVec(i)+1,sum(klNChR<=TimeVec(i))/sum(ReturnedCorT.WasChased==0)]];
end

figure
hold on
plot(ForplotCh(:,1),ForplotCh(:,2),'-','LineWidth',4,'color',[Grey,0.7])
plot(ForplotNCh(:,1),ForplotNCh(:,2),'--','LineWidth',4,'color',[Grey,0.7])
ylabel('cumulative proportion of returnes','FontSize',16,'FontWeight','bold')
xlabel('days since left','FontSize',16,'FontWeight','bold');
ax1 = gca;
ax1.YTick=[0:0.2:1];
ax1.YLim=[0 1];
ax1.XLim=[0 10];
ax1.FontSize=22;
ax1.FontWeight='bold';
set(gca,'fontname','calibri')
set(gca,'TickDir','out')
%-- legend, if needed
% legend({'Chased','NotChased'})
% legend show
